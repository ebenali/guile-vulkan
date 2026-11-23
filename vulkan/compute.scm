;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; GPU tensors: 1-D and 2-D arrays of 32-bit floats in GPU memory, with the
;;; operations a small neural network needs.
;;;
;;;   (use-modules (vulkan compute) (srfi srfi-4))
;;;   (define a (list->tensor '(1 2 3 4 5 6) '(2 3)))
;;;   (define b (list->tensor '(1 0 0 1 1 1) '(3 2)))
;;;   (tensor->list (tensor-relu (tensor-matmul a b)))   ; => (4.0 5.0 10.0 11.0)
;;;
;;; Operations are recorded into a command buffer, not run one by one;
;;; reading a tensor back (tensor->f32vector, tensor->list) or
;;; compute-flush! submits everything recorded and waits.  Tensors are
;;; freed by the garbage collector (or tensor-free!), once the GPU is done
;;; with them, and their buffers reused for new tensors of the same size
;;; (up to pool-limit-bytes are kept), so a training loop does not
;;; allocate GPU memory every step.  The collector does not see GPU memory, so a collection is
;;; forced to free the unreferenced ones after every gc-threshold-bytes of
;;; tensors, when the live allocations near the device's limit (as low as
;;; 4096 on some drivers), and when an allocation fails for lack of memory
;;; or of allocations.
;;;
;;; The context (device, queue, ...) is created on first use, on the most
;;; capable device; make-compute-context and current-compute-context choose
;;; another.

(define-module (vulkan compute)
  #:use-module (vulkan)
  #:use-module (vulkan constants)
  #:use-module (vulkan compute shaders)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:export (make-compute-context compute-context? compute-context-device-name
            compute-context-allocation-count
            current-compute-context default-compute-context
            compute-flush! compute-context-destroy!
            make-tensor tensor? tensor-shape tensor-size tensor-context
            f32vector->tensor list->tensor tensor->f32vector tensor->list
            tensor-reshape tensor-free!
            tensor-matmul
            tensor-add tensor-sub tensor-mul tensor-div tensor-relu-backward
            tensor-relu tensor-scale tensor-add-scalar tensor-exp tensor-tanh
            tensor-sigmoid tensor-copy
            tensor-softmax-rows tensor-sum-rows))

;;; ------------------------------------------------------------------
;;; Vulkan helpers.

(define (check who result)
  (unless (= result vk:success)
    (error (format #f "~a failed with VkResult ~a" who result))))

;; Call F with an 8-byte slot, check its result, return the handle written.
(define (create who f)
  (let ((slot (make-bytevector 8 0)))
    (check who (f slot))
    (bytevector-u64-native-ref slot 0)))

(define (u64-array . values)
  (let ((bv (make-bytevector (* 8 (max 1 (length values))) 0)))
    (for-each (lambda (v i) (bytevector-u64-native-set! bv (* 8 i) v)) values (iota (length values)))
    bv))

(define (enumerate element-size f)
  (let ((count (make-bytevector 4 0)))
    (f count #f)
    (let ((items (make-bytevector (* element-size (max 1 (bytevector-u32-native-ref count 0))) 0)))
      (f count items)
      (values (bytevector-u32-native-ref count 0) items))))

(define max-u64 #xFFFFFFFFFFFFFFFF)

;; Descriptor sets (of at most 3 buffers) recorded between flushes.  Vulkan
;; 1.0 does not allow allocating from a full pool, so it is flushed first.
(define max-descriptor-sets 1024)

;;; ------------------------------------------------------------------
;;; Contexts.

(define-record-type <compute-context>
  (%make-compute-context instance physical-device device queue device-name
                         memory-properties command-pool command-buffer fence
                         descriptor-pool kernels)
  compute-context?
  (instance context-instance)
  (physical-device context-physical-device)
  (device context-device)
  (queue context-queue)
  (device-name compute-context-device-name)
  (memory-properties context-memory-properties)
  (command-pool context-command-pool)
  (command-buffer context-command-buffer)
  (fence context-fence)
  (descriptor-pool context-descriptor-pool)
  (kernels context-kernels)                         ; name -> <kernel>
  (recording? context-recording? set-context-recording?!)
  (to-release context-to-release set-context-to-release!) ; thunks run after the next flush
  (guardian context-guardian set-context-guardian!)
  (sets-allocated context-sets-allocated set-context-sets-allocated!) ; since the last flush
  (bytes-allocated context-bytes-allocated set-context-bytes-allocated!) ; since the last collection
  (allocation-count compute-context-allocation-count set-context-allocation-count!) ; live vkAllocateMemory blocks
  (max-allocations context-max-allocations set-context-max-allocations!)
  (live-tensors context-live-tensors set-context-live-tensors!)       ; weak: marked freed on destroy
  (buffers context-buffers set-context-buffers!)                    ; every live buffer -> memory
  (pool context-pool set-context-pool!)                             ; bytes -> ((buffer . memory) ...)
  (pool-bytes context-pool-bytes set-context-pool-bytes!)
  (destroyed? context-destroyed? set-context-destroyed?!))

(define (device-type-rank type)                     ; lower is better
  (cond ((= type vk:physical-device-type-discrete-gpu) 0)
        ((= type vk:physical-device-type-integrated-gpu) 1)
        ((= type vk:physical-device-type-virtual-gpu) 2)
        ((= type vk:physical-device-type-cpu) 3)
        (else 4)))

(define (compute-queue-family pd)
  (call-with-values
      (lambda () (enumerate (vk-sizeof vk:queue-family-properties)
                            (lambda (count items)
                              (vk:get-physical-device-queue-family-properties pd count items))))
    (lambda (n families)
      (find (lambda (i) (logtest (vk-ref vk:queue-family-properties (queue-flags) families i)
                                 vk:queue-compute-bit))
            (iota n)))))

(define* (make-compute-context #:key device max-allocations)
  "Create a compute context on the Vulkan device with index DEVICE, or by
default on the most capable device with a compute queue (a discrete GPU
before an integrated one, before a software renderer).  MAX-ALLOCATIONS
lowers the device's limit on live memory allocations (for testing)."
  (let* ((instance
          (create 'vkCreateInstance
                  (lambda (out)
                    (vk:create-instance
                     (vk-new vk:instance-create-info
                             (s-type vk:structure-type-instance-create-info)
                             (p-application-info
                              (vk-new vk:application-info
                                      (s-type vk:structure-type-application-info)
                                      (p-application-name "guile-vulkan compute")
                                      (api-version vk:api-version-1-0))))
                     #f out))))
         (devices (call-with-values
                      (lambda () (enumerate 8 (lambda (count items)
                                                (vk:enumerate-physical-devices instance count items))))
                    (lambda (n bv) (map (lambda (i) (bytevector-u64-native-ref bv (* 8 i))) (iota n)))))
         (properties (lambda (pd)
                       (let ((props (make-vk vk:physical-device-properties)))
                         (vk:get-physical-device-properties pd props)
                         props)))
         (candidates
          (if device
              (begin
                (unless (and (exact-integer? device) (< -1 device (length devices)))
                  (error "no such Vulkan device; there are" (length devices)))
                (list (list-ref devices device)))
              (sort (filter compute-queue-family devices)
                    (lambda (a b)
                      (< (device-type-rank (vk-ref vk:physical-device-properties (device-type) (properties a)))
                         (device-type-rank (vk-ref vk:physical-device-properties (device-type) (properties b))))))))
         (pd (if (null? candidates) (error "no Vulkan device with a compute queue") (car candidates)))
         (family (or (compute-queue-family pd) (error "this device has no compute queue")))
         (priority (let ((bv (make-bytevector 4))) (bytevector-ieee-single-native-set! bv 0 1.0) bv))
         (device
          (create 'vkCreateDevice
                  (lambda (out)
                    (vk:create-device
                     pd
                     (vk-new vk:device-create-info
                             (s-type vk:structure-type-device-create-info)
                             (queue-create-info-count 1)
                             (p-queue-create-infos
                              (vk-new vk:device-queue-create-info
                                      (s-type vk:structure-type-device-queue-create-info)
                                      (queue-family-index family)
                                      (queue-count 1)
                                      (p-queue-priorities priority))))
                     #f out))))
         (queue (let ((slot (make-bytevector 8 0)))
                  (vk:get-device-queue device family 0 slot)
                  (bytevector-u64-native-ref slot 0)))
         (memory-properties (let ((props (make-vk vk:physical-device-memory-properties)))
                              (vk:get-physical-device-memory-properties pd props)
                              props))
         (command-pool
          (create 'vkCreateCommandPool
                  (lambda (out)
                    (vk:create-command-pool
                     device
                     (vk-new vk:command-pool-create-info
                             (s-type vk:structure-type-command-pool-create-info)
                             (flags vk:command-pool-create-reset-command-buffer-bit)
                             (queue-family-index family))
                     #f out))))
         (command-buffer
          (create 'vkAllocateCommandBuffers
                  (lambda (out)
                    (vk:allocate-command-buffers
                     device
                     (vk-new vk:command-buffer-allocate-info
                             (s-type vk:structure-type-command-buffer-allocate-info)
                             (command-pool command-pool)
                             (level vk:command-buffer-level-primary)
                             (command-buffer-count 1))
                     out))))
         (fence (create 'vkCreateFence
                        (lambda (out)
                          (vk:create-fence device (vk-new vk:fence-create-info
                                                          (s-type vk:structure-type-fence-create-info))
                                           #f out))))
         (descriptor-pool
          (create 'vkCreateDescriptorPool
                  (lambda (out)
                    (vk:create-descriptor-pool
                     device
                     (vk-new vk:descriptor-pool-create-info
                             (s-type vk:structure-type-descriptor-pool-create-info)
                             (max-sets max-descriptor-sets)
                             (pool-size-count 1)
                             (p-pool-sizes (vk-new vk:descriptor-pool-size
                                                   (type vk:descriptor-type-storage-buffer)
                                                   (descriptor-count (* 3 max-descriptor-sets)))))
                     #f out))))
         (ctx (%make-compute-context
               instance pd device queue
               (vk-c-string-ref (properties pd) (vk-offset vk:physical-device-properties (device-name)))
               memory-properties command-pool command-buffer fence descriptor-pool
               (make-hash-table))))
    (set-context-recording?! ctx #f)
    (set-context-to-release! ctx '())
    (set-context-guardian! ctx (make-guardian))
    (set-context-sets-allocated! ctx 0)
    (set-context-bytes-allocated! ctx 0)
    (set-context-allocation-count! ctx 0)
    (set-context-max-allocations!
     ctx (let ((limit (vk-ref vk:physical-device-properties (limits max-memory-allocation-count) (properties pd))))
           (if max-allocations (min limit max-allocations) limit)))
    (set-context-live-tensors! ctx (make-weak-key-hash-table))
    (set-context-buffers! ctx (make-hash-table))
    (set-context-pool! ctx (make-hash-table))
    (set-context-pool-bytes! ctx 0)
    (set-context-destroyed?! ctx #f)
    ctx))

(define current-compute-context (make-parameter #f))

(define %default-context #f)
(define (default-compute-context)
  "The context used when none is given: current-compute-context if set,
else one made on first use with make-compute-context's default device."
  (or (current-compute-context)
      %default-context
      (begin (set! %default-context (make-compute-context)) %default-context)))

;;; ------------------------------------------------------------------
;;; Memory.

(define (find-memory-type ctx type-bits wanted)
  (let ((props (context-memory-properties ctx)))
    (or (find (lambda (i)
                (and (logbit? i type-bits)
                     (= wanted (logand wanted (vk-ref vk:physical-device-memory-properties
                                                      (memory-types i property-flags) props)))))
              (iota (vk-ref vk:physical-device-memory-properties (memory-type-count) props)))
        (error "no suitable Vulkan memory type"))))

;; => (values buffer memory)
(define (make-buffer ctx size usage properties)
  (let* ((device (context-device ctx))
         (buffer (create 'vkCreateBuffer
                         (lambda (out)
                           (vk:create-buffer device
                                             (vk-new vk:buffer-create-info
                                                     (s-type vk:structure-type-buffer-create-info)
                                                     (size size)
                                                     (usage usage)
                                                     (sharing-mode vk:sharing-mode-exclusive))
                                             #f out))))
         (reqs (make-vk vk:memory-requirements)))
    (vk:get-buffer-memory-requirements device buffer reqs)
    (let* ((slot (make-bytevector 8 0))
           (r (vk:allocate-memory
               device
               (vk-new vk:memory-allocate-info
                       (s-type vk:structure-type-memory-allocate-info)
                       (allocation-size (vk-ref vk:memory-requirements (size) reqs))
                       (memory-type-index
                        (find-memory-type ctx (vk-ref vk:memory-requirements (memory-type-bits) reqs)
                                          properties)))
               #f slot)))
      (cond
       ((= r vk:success)
        (let ((memory (bytevector-u64-native-ref slot 0)))
          (set-context-allocation-count! ctx (+ 1 (compute-context-allocation-count ctx)))
          (hashv-set! (context-buffers ctx) buffer memory)
          (check 'vkBindBufferMemory (vk:bind-buffer-memory device buffer memory 0))
          (values buffer memory)))
       ;; Out of memory, or of allocations: let the caller collect and retry.
       ((memv r (list vk:error-out-of-device-memory vk:error-out-of-host-memory
                      vk:error-too-many-objects))
        (vk:destroy-buffer device buffer #f)
        (values #f #f))
       (else (check 'vkAllocateMemory r))))))

(define (free-buffer ctx buffer memory)
  (vk:destroy-buffer (context-device ctx) buffer #f)
  (vk:free-memory (context-device ctx) memory #f)
  (hashv-remove! (context-buffers ctx) buffer)
  (set-context-allocation-count! ctx (- (compute-context-allocation-count ctx) 1)))

;; A host-visible buffer of SIZE bytes => (values buffer memory mapped-bytevector).
(define (make-staging-buffer ctx size usage)
  (call-with-values
      (lambda () (make-buffer ctx size usage (logior vk:memory-property-host-visible-bit
                                                     vk:memory-property-host-coherent-bit)))
    (lambda (buffer memory)
      (unless buffer (error "out of host-visible Vulkan memory for" size 'bytes))
      (let ((address (create 'vkMapMemory
                             (lambda (out) (vk:map-memory (context-device ctx) memory 0 vk:whole-size 0 out)))))
        (values buffer memory (vk-bytevector address size))))))

;;; ------------------------------------------------------------------
;;; Recording and flushing.

(define (command-buffer! ctx)
  "The context's command buffer, begun if it was not."
  (when (context-destroyed? ctx) (error "compute context was destroyed"))
  (unless (context-recording? ctx)
    (check 'vkBeginCommandBuffer
           (vk:begin-command-buffer (context-command-buffer ctx)
                                    (vk-new vk:command-buffer-begin-info
                                            (s-type vk:structure-type-command-buffer-begin-info)
                                            (flags vk:command-buffer-usage-one-time-submit-bit))))
    (set-context-recording?! ctx #t))
  (context-command-buffer ctx))

;; Makes the writes of every command recorded so far visible to the
;; commands after it (and, at the end, to the host).
(define* (barrier! ctx #:key (to-host? #f))
  (vk:cmd-pipeline-barrier
   (command-buffer! ctx)
   (logior vk:pipeline-stage-compute-shader-bit vk:pipeline-stage-transfer-bit)
   (if to-host?
       vk:pipeline-stage-host-bit
       (logior vk:pipeline-stage-compute-shader-bit vk:pipeline-stage-transfer-bit))
   0
   1 (vk-new vk:memory-barrier
             (s-type vk:structure-type-memory-barrier)
             (src-access-mask (logior vk:access-shader-write-bit vk:access-transfer-write-bit))
             (dst-access-mask (if to-host?
                                  vk:access-host-read-bit
                                  (logior vk:access-shader-read-bit vk:access-shader-write-bit
                                          vk:access-transfer-read-bit vk:access-transfer-write-bit))))
   0 #f 0 #f))

(define (release-later! ctx thunk)
  (set-context-to-release! ctx (cons thunk (context-to-release ctx))))

(define* (compute-flush! #:optional (ctx (default-compute-context)))
  "Submit everything recorded on CTX and wait for it; then free what
waited for the GPU to finish (staging buffers, collected tensors)."
  (when (context-recording? ctx)
    (barrier! ctx #:to-host? #t)
    (let ((cb (context-command-buffer ctx))
          (device (context-device ctx))
          (fence (context-fence ctx)))
      (check 'vkEndCommandBuffer (vk:end-command-buffer cb))
      (check 'vkQueueSubmit
             (vk:queue-submit (context-queue ctx) 1
                              (vk-new vk:submit-info
                                      (s-type vk:structure-type-submit-info)
                                      (command-buffer-count 1)
                                      (p-command-buffers (u64-array cb)))
                              fence))
      (check 'vkWaitForFences (vk:wait-for-fences device 1 (u64-array fence) vk:true max-u64))
      (check 'vkResetFences (vk:reset-fences device 1 (u64-array fence)))
      (check 'vkResetCommandBuffer (vk:reset-command-buffer cb 0))
      (check 'vkResetDescriptorPool (vk:reset-descriptor-pool device (context-descriptor-pool ctx) 0))
      (set-context-sets-allocated! ctx 0)
      (set-context-recording?! ctx #f)))
  ;; Nothing is in flight now.
  (let ((thunks (context-to-release ctx)))
    (set-context-to-release! ctx '())
    (for-each (lambda (thunk) (thunk)) thunks))
  (let loop ()
    (let ((t ((context-guardian ctx))))
      (when t
        (release-tensor! t)
        (loop)))))

(define* (compute-context-destroy! #:optional (ctx (default-compute-context)))
  "Wait for CTX's work and destroy it.  Its tensors may not be used after."
  (unless (context-destroyed? ctx)
    (compute-flush! ctx)
    (let ((device (context-device ctx)))
      (check 'vkDeviceWaitIdle (vk:device-wait-idle device))
      ;; Every buffer still allocated, whether its tensor is referenced, was
      ;; collected or waits in the guardian; tensors still referenced are
      ;; marked freed, so using them is an error.
      (hash-for-each (lambda (t _) (forget-tensor-memory! t)) (context-live-tensors ctx))
      (for-each (match-lambda ((buffer . memory) (free-buffer ctx buffer memory)))
                (hash-map->list cons (context-buffers ctx)))
      (hash-clear! (context-pool ctx))
      (set-context-pool-bytes! ctx 0)
      (hash-for-each (lambda (name k) (destroy-kernel ctx k)) (context-kernels ctx))
      (vk:destroy-descriptor-pool device (context-descriptor-pool ctx) #f)
      (vk:destroy-fence device (context-fence ctx) #f)
      (vk:destroy-command-pool device (context-command-pool ctx) #f)
      (vk:destroy-device device #f)
      (vk:destroy-instance (context-instance ctx) #f))
    (set-context-destroyed?! ctx #t)
    (when (eq? ctx %default-context) (set! %default-context #f))))

;;; ------------------------------------------------------------------
;;; Kernels: one compute pipeline per shader.

(define-record-type <kernel>
  (make-kernel pipeline layout set-layout bindings)
  kernel?
  (pipeline kernel-pipeline)
  (layout kernel-layout)
  (set-layout kernel-set-layout)
  (bindings kernel-bindings))

(define push-constant-size 32)

(define (kernel ctx name spirv bindings)
  (or (hash-ref (context-kernels ctx) name)
      (let* ((device (context-device ctx))
             (module (create 'vkCreateShaderModule
                             (lambda (out)
                               (vk:create-shader-module
                                device
                                (vk-new vk:shader-module-create-info
                                        (s-type vk:structure-type-shader-module-create-info)
                                        (code-size (bytevector-length spirv))
                                        (p-code spirv))
                                #f out))))
             (binding-info (make-vk vk:descriptor-set-layout-binding bindings))
             (_ (for-each (lambda (i)
                            (vk-set! vk:descriptor-set-layout-binding (binding) binding-info i i)
                            (vk-set! vk:descriptor-set-layout-binding (descriptor-type) binding-info i
                                     vk:descriptor-type-storage-buffer)
                            (vk-set! vk:descriptor-set-layout-binding (descriptor-count) binding-info i 1)
                            (vk-set! vk:descriptor-set-layout-binding (stage-flags) binding-info i
                                     vk:shader-stage-compute-bit))
                          (iota bindings)))
             (set-layout (create 'vkCreateDescriptorSetLayout
                                 (lambda (out)
                                   (vk:create-descriptor-set-layout
                                    device
                                    (vk-new vk:descriptor-set-layout-create-info
                                            (s-type vk:structure-type-descriptor-set-layout-create-info)
                                            (binding-count bindings)
                                            (p-bindings binding-info))
                                    #f out))))
             (layout (create 'vkCreatePipelineLayout
                             (lambda (out)
                               (vk:create-pipeline-layout
                                device
                                (vk-new vk:pipeline-layout-create-info
                                        (s-type vk:structure-type-pipeline-layout-create-info)
                                        (set-layout-count 1)
                                        (p-set-layouts (u64-array set-layout))
                                        (push-constant-range-count 1)
                                        (p-push-constant-ranges
                                         (vk-new vk:push-constant-range
                                                 (stage-flags vk:shader-stage-compute-bit)
                                                 (offset 0)
                                                 (size push-constant-size))))
                                #f out))))
             (pipeline (create 'vkCreateComputePipelines
                               (lambda (out)
                                 (vk:create-compute-pipelines
                                  device 0 1
                                  (vk-new vk:compute-pipeline-create-info
                                          (s-type vk:structure-type-compute-pipeline-create-info)
                                          (stage s-type vk:structure-type-pipeline-shader-stage-create-info)
                                          (stage stage vk:shader-stage-compute-bit)
                                          (stage module module)
                                          (stage p-name "main")
                                          (layout layout)
                                          (base-pipeline-index -1))
                                  #f out))))
             (k (make-kernel pipeline layout set-layout bindings)))
        (vk:destroy-shader-module device module #f)
        (hash-set! (context-kernels ctx) name k)
        k)))

(define (destroy-kernel ctx k)
  (let ((device (context-device ctx)))
    (vk:destroy-pipeline device (kernel-pipeline k) #f)
    (vk:destroy-pipeline-layout device (kernel-layout k) #f)
    (vk:destroy-descriptor-set-layout device (kernel-set-layout k) #f)))

(define (allocate-descriptor-set ctx k)
  (when (>= (context-sets-allocated ctx) max-descriptor-sets)
    (compute-flush! ctx))                 ; runs what is recorded and resets the pool
  (set-context-sets-allocated! ctx (+ 1 (context-sets-allocated ctx)))
  (let* ((slot (make-bytevector 8 0))
         (info (vk-new vk:descriptor-set-allocate-info
                       (s-type vk:structure-type-descriptor-set-allocate-info)
                       (descriptor-pool (context-descriptor-pool ctx))
                       (descriptor-set-count 1)
                       (p-set-layouts (u64-array (kernel-set-layout k)))))
         (r (vk:allocate-descriptor-sets (context-device ctx) info slot)))
    (check 'vkAllocateDescriptorSets r)
    (bytevector-u64-native-ref slot 0)))

;; Record a dispatch of kernel K over TENSORS (bound in order) with PUSH (a
;; bytevector of at most 32 bytes), followed by a barrier.  Allocating the
;; descriptor set may flush, and a flush frees collected tensors; so the
;; buffers are looked up only after it, through TENSORS, which are
;; referenced until then.
(define (dispatch! ctx k tensors push groups-x groups-y)
  (let* ((set (allocate-descriptor-set ctx k))
         (buffers (map live-buffer tensors))
         (n (length buffers))
         (writes (make-vk vk:write-descriptor-set n)))
    (for-each (lambda (buffer i)
                (vk-set! vk:write-descriptor-set (s-type) writes i vk:structure-type-write-descriptor-set)
                (vk-set! vk:write-descriptor-set (dst-set) writes i set)
                (vk-set! vk:write-descriptor-set (dst-binding) writes i i)
                (vk-set! vk:write-descriptor-set (descriptor-count) writes i 1)
                (vk-set! vk:write-descriptor-set (descriptor-type) writes i vk:descriptor-type-storage-buffer)
                (vk-set! vk:write-descriptor-set (p-buffer-info) writes i
                         (vk-new vk:descriptor-buffer-info (buffer buffer) (offset 0) (range vk:whole-size))))
              buffers (iota n))
    (vk:update-descriptor-sets (context-device ctx) n writes 0 #f)
    (let ((cb (command-buffer! ctx))
          (push-bv (let ((bv (make-bytevector push-constant-size 0)))
                     (bytevector-copy! push 0 bv 0 (bytevector-length push))
                     bv)))
      (vk:cmd-bind-pipeline cb vk:pipeline-bind-point-compute (kernel-pipeline k))
      (vk:cmd-bind-descriptor-sets cb vk:pipeline-bind-point-compute (kernel-layout k) 0 1 (u64-array set) 0 #f)
      (vk:cmd-push-constants cb (kernel-layout k) vk:shader-stage-compute-bit 0 push-constant-size push-bv)
      (vk:cmd-dispatch cb groups-x groups-y 1)
      (barrier! ctx))))

;; Push constants: a list of (u32 n) / (f32 x) values, packed in order.
(define (push-constants . items)
  (let ((bv (make-bytevector (* 4 (length items)) 0)))
    (for-each (lambda (item i)
                (match item
                  (('u32 v) (bytevector-u32-native-set! bv (* 4 i) v))
                  (('f32 v) (bytevector-ieee-single-native-set! bv (* 4 i) (exact->inexact v)))))
              items (iota (length items)))
    bv))

(define (ceiling-quotient a b) (quotient (+ a b -1) b))

;;; ------------------------------------------------------------------
;;; Tensors.

;; A view (tensor-reshape) shares its base's buffer and keeps it alive;
;; only a base owns GPU memory.
(define-record-type <tensor>
  (%make-tensor context shape size buffer memory base)
  tensor?
  (context tensor-context)
  (shape tensor-shape)
  (size tensor-size)                                ; elements
  (buffer tensor-buffer set-tensor-buffer!)
  (memory tensor-memory set-tensor-memory!)
  (base tensor-base))

(define (check-shape shape)
  (match shape
    (((? exact-integer? n)) (when (< n 1) (error "bad tensor shape" shape)))
    (((? exact-integer? r) (? exact-integer? c)) (when (or (< r 1) (< c 1)) (error "bad tensor shape" shape)))
    (_ (error "a tensor shape is (n) or (rows cols):" shape))))

(define (shape-size shape) (apply * shape))
(define (tensor-rows t) (match (tensor-shape t) ((n) 1) ((r c) r)))
(define (tensor-cols t) (match (tensor-shape t) ((n) n) ((r c) c)))

(define (live-buffer t)
  (or (tensor-buffer (or (tensor-base t) t)) (error "tensor was freed" t)))

;; (Called from compute-context-destroy!, above the record type.)
(define (forget-tensor-memory! t)
  (set-tensor-buffer! t #f)
  (set-tensor-memory! t #f))

;; Called once the GPU is done with T: keeps its buffer for reuse if the
;; pool has room, else frees it.
(define (release-tensor! t)
  (when (tensor-buffer t)
    (let* ((ctx (tensor-context t))
           (bytes (* 4 (tensor-size t))))
      (if (and (not (context-destroyed? ctx))
               (<= (+ bytes (context-pool-bytes ctx)) pool-limit-bytes))
          (begin
            (hashv-set! (context-pool ctx) bytes
                        (cons (cons (tensor-buffer t) (tensor-memory t))
                              (hashv-ref (context-pool ctx) bytes '())))
            (set-context-pool-bytes! ctx (+ bytes (context-pool-bytes ctx))))
          (free-buffer ctx (tensor-buffer t) (tensor-memory t))))
    (set-tensor-buffer! t #f)
    (set-tensor-memory! t #f)))

;; Freed tensors' buffers kept for reuse, at most this many bytes.
(define pool-limit-bytes (* 256 1024 1024))

;; A pooled buffer of BYTES => (buffer . memory), or #f.
(define (pool-take! ctx bytes)
  (match (hashv-ref (context-pool ctx) bytes '())
    (() #f)
    ((entry . rest)
     (hashv-set! (context-pool ctx) bytes rest)
     (set-context-pool-bytes! ctx (- (context-pool-bytes ctx) bytes))
     entry)))

(define (drain-pool! ctx)
  (hash-for-each (lambda (bytes entries)
                   (for-each (match-lambda ((buffer . memory) (free-buffer ctx buffer memory))) entries))
                 (context-pool ctx))
  (hash-clear! (context-pool ctx))
  (set-context-pool-bytes! ctx 0))

;; The collector cannot see GPU memory: after this many bytes of tensors,
;; a collection is forced (and its garbage freed by a flush).
(define gc-threshold-bytes (* 256 1024 1024))

(define (collect-garbage! ctx)
  (set-context-bytes-allocated! ctx 0)
  (gc)
  (compute-flush! ctx)                  ; releases the tensors the collector found
  (drain-pool! ctx))                    ; and really frees their memory

;; An uninitialized tensor of SHAPE on CTX.
(define (allocate-tensor ctx shape)
  (check-shape shape)
  (let* ((size (shape-size shape))
         (bytes (* 4 size))
         (allocate (lambda ()
                     (make-buffer ctx bytes
                                  (logior vk:buffer-usage-storage-buffer-bit
                                          vk:buffer-usage-transfer-src-bit
                                          vk:buffer-usage-transfer-dst-bit)
                                  vk:memory-property-device-local-bit))))
    (unless (pair? (hashv-ref (context-pool ctx) bytes '()))   ; reuse counts as nothing new
      (set-context-bytes-allocated! ctx (+ bytes (context-bytes-allocated ctx))))
    (when (or (> (context-bytes-allocated ctx) gc-threshold-bytes)
              ;; Leave a quarter of the allocations for staging buffers and slack.
              (>= (* 4 (compute-context-allocation-count ctx)) (* 3 (context-max-allocations ctx))))
      (collect-garbage! ctx))
    (call-with-values
        (lambda ()
          (match (pool-take! ctx bytes)
            ((buffer . memory) (values buffer memory))  ; reused: no new allocation
            (#f
             (call-with-values allocate
               (lambda (buffer memory)
                 (if buffer
                     (values buffer memory)
                     (begin (collect-garbage! ctx)      ; out of memory: free garbage, retry once
                            (allocate))))))))
      (lambda (buffer memory)
        (unless buffer
          (error "out of GPU memory for a tensor of" shape))
        (let ((t (%make-tensor ctx shape size buffer memory #f)))
          ((context-guardian ctx) t)
          (hash-set! (context-live-tensors ctx) t #t)
          t)))))

(define* (make-tensor shape #:key (context (default-compute-context)) (fill 0.0))
  "A tensor of SHAPE ((n) or (rows cols)) with every element FILL."
  (let ((t (allocate-tensor context shape))
        (pattern (let ((bv (make-bytevector 4))) (bytevector-ieee-single-native-set! bv 0 (exact->inexact fill)) bv)))
    (vk:cmd-fill-buffer (command-buffer! context) (live-buffer t) 0 vk:whole-size
                        (bytevector-u32-native-ref pattern 0))
    (barrier! context)
    t))

(define* (f32vector->tensor v #:optional shape #:key (context (default-compute-context)))
  "A tensor holding the 32-bit floats of V (an f32vector or bytevector),
with SHAPE, by default (length)."
  (let* ((n (quotient (bytevector-length v) 4))
         (shape (or shape (list n))))
    (unless (= n (shape-size shape))
      (error "data does not fit the shape" n shape))
    (let ((t (allocate-tensor context shape)))
      (call-with-values
          (lambda () (make-staging-buffer context (* 4 n) vk:buffer-usage-transfer-src-bit))
        (lambda (staging memory mapped)
          (bytevector-copy! v 0 mapped 0 (* 4 n))
          (vk:cmd-copy-buffer (command-buffer! context) staging (live-buffer t) 1
                              (vk-new vk:buffer-copy (size (* 4 n))))
          (barrier! context)
          (release-later! context (lambda () (free-buffer context staging memory)))))
      t)))

(define* (list->tensor numbers #:optional shape #:key (context (default-compute-context)))
  (f32vector->tensor (list->f32vector (map exact->inexact numbers)) shape #:context context))

(define (tensor->f32vector t)
  "The elements of T, as a new f32vector (after running pending work)."
  (let* ((ctx (tensor-context t))
         (bytes (* 4 (tensor-size t))))
    (call-with-values
        (lambda () (make-staging-buffer ctx bytes vk:buffer-usage-transfer-dst-bit))
      (lambda (staging memory mapped)
        (vk:cmd-copy-buffer (command-buffer! ctx) (live-buffer t) staging 1
                            (vk-new vk:buffer-copy (size bytes)))
        (compute-flush! ctx)
        (let ((out (make-f32vector (tensor-size t))))
          (bytevector-copy! mapped 0 out 0 bytes)
          (free-buffer ctx staging memory)
          out)))))

(define (tensor->list t)
  (f32vector->list (tensor->f32vector t)))

(define (tensor-reshape t shape)
  "A tensor with SHAPE sharing T's elements."
  (check-shape shape)
  (unless (= (shape-size shape) (tensor-size t))
    (error "reshape changes the number of elements" (tensor-shape t) shape))
  (%make-tensor (tensor-context t) shape (tensor-size t) #f #f (or (tensor-base t) t)))

(define (tensor-free! t)
  "Free T's GPU memory once pending work is done (also done by the GC)."
  (unless (tensor-base t)
    (when (tensor-buffer t)
      (let ((ctx (tensor-context t)))
        (release-later! ctx (lambda () (release-tensor! t)))))))

;;; ------------------------------------------------------------------
;;; Operations.

(define max-groups 65535)
(define (element-groups n) (min max-groups (ceiling-quotient n 256)))

(define* (tensor-matmul a b #:key transpose-a? transpose-b? (alpha 1.0))
  "alpha * op(A) * op(B), where op transposes when asked.  op(A) is M x K
and op(B) K x N; the result is M x N."
  (let*-values (((ctx) (tensor-context a))
                ((m k) (if transpose-a? (values (tensor-cols a) (tensor-rows a)) (values (tensor-rows a) (tensor-cols a))))
                ((k2 n) (if transpose-b? (values (tensor-cols b) (tensor-rows b)) (values (tensor-rows b) (tensor-cols b)))))
    (unless (= k k2)
      (error "tensor-matmul: inner dimensions differ" (tensor-shape a) (tensor-shape b)))
    (let ((c (allocate-tensor ctx (list m n))))
      (dispatch! ctx (kernel ctx 'matmul matmul-spirv 3)
                 (list a b c)
                 (push-constants `(u32 ,m) `(u32 ,n) `(u32 ,k)
                                 `(u32 ,(if transpose-a? 1 0)) `(u32 ,(if transpose-b? 1 0)) `(f32 ,alpha))
                 (ceiling-quotient n 16) (ceiling-quotient m 16))
      c)))

;; B has A's size, or is (cols) of A, added to every row.
(define (binary op a b)
  (let* ((ctx (tensor-context a))
         (n (tensor-size a))
         (b-cols (cond ((= (tensor-size b) n) 0)
                       ((and (= (length (tensor-shape b)) 1) (= (tensor-size b) (tensor-cols a))) (tensor-size b))
                       (else (error "shapes do not match" (tensor-shape a) (tensor-shape b)))))
         (out (allocate-tensor ctx (tensor-shape a))))
    (dispatch! ctx (kernel ctx 'binary binary-spirv 3)
               (list a b out)
               (push-constants `(u32 ,n) `(u32 ,op) `(u32 ,b-cols))
               (element-groups n) 1)
    out))

(define (tensor-add a b) "A + B (B may be a row added to each row of A)." (binary 0 a b))
(define (tensor-sub a b) "A - B (B may be a row)." (binary 1 a b))
(define (tensor-mul a b) "A * B elementwise (B may be a row)." (binary 2 a b))
(define (tensor-div a b) "A / B elementwise (B may be a row)." (binary 3 a b))
(define (tensor-relu-backward x gradient)
  "GRADIENT where X > 0, else 0: the gradient through relu at X."
  (binary 4 x gradient))

(define* (unary op a #:optional (s 0.0))
  (let* ((ctx (tensor-context a))
         (n (tensor-size a))
         (out (allocate-tensor ctx (tensor-shape a))))
    (dispatch! ctx (kernel ctx 'unary unary-spirv 2)
               (list a out)
               (push-constants `(u32 ,n) `(u32 ,op) `(f32 ,s))
               (element-groups n) 1)
    out))

(define (tensor-relu a) (unary 0 a))
(define (tensor-scale a s) "A * S for a number S." (unary 1 a s))
(define (tensor-add-scalar a s) "A + S for a number S." (unary 2 a s))
(define (tensor-exp a) (unary 3 a))
(define (tensor-tanh a) (unary 4 a))
(define (tensor-sigmoid a) (unary 5 a))
(define (tensor-copy a) (unary 6 a))

(define (tensor-softmax-rows a)
  "Softmax of each row of A."
  (let* ((ctx (tensor-context a))
         (rows (tensor-rows a))
         (out (allocate-tensor ctx (tensor-shape a))))
    (dispatch! ctx (kernel ctx 'softmax softmax-spirv 2)
               (list a out)
               (push-constants `(u32 ,rows) `(u32 ,(tensor-cols a)))
               (min max-groups rows) 1)
    out))

(define (tensor-sum-rows a)
  "The sum of A's rows: a (cols) tensor."
  (let* ((ctx (tensor-context a))
         (cols (tensor-cols a))
         (out (allocate-tensor ctx (list cols))))
    (dispatch! ctx (kernel ctx 'sum-rows sum-rows-spirv 2)
               (list a out)
               (push-constants `(u32 ,(tensor-rows a)) `(u32 ,cols))
               (element-groups cols) 1)
    out))
