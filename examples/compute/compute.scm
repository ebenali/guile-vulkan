;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; Headless compute example: squares a buffer of floats on the GPU.
;;; Run from the repository root:
;;;
;;;   guile -L . examples/compute/compute.scm [device-index]
;;;
;;; The shader is square.comp, compiled to square.spv with
;;;   glslangValidator -V examples/compute/square.comp -o examples/compute/square.spv

(use-modules (vulkan)
             (srfi srfi-1)
             (vulkan constants)
             (rnrs bytevectors)
             (ice-9 binary-ports)
             (ice-9 match))

(define element-count 1000)
(define workgroup-size 64)              ; local_size_x in square.comp
(define shader-path "examples/compute/square.spv")

;; Signal an error unless a VkResult is VK_SUCCESS.
(define (check who result)
  (unless (= result vk:success)
    (error who "Vulkan call failed with VkResult" result)))

;; Call F with a fresh 8-byte slot, check the result, return the handle.
(define (create who f)
  (let ((slot (make-bytevector 8 0)))
    (check who (f slot))
    (bytevector-u64-native-ref slot 0)))

;; Vulkan's two-call enumeration: F takes (count-slot array-or-#f).
;; Returns (values count bytevector-of-count-elements).
(define (enumerate element-size f)
  (let ((count (make-bytevector 4 0)))
    (f count #f)
    (let ((items (make-bytevector (* element-size (max 1 (bytevector-u32-native-ref count 0))) 0)))
      (f count items)
      (values (bytevector-u32-native-ref count 0) items))))

;;; ------------------------------------------------------------------
;;; Instance and device.

(define instance
  (let ((info (vk-new vk:instance-create-info
                      (s-type vk:structure-type-instance-create-info)
                      (p-application-info
                       (vk-new vk:application-info
                               (s-type vk:structure-type-application-info)
                               (p-application-name "guile-vulkan compute")
                               (application-version 1)
                               (api-version vk:api-version-1-0))))))
    (create 'vkCreateInstance (lambda (out) (vk:create-instance info #f out)))))

(define physical-devices
  (call-with-values
      (lambda () (enumerate 8 (lambda (count items)
                                (vk:enumerate-physical-devices instance count items))))
    (lambda (n devices)
      (map (lambda (i) (bytevector-u64-native-ref devices (* 8 i))) (iota n)))))

(define (device-name pd)
  (let ((props (make-vk vk:physical-device-properties)))
    (vk:get-physical-device-properties pd props)
    (vk-c-string-ref props (vk-offset vk:physical-device-properties (device-name)))))

;; Index of the first queue family that supports compute, or #f.
(define (compute-queue-family pd)
  (call-with-values
      (lambda () (enumerate (vk-sizeof vk:queue-family-properties)
                            (lambda (count items)
                              (vk:get-physical-device-queue-family-properties pd count items))))
    (lambda (n families)
      (find (lambda (i)
              (logtest (vk-ref vk:queue-family-properties (queue-flags) families i)
                       vk:queue-compute-bit))
            (iota n)))))

(define-values (physical-device queue-family)
  (let ((candidates (match (cdr (command-line))
                      ((index) (list (list-ref physical-devices (string->number index))))
                      (_ physical-devices))))
    (let loop ((ds candidates))
      (cond ((null? ds) (error "no device with a compute queue"))
            ((compute-queue-family (car ds)) => (lambda (q) (values (car ds) q)))
            (else (loop (cdr ds)))))))

(format #t "device: ~a (queue family ~a)~%" (device-name physical-device) queue-family)

(define device
  (let* ((priority (let ((bv (make-bytevector 4))) (bytevector-ieee-single-native-set! bv 0 1.0) bv))
         (info (vk-new vk:device-create-info
                       (s-type vk:structure-type-device-create-info)
                       (queue-create-info-count 1)
                       (p-queue-create-infos
                        (vk-new vk:device-queue-create-info
                                (s-type vk:structure-type-device-queue-create-info)
                                (queue-family-index queue-family)
                                (queue-count 1)
                                (p-queue-priorities priority))))))
    (create 'vkCreateDevice (lambda (out) (vk:create-device physical-device info #f out)))))

(define queue
  (let ((slot (make-bytevector 8 0)))
    (vk:get-device-queue device queue-family 0 slot)
    (bytevector-u64-native-ref slot 0)))

;;; ------------------------------------------------------------------
;;; Storage buffer in host-visible, host-coherent memory.

(define buffer-size (* 4 element-count))

(define buffer
  (let ((info (vk-new vk:buffer-create-info
                      (s-type vk:structure-type-buffer-create-info)
                      (size buffer-size)
                      (usage vk:buffer-usage-storage-buffer-bit)
                      (sharing-mode vk:sharing-mode-exclusive))))
    (create 'vkCreateBuffer (lambda (out) (vk:create-buffer device info #f out)))))

(define (find-memory-type type-bits wanted)
  (let ((props (make-vk vk:physical-device-memory-properties)))
    (vk:get-physical-device-memory-properties physical-device props)
    (or (find (lambda (i)
                (and (logbit? i type-bits)
                     (= wanted (logand wanted (vk-ref vk:physical-device-memory-properties
                                                      (memory-types i property-flags) props)))))
              (iota (vk-ref vk:physical-device-memory-properties (memory-type-count) props)))
        (error "no suitable memory type"))))

(define memory
  (let ((reqs (make-vk vk:memory-requirements)))
    (vk:get-buffer-memory-requirements device buffer reqs)
    (let ((info (vk-new vk:memory-allocate-info
                        (s-type vk:structure-type-memory-allocate-info)
                        (allocation-size (vk-ref vk:memory-requirements (size) reqs))
                        (memory-type-index
                         (find-memory-type (vk-ref vk:memory-requirements (memory-type-bits) reqs)
                                           (logior vk:memory-property-host-visible-bit
                                                   vk:memory-property-host-coherent-bit))))))
      (create 'vkAllocateMemory (lambda (out) (vk:allocate-memory device info #f out))))))

(check 'vkBindBufferMemory (vk:bind-buffer-memory device buffer memory 0))

;; The mapped memory, seen as a bytevector.
(define mapped
  (vk-bytevector (create 'vkMapMemory (lambda (out) (vk:map-memory device memory 0 vk:whole-size 0 out)))
                 buffer-size))

;; Input: 0.0, 1.0, 2.0, ...
(do ((i 0 (+ i 1))) ((= i element-count))
  (bytevector-ieee-single-native-set! mapped (* 4 i) (exact->inexact i)))

;;; ------------------------------------------------------------------
;;; Compute pipeline.

(define shader-module
  (let* ((code (call-with-input-file shader-path get-bytevector-all #:binary #t))
         (info (vk-new vk:shader-module-create-info
                       (s-type vk:structure-type-shader-module-create-info)
                       (code-size (bytevector-length code))
                       (p-code code))))
    (create 'vkCreateShaderModule (lambda (out) (vk:create-shader-module device info #f out)))))

(define set-layout
  (let ((info (vk-new vk:descriptor-set-layout-create-info
                      (s-type vk:structure-type-descriptor-set-layout-create-info)
                      (binding-count 1)
                      (p-bindings (vk-new vk:descriptor-set-layout-binding
                                          (binding 0)
                                          (descriptor-type vk:descriptor-type-storage-buffer)
                                          (descriptor-count 1)
                                          (stage-flags vk:shader-stage-compute-bit))))))
    (create 'vkCreateDescriptorSetLayout
            (lambda (out) (vk:create-descriptor-set-layout device info #f out)))))

;; A bytevector holding one 64-bit handle, for "pointer to handles" fields.
(define (handle-array . handles)
  (let ((bv (make-bytevector (* 8 (length handles)) 0)))
    (let loop ((hs handles) (i 0))
      (unless (null? hs)
        (bytevector-u64-native-set! bv (* 8 i) (car hs))
        (loop (cdr hs) (+ i 1))))
    bv))

(define pipeline-layout
  (let ((info (vk-new vk:pipeline-layout-create-info
                      (s-type vk:structure-type-pipeline-layout-create-info)
                      (set-layout-count 1)
                      (p-set-layouts (handle-array set-layout))
                      (push-constant-range-count 1)
                      (p-push-constant-ranges (vk-new vk:push-constant-range
                                                      (stage-flags vk:shader-stage-compute-bit)
                                                      (offset 0)
                                                      (size 4))))))
    (create 'vkCreatePipelineLayout (lambda (out) (vk:create-pipeline-layout device info #f out)))))

(define pipeline
  (let ((info (vk-new vk:compute-pipeline-create-info
                      (s-type vk:structure-type-compute-pipeline-create-info)
                      (stage s-type vk:structure-type-pipeline-shader-stage-create-info)
                      (stage stage vk:shader-stage-compute-bit)
                      (stage module shader-module)
                      (stage p-name "main")
                      (layout pipeline-layout)
                      (base-pipeline-index -1))))
    (create 'vkCreateComputePipelines
            (lambda (out) (vk:create-compute-pipelines device 0 1 info #f out)))))

;;; ------------------------------------------------------------------
;;; Descriptor set pointing at the buffer.

(define descriptor-pool
  (let ((info (vk-new vk:descriptor-pool-create-info
                      (s-type vk:structure-type-descriptor-pool-create-info)
                      (max-sets 1)
                      (pool-size-count 1)
                      (p-pool-sizes (vk-new vk:descriptor-pool-size
                                            (type vk:descriptor-type-storage-buffer)
                                            (descriptor-count 1))))))
    (create 'vkCreateDescriptorPool (lambda (out) (vk:create-descriptor-pool device info #f out)))))

(define descriptor-set
  (let ((info (vk-new vk:descriptor-set-allocate-info
                      (s-type vk:structure-type-descriptor-set-allocate-info)
                      (descriptor-pool descriptor-pool)
                      (descriptor-set-count 1)
                      (p-set-layouts (handle-array set-layout)))))
    (create 'vkAllocateDescriptorSets (lambda (out) (vk:allocate-descriptor-sets device info out)))))

(vk:update-descriptor-sets
 device 1
 (vk-new vk:write-descriptor-set
         (s-type vk:structure-type-write-descriptor-set)
         (dst-set descriptor-set)
         (dst-binding 0)
         (descriptor-count 1)
         (descriptor-type vk:descriptor-type-storage-buffer)
         (p-buffer-info (vk-new vk:descriptor-buffer-info
                                (buffer buffer)
                                (offset 0)
                                (range vk:whole-size))))
 0 #f)

;;; ------------------------------------------------------------------
;;; Record and submit the dispatch.

(define command-pool
  (let ((info (vk-new vk:command-pool-create-info
                      (s-type vk:structure-type-command-pool-create-info)
                      (queue-family-index queue-family))))
    (create 'vkCreateCommandPool (lambda (out) (vk:create-command-pool device info #f out)))))

(define command-buffer
  (let ((info (vk-new vk:command-buffer-allocate-info
                      (s-type vk:structure-type-command-buffer-allocate-info)
                      (command-pool command-pool)
                      (level vk:command-buffer-level-primary)
                      (command-buffer-count 1))))
    (create 'vkAllocateCommandBuffers (lambda (out) (vk:allocate-command-buffers device info out)))))

(let ((count (make-bytevector 4)))
  (bytevector-u32-native-set! count 0 element-count)
  (check 'vkBeginCommandBuffer
         (vk:begin-command-buffer command-buffer
                                  (vk-new vk:command-buffer-begin-info
                                          (s-type vk:structure-type-command-buffer-begin-info)
                                          (flags vk:command-buffer-usage-one-time-submit-bit))))
  (vk:cmd-bind-pipeline command-buffer vk:pipeline-bind-point-compute pipeline)
  (vk:cmd-bind-descriptor-sets command-buffer vk:pipeline-bind-point-compute
                               pipeline-layout 0 1 (handle-array descriptor-set) 0 #f)
  (vk:cmd-push-constants command-buffer pipeline-layout vk:shader-stage-compute-bit 0 4 count)
  (vk:cmd-dispatch command-buffer (quotient (+ element-count workgroup-size -1) workgroup-size) 1 1)
  (check 'vkEndCommandBuffer (vk:end-command-buffer command-buffer)))

(define fence
  (create 'vkCreateFence
          (lambda (out)
            (vk:create-fence device (vk-new vk:fence-create-info
                                            (s-type vk:structure-type-fence-create-info))
                             #f out))))

(check 'vkQueueSubmit
       (vk:queue-submit queue 1
                        (vk-new vk:submit-info
                                (s-type vk:structure-type-submit-info)
                                (command-buffer-count 1)
                                (p-command-buffers (handle-array command-buffer)))
                        fence))
(check 'vkWaitForFences (vk:wait-for-fences device 1 (handle-array fence) vk:true #xFFFFFFFFFFFFFFFF))

;;; ------------------------------------------------------------------
;;; Check the results.

(define results
  (map (lambda (i) (bytevector-ieee-single-native-ref mapped (* 4 i))) (iota element-count)))

(define mismatches
  (count (lambda (i r) (not (= r (exact->inexact (* i i))))) (iota element-count) results))

(format #t "first values: ~a~%" (list-head results 8))
(format #t "last value:   ~a (expected ~a)~%"
        (list-ref results (- element-count 1))
        (exact->inexact (* (- element-count 1) (- element-count 1))))
(format #t "~a/~a elements correct~%" (- element-count mismatches) element-count)

;;; ------------------------------------------------------------------
;;; Clean up.

(vk:destroy-fence device fence #f)
(vk:destroy-command-pool device command-pool #f)
(vk:destroy-descriptor-pool device descriptor-pool #f)
(vk:destroy-pipeline device pipeline #f)
(vk:destroy-pipeline-layout device pipeline-layout #f)
(vk:destroy-descriptor-set-layout device set-layout #f)
(vk:destroy-shader-module device shader-module #f)
(vk:unmap-memory device memory)
(vk:free-memory device memory #f)
(vk:destroy-buffer device buffer #f)
(vk:destroy-device device #f)
(vk:destroy-instance instance #f)

(exit (if (zero? mismatches) 0 1))
