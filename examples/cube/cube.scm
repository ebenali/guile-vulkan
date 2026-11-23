;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; Cube: a spinning, lit cube with a λ texture, in a native Wayland window.
;;; Needs only Guile, a Vulkan 1.3 driver and libwayland-client.  Run from
;;; the repository root:
;;;
;;;   guile -L . examples/cube/cube.scm [--device N] [--novsync] [--frames N] [--step DEGREES]
;;;
;;; Keys: arrows rotate the cube, space pauses the spin, Esc quits.
;;; --step DEGREES turns the cube by a fixed angle per frame (for captures).
;;; Shaders: glslangValidator -V examples/cube/cube.vert -o examples/cube/cube.vert.spv
;;;          glslangValidator -V examples/cube/cube.frag -o examples/cube/cube.frag.spv

(use-modules (vulkan) (vulkan constants) (vulkan wayland)
             (examples common) (examples math) (examples wayland)
             (rnrs bytevectors) (srfi srfi-1) (srfi srfi-4) (ice-9 match) (ice-9 format)
             (ice-9 binary-ports) (ice-9 rdelim))

;;; ------------------------------------------------------------------
;;; Options.

(define (option name convert)
  (let loop ((args (cdr (command-line))))
    (match args
      (() #f)
      (((? (lambda (a) (string=? a name))) value . _) (if convert (convert value) #t))
      (((? (lambda (a) (string=? a name)))) (if convert (error "missing value for" name) #t))
      ((_ . rest) (loop rest)))))

(define device-choice (option "--device" string->number))
(define no-vsync? (option "--novsync" #f))
(define frame-limit (option "--frames" string->number))
(define spin-step (option "--step" string->number))
(define texture-path "examples/cube/lambda.ppm")
(define frames-in-flight 2)

;;; ------------------------------------------------------------------
;;; The texture: a binary PPM (P6), read into RGBA.

(define (read-ppm path)
  "=> (values width height rgba-bytevector)"
  (call-with-input-file path
    (lambda (port)
      (define (token)                       ; skips whitespace and # comments
        (let loop ((chars '()))
          (let ((c (peek-char port)))
            (cond ((eof-object? c) (list->string (reverse chars)))
                  ((and (null? chars) (char=? c #\#)) (read-line port) (loop chars))
                  ((char-whitespace? c)
                   (read-char port)
                   (if (null? chars) (loop chars) (list->string (reverse chars))))
                  (else (read-char port) (loop (cons c chars)))))))
      (unless (string=? (token) "P6") (error "not a binary PPM:" path))
      (let* ((width (string->number (token)))
             (height (string->number (token)))
             (maxval (string->number (token))))
        (unless (= maxval 255) (error "only 8-bit PPMs are supported:" path))
        (let* ((rgb (get-bytevector-n port (* 3 width height)))
               (rgba (make-bytevector (* 4 width height) 255)))
          (do ((i 0 (+ i 1))) ((= i (* width height)))
            (bytevector-copy! rgb (* 3 i) rgba (* 4 i) 3))
          (values width height rgba))))
    #:binary #t))

;;; ------------------------------------------------------------------
;;; Cube geometry: 6 faces x 2 triangles of (x y z nx ny nz u v), each face
;;; counter-clockwise seen from outside.

(define (cross a b)
  (match (list a b)
    (((ax ay az) (bx by bz)) (list (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))))))

(define cube-vertices
  (append-map
   (match-lambda
     ((n up)
      (let* ((right (cross up n))             ; right x up = n
             (corner (lambda (du dv uv)
                       (append (map (lambda (c u r) (+ c (* dv u) (* du r))) n up right) n uv))))
        (list (corner -1 -1 '(0 1)) (corner 1 -1 '(1 1)) (corner 1 1 '(1 0))
              (corner -1 -1 '(0 1)) (corner 1 1 '(1 0)) (corner -1 1 '(0 0))))))
   '(((1 0 0) (0 1 0)) ((-1 0 0) (0 1 0))
     ((0 0 1) (0 1 0)) ((0 0 -1) (0 1 0))
     ((0 1 0) (0 0 -1)) ((0 -1 0) (0 0 1)))))

;;; ------------------------------------------------------------------
;;; Window, instance, surface.

(define window (wayland-open-window "guile cube" 640 480 #:app-id "guile-vulkan.cube"))

(define instance
  (create 'vkCreateInstance
          (lambda (out)
            (vk:create-instance
             (vk-new vk:instance-create-info
                     (s-type vk:structure-type-instance-create-info)
                     (p-application-info (vk-new vk:application-info
                                                 (s-type vk:structure-type-application-info)
                                                 (p-application-name "guile cube")
                                                 (application-version 1)
                                                 (api-version vk:api-version-1-3)))
                     (enabled-extension-count 2)
                     (pp-enabled-extension-names (vk-c-string-array '("VK_KHR_surface" "VK_KHR_wayland_surface"))))
             #f out))))

(define surface
  (create 'vkCreateWaylandSurfaceKHR
          (lambda (out)
            (vk:create-wayland-surface-khr
             instance
             (vk-new vk:wayland-surface-create-info-khr
                     (s-type vk:structure-type-wayland-surface-create-info-khr)
                     (display (wayland-window-display window))
                     (surface (wayland-window-surface window)))
             #f out))))

;;; ------------------------------------------------------------------
;;; Physical device: Vulkan 1.3, VK_KHR_swapchain, graphics + present queue.

(define (device-properties pd)
  (let ((props (make-vk vk:physical-device-properties)))
    (vk:get-physical-device-properties pd props)
    props))

(define (device-name pd)
  (vk-c-string-ref (device-properties pd) (vk-offset vk:physical-device-properties (device-name))))

(define (has-device-extension? pd name)
  (call-with-values
      (lambda () (enumerate (vk-sizeof vk:extension-properties)
                            (lambda (count items) (vk:enumerate-device-extension-properties pd #f count items))))
    (lambda (n exts)
      (any (lambda (i)
             (string=? name (vk-c-string-ref exts (+ (* i (vk-sizeof vk:extension-properties))
                                                     (vk-offset vk:extension-properties (extension-name))))))
           (iota n)))))

(define (graphics-present-family pd)
  (call-with-values
      (lambda () (enumerate (vk-sizeof vk:queue-family-properties)
                            (lambda (count items) (vk:get-physical-device-queue-family-properties pd count items))))
    (lambda (n families)
      (let ((supported (make-bytevector 4 0)))
        (find (lambda (i)
                (and (logtest (vk-ref vk:queue-family-properties (queue-flags) families i) vk:queue-graphics-bit)
                     (begin (vk:get-physical-device-surface-support-khr pd i surface supported)
                            (= 1 (bytevector-u32-native-ref supported 0)))))
              (iota n))))))

(define (usable-device pd)
  (and (>= (vk-ref vk:physical-device-properties (api-version) (device-properties pd)) vk:api-version-1-3)
       (has-device-extension? pd "VK_KHR_swapchain")
       (graphics-present-family pd)))

(define-values (physical-device queue-family)
  (call-with-values
      (lambda () (enumerate 8 (lambda (count items) (vk:enumerate-physical-devices instance count items))))
    (lambda (n devices)
      (when (and device-choice (not (and (exact-integer? device-choice) (< -1 device-choice n))))
        (error "no such device; --device takes 0 to" (- n 1)))
      (let loop ((ds (if device-choice
                         (list (bytevector-u64-native-ref devices (* 8 device-choice)))
                         (map (lambda (i) (bytevector-u64-native-ref devices (* 8 i))) (iota n)))))
        (cond ((null? ds) (error "no device with Vulkan 1.3, VK_KHR_swapchain and presentation"))
              ((usable-device (car ds)) => (lambda (q) (values (car ds) q)))
              (else (loop (cdr ds))))))))

;; Anisotropic filtering, if the device has it: the sampler's maximum, else #f.
(define max-anisotropy
  (let ((features (make-vk vk:physical-device-features)))
    (vk:get-physical-device-features physical-device features)
    (and (= vk:true (vk-ref vk:physical-device-features (sampler-anisotropy) features))
         (min 16.0 (vk-ref vk:physical-device-properties (limits max-sampler-anisotropy)
                           (device-properties physical-device))))))

(format #t "device: ~a~%" (device-name physical-device))

(define device
  (create 'vkCreateDevice
          (lambda (out)
            (vk:create-device
             physical-device
             (vk-new vk:device-create-info
                     (s-type vk:structure-type-device-create-info)
                     (p-next (vk-new vk:physical-device-features2
                                     (s-type vk:structure-type-physical-device-features-2)
                                     (p-next (vk-new vk:physical-device-vulkan13-features
                                                     (s-type vk:structure-type-physical-device-vulkan-1-3-features)
                                                     (synchronization2 vk:true)
                                                     (dynamic-rendering vk:true)))
                                     (features sampler-anisotropy (if max-anisotropy vk:true vk:false))))
                     (queue-create-info-count 1)
                     (p-queue-create-infos (vk-new vk:device-queue-create-info
                                                   (s-type vk:structure-type-device-queue-create-info)
                                                   (queue-family-index queue-family)
                                                   (queue-count 1)
                                                   (p-queue-priorities (make-f32vector 1 1.0))))
                     (enabled-extension-count 1)
                     (pp-enabled-extension-names (vk-c-string-array '("VK_KHR_swapchain"))))
             #f out))))

(define queue
  (let ((slot (make-bytevector 8 0)))
    (vk:get-device-queue device queue-family 0 slot)
    (bytevector-u64-native-ref slot 0)))

;;; ------------------------------------------------------------------
;;; Memory, buffers and one-off commands.

(define memory-properties
  (let ((props (make-vk vk:physical-device-memory-properties)))
    (vk:get-physical-device-memory-properties physical-device props)
    props))

(define (find-memory-type type-bits wanted)
  (or (find (lambda (i)
              (and (logbit? i type-bits)
                   (= wanted (logand wanted (vk-ref vk:physical-device-memory-properties
                                                    (memory-types i property-flags) memory-properties)))))
            (iota (vk-ref vk:physical-device-memory-properties (memory-type-count) memory-properties)))
      (error "no suitable memory type")))

(define (allocate-memory reqs properties)
  (create 'vkAllocateMemory
          (lambda (out)
            (vk:allocate-memory device
                                (vk-new vk:memory-allocate-info
                                        (s-type vk:structure-type-memory-allocate-info)
                                        (allocation-size (vk-ref vk:memory-requirements (size) reqs))
                                        (memory-type-index
                                         (find-memory-type (vk-ref vk:memory-requirements (memory-type-bits) reqs)
                                                           properties)))
                                #f out))))

;; A host-visible buffer holding BYTES => (values buffer memory).
(define (make-host-buffer bytes usage)
  (let* ((buffer (create 'vkCreateBuffer
                         (lambda (out)
                           (vk:create-buffer device
                                             (vk-new vk:buffer-create-info
                                                     (s-type vk:structure-type-buffer-create-info)
                                                     (size (bytevector-length bytes))
                                                     (usage usage)
                                                     (sharing-mode vk:sharing-mode-exclusive))
                                             #f out))))
         (reqs (make-vk vk:memory-requirements)))
    (vk:get-buffer-memory-requirements device buffer reqs)
    (let ((memory (allocate-memory reqs (logior vk:memory-property-host-visible-bit
                                                vk:memory-property-host-coherent-bit))))
      (check 'vkBindBufferMemory (vk:bind-buffer-memory device buffer memory 0))
      (let ((mapped (vk-bytevector (create 'vkMapMemory
                                           (lambda (out) (vk:map-memory device memory 0 vk:whole-size 0 out)))
                                   (bytevector-length bytes))))
        (bytevector-copy! bytes 0 mapped 0 (bytevector-length bytes))
        (vk:unmap-memory device memory))
      (values buffer memory))))

(define command-pool
  (create 'vkCreateCommandPool
          (lambda (out)
            (vk:create-command-pool device
                                    (vk-new vk:command-pool-create-info
                                            (s-type vk:structure-type-command-pool-create-info)
                                            (flags vk:command-pool-create-reset-command-buffer-bit)
                                            (queue-family-index queue-family))
                                    #f out))))

;; Record commands with (RECORD cmd), submit them and wait.
(define (run-commands! record)
  (let ((slot (make-bytevector 8 0)))
    (check 'vkAllocateCommandBuffers
           (vk:allocate-command-buffers device
                                        (vk-new vk:command-buffer-allocate-info
                                                (s-type vk:structure-type-command-buffer-allocate-info)
                                                (command-pool command-pool)
                                                (level vk:command-buffer-level-primary)
                                                (command-buffer-count 1))
                                        slot))
    (let ((cmd (bytevector-u64-native-ref slot 0)))
      (check 'vkBeginCommandBuffer
             (vk:begin-command-buffer cmd (vk-new vk:command-buffer-begin-info
                                                  (s-type vk:structure-type-command-buffer-begin-info)
                                                  (flags vk:command-buffer-usage-one-time-submit-bit))))
      (record cmd)
      (check 'vkEndCommandBuffer (vk:end-command-buffer cmd))
      (check 'vkQueueSubmit (vk:queue-submit queue 1 (vk-new vk:submit-info
                                                             (s-type vk:structure-type-submit-info)
                                                             (command-buffer-count 1)
                                                             (p-command-buffers slot))
                                             0))
      (check 'vkQueueWaitIdle (vk:queue-wait-idle queue))
      (vk:free-command-buffers device command-pool 1 slot))))

;; One image layout transition (setup only).
(define (image-barrier! cmd image aspect base-level levels from to src-stage src-access dst-stage dst-access)
  (vk:cmd-pipeline-barrier2
   cmd
   (vk-new vk:dependency-info
           (s-type vk:structure-type-dependency-info)
           (image-memory-barrier-count 1)
           (p-image-memory-barriers
            (vk-new vk:image-memory-barrier2
                    (s-type vk:structure-type-image-memory-barrier-2)
                    (src-stage-mask src-stage) (src-access-mask src-access)
                    (dst-stage-mask dst-stage) (dst-access-mask dst-access)
                    (old-layout from) (new-layout to)
                    (src-queue-family-index vk:queue-family-ignored)
                    (dst-queue-family-index vk:queue-family-ignored)
                    (image image)
                    (subresource-range aspect-mask aspect)
                    (subresource-range base-mip-level base-level)
                    (subresource-range level-count levels)
                    (subresource-range layer-count 1))))))

(define (create-view image format aspect levels)
  (create 'vkCreateImageView
          (lambda (out)
            (vk:create-image-view device
                                  (vk-new vk:image-view-create-info
                                          (s-type vk:structure-type-image-view-create-info)
                                          (image image)
                                          (view-type vk:image-view-type-2d)
                                          (format format)
                                          (subresource-range aspect-mask aspect)
                                          (subresource-range level-count levels)
                                          (subresource-range layer-count 1))
                                  #f out))))

;;; ------------------------------------------------------------------
;;; Texture: PPM -> staging buffer -> sRGB image with mipmaps.

(define texture-format vk:format-r8g8b8a8-srgb)

(define-values (texture-image texture-memory texture-levels texture-width texture-height)
  (call-with-values (lambda () (read-ppm texture-path))
    (lambda (w h rgba)
      (let* ((can-blit? (let ((props (make-vk vk:format-properties))
                              (needed (logior vk:format-feature-blit-src-bit vk:format-feature-blit-dst-bit
                                              vk:format-feature-sampled-image-filter-linear-bit)))
                          (vk:get-physical-device-format-properties physical-device texture-format props)
                          (= needed (logand needed (vk-ref vk:format-properties (optimal-tiling-features) props)))))
             (levels (if can-blit? (integer-length (max w h)) 1))   ; down to 1x1
             (color vk:image-aspect-color-bit)
             (transfer vk:pipeline-stage-2-all-transfer-bit)
             (image (create 'vkCreateImage
                            (lambda (out)
                              (vk:create-image device
                                               (vk-new vk:image-create-info
                                                       (s-type vk:structure-type-image-create-info)
                                                       (image-type vk:image-type-2d)
                                                       (format texture-format)
                                                       (extent width w) (extent height h) (extent depth 1)
                                                       (mip-levels levels) (array-layers 1)
                                                       (samples vk:sample-count-1-bit)
                                                       (tiling vk:image-tiling-optimal)
                                                       (usage (logior vk:image-usage-transfer-src-bit
                                                                      vk:image-usage-transfer-dst-bit
                                                                      vk:image-usage-sampled-bit))
                                                       (sharing-mode vk:sharing-mode-exclusive)
                                                       (initial-layout vk:image-layout-undefined))
                                               #f out))))
             (memory (let ((reqs (make-vk vk:memory-requirements)))
                       (vk:get-image-memory-requirements device image reqs)
                       (allocate-memory reqs vk:memory-property-device-local-bit))))
        (check 'vkBindImageMemory (vk:bind-image-memory device image memory 0))
        (call-with-values (lambda () (make-host-buffer rgba vk:buffer-usage-transfer-src-bit))
          (lambda (staging staging-memory)
            (run-commands!
             (lambda (cmd)
               (image-barrier! cmd image color 0 levels
                               vk:image-layout-undefined vk:image-layout-transfer-dst-optimal
                               vk:pipeline-stage-2-none vk:access-2-none transfer vk:access-2-transfer-write-bit)
               (vk:cmd-copy-buffer-to-image cmd staging image vk:image-layout-transfer-dst-optimal 1
                                            (vk-new vk:buffer-image-copy
                                                    (image-subresource aspect-mask color)
                                                    (image-subresource layer-count 1)
                                                    (image-extent width w) (image-extent height h)
                                                    (image-extent depth 1)))
               ;; Each level is a linear downscale of the previous one.
               (for-each
                (lambda (level)
                  (image-barrier! cmd image color (- level 1) 1
                                  vk:image-layout-transfer-dst-optimal vk:image-layout-transfer-src-optimal
                                  transfer vk:access-2-transfer-write-bit transfer vk:access-2-transfer-read-bit)
                  (vk:cmd-blit-image cmd image vk:image-layout-transfer-src-optimal
                                     image vk:image-layout-transfer-dst-optimal 1
                                     (vk-new vk:image-blit
                                             (src-subresource aspect-mask color)
                                             (src-subresource mip-level (- level 1))
                                             (src-subresource layer-count 1)
                                             (src-offsets 1 x (max 1 (ash w (- 1 level))))
                                             (src-offsets 1 y (max 1 (ash h (- 1 level))))
                                             (src-offsets 1 z 1)
                                             (dst-subresource aspect-mask color)
                                             (dst-subresource mip-level level)
                                             (dst-subresource layer-count 1)
                                             (dst-offsets 1 x (max 1 (ash w (- level))))
                                             (dst-offsets 1 y (max 1 (ash h (- level))))
                                             (dst-offsets 1 z 1))
                                     vk:filter-linear))
                (iota (- levels 1) 1))
               ;; Levels 0..n-2 were blit sources; the last was only written.
               (when (> levels 1)
                 (image-barrier! cmd image color 0 (- levels 1)
                                 vk:image-layout-transfer-src-optimal vk:image-layout-shader-read-only-optimal
                                 transfer vk:access-2-transfer-read-bit
                                 vk:pipeline-stage-2-fragment-shader-bit vk:access-2-shader-sampled-read-bit))
               (image-barrier! cmd image color (- levels 1) 1
                               vk:image-layout-transfer-dst-optimal vk:image-layout-shader-read-only-optimal
                               transfer vk:access-2-transfer-write-bit
                               vk:pipeline-stage-2-fragment-shader-bit vk:access-2-shader-sampled-read-bit)))
            (vk:destroy-buffer device staging #f)
            (vk:free-memory device staging-memory #f)))
        (values image memory levels w h)))))

(format #t "texture: ~a (~ax~a, ~a mip levels~@[, ~ax anisotropic~])~%"
        texture-path texture-width texture-height texture-levels
        (and max-anisotropy (inexact->exact (round max-anisotropy))))

(define texture-view (create-view texture-image texture-format vk:image-aspect-color-bit texture-levels))

(define sampler
  (create 'vkCreateSampler
          (lambda (out)
            (vk:create-sampler device
                               (vk-new vk:sampler-create-info
                                       (s-type vk:structure-type-sampler-create-info)
                                       (mag-filter vk:filter-linear)
                                       (min-filter vk:filter-linear)
                                       (mipmap-mode vk:sampler-mipmap-mode-linear)
                                       (address-mode-u vk:sampler-address-mode-clamp-to-edge)
                                       (address-mode-v vk:sampler-address-mode-clamp-to-edge)
                                       (address-mode-w vk:sampler-address-mode-clamp-to-edge)
                                       (anisotropy-enable (if max-anisotropy vk:true vk:false))
                                       (max-anisotropy (or max-anisotropy 1.0))
                                       (max-lod vk:lod-clamp-none))
                               #f out))))

;;; ------------------------------------------------------------------
;;; Descriptor set: binding 0 = the texture and its sampler.

(define set-layout
  (create 'vkCreateDescriptorSetLayout
          (lambda (out)
            (vk:create-descriptor-set-layout
             device
             (vk-new vk:descriptor-set-layout-create-info
                     (s-type vk:structure-type-descriptor-set-layout-create-info)
                     (binding-count 1)
                     (p-bindings (vk-new vk:descriptor-set-layout-binding
                                         (binding 0)
                                         (descriptor-type vk:descriptor-type-combined-image-sampler)
                                         (descriptor-count 1)
                                         (stage-flags vk:shader-stage-fragment-bit))))
             #f out))))

(define descriptor-pool
  (create 'vkCreateDescriptorPool
          (lambda (out)
            (vk:create-descriptor-pool
             device
             (vk-new vk:descriptor-pool-create-info
                     (s-type vk:structure-type-descriptor-pool-create-info)
                     (max-sets 1)
                     (pool-size-count 1)
                     (p-pool-sizes (vk-new vk:descriptor-pool-size
                                           (type vk:descriptor-type-combined-image-sampler)
                                           (descriptor-count 1))))
             #f out))))

(define descriptor-set
  (let ((set (create 'vkAllocateDescriptorSets
                     (lambda (out)
                       (vk:allocate-descriptor-sets device
                                                    (vk-new vk:descriptor-set-allocate-info
                                                            (s-type vk:structure-type-descriptor-set-allocate-info)
                                                            (descriptor-pool descriptor-pool)
                                                            (descriptor-set-count 1)
                                                            (p-set-layouts (handle-array set-layout)))
                                                    out)))))
    (vk:update-descriptor-sets
     device 1
     (vk-new vk:write-descriptor-set
             (s-type vk:structure-type-write-descriptor-set)
             (dst-set set)
             (dst-binding 0)
             (descriptor-count 1)
             (descriptor-type vk:descriptor-type-combined-image-sampler)
             (p-image-info (vk-new vk:descriptor-image-info
                                   (sampler sampler)
                                   (image-view texture-view)
                                   (image-layout vk:image-layout-shader-read-only-optimal))))
     0 #f)
    set))

;;; ------------------------------------------------------------------
;;; Vertex buffer.

(define vertex-count (length cube-vertices))
(define-values (vertex-buffer vertex-memory)
  (let ((bytes (list->f32vector (map exact->inexact (concatenate cube-vertices)))))
    (make-host-buffer bytes vk:buffer-usage-vertex-buffer-bit)))

;;; ------------------------------------------------------------------
;;; Surface format, depth format, present mode.

(define-values (surface-format color-space)
  (call-with-values
      (lambda () (enumerate (vk-sizeof vk:surface-format-khr)
                            (lambda (count items)
                              (vk:get-physical-device-surface-formats-khr physical-device surface count items))))
    (lambda (n formats)
      ;; sRGB, so the sRGB texture is shown with the right colors.
      (let ((i (or (find (lambda (i)
                           (and (memv (vk-ref vk:surface-format-khr (format) formats i)
                                      (list vk:format-b8g8r8a8-srgb vk:format-r8g8b8a8-srgb))
                                (= vk:color-space-srgb-nonlinear-khr
                                   (vk-ref vk:surface-format-khr (color-space) formats i))))
                         (iota n))
                   0)))
        (values (vk-ref vk:surface-format-khr (format) formats i)
                (vk-ref vk:surface-format-khr (color-space) formats i))))))

(define depth-format
  (let ((props (make-vk vk:format-properties)))
    (vk:get-physical-device-format-properties physical-device vk:format-d32-sfloat props)
    (if (logtest (vk-ref vk:format-properties (optimal-tiling-features) props)
                 vk:format-feature-depth-stencil-attachment-bit)
        vk:format-d32-sfloat
        vk:format-d16-unorm)))

(define present-mode
  (call-with-values
      (lambda () (enumerate 4 (lambda (count items)
                                (vk:get-physical-device-surface-present-modes-khr physical-device surface count items))))
    (lambda (n modes)
      (let ((available (map (lambda (i) (bytevector-u32-native-ref modes (* 4 i))) (iota n))))
        (cond ((not no-vsync?) vk:present-mode-fifo-khr)
              ((memv vk:present-mode-mailbox-khr available) vk:present-mode-mailbox-khr)
              ((memv vk:present-mode-immediate-khr available) vk:present-mode-immediate-khr)
              (else vk:present-mode-fifo-khr))))))

(format #t "present mode: ~a~%"
        (assv-ref `((,vk:present-mode-fifo-khr . "FIFO (vsync)")
                    (,vk:present-mode-mailbox-khr . "mailbox")
                    (,vk:present-mode-immediate-khr . "immediate"))
                  present-mode))

;;; ------------------------------------------------------------------
;;; Pipeline.

(define (shader-module path)
  (let ((code (read-file-bytevector path)))
    (create 'vkCreateShaderModule
            (lambda (out)
              (vk:create-shader-module device
                                       (vk-new vk:shader-module-create-info
                                               (s-type vk:structure-type-shader-module-create-info)
                                               (code-size (bytevector-length code))
                                               (p-code code))
                                       #f out)))))

(define push-constant-size 112)           ; mat4 mvp + mat3 normal matrix

(define pipeline-layout
  (create 'vkCreatePipelineLayout
          (lambda (out)
            (vk:create-pipeline-layout
             device
             (vk-new vk:pipeline-layout-create-info
                     (s-type vk:structure-type-pipeline-layout-create-info)
                     (set-layout-count 1)
                     (p-set-layouts (handle-array set-layout))
                     (push-constant-range-count 1)
                     (p-push-constant-ranges (vk-new vk:push-constant-range
                                                     (stage-flags vk:shader-stage-vertex-bit)
                                                     (offset 0)
                                                     (size push-constant-size))))
             #f out))))

(define pipeline
  (let* ((vert (shader-module "examples/cube/cube.vert.spv"))
         (frag (shader-module "examples/cube/cube.frag.spv"))
         (stages (make-vk vk:pipeline-shader-stage-create-info 2))
         (attributes (make-vk vk:vertex-input-attribute-description 3))
         (color-format (make-bytevector 4 0))
         (dynamic-states (make-bytevector 8 0)))
    (for-each (lambda (i stage module)
                (vk-set! vk:pipeline-shader-stage-create-info (s-type) stages i
                         vk:structure-type-pipeline-shader-stage-create-info)
                (vk-set! vk:pipeline-shader-stage-create-info (stage) stages i stage)
                (vk-set! vk:pipeline-shader-stage-create-info (module) stages i module)
                (vk-set! vk:pipeline-shader-stage-create-info (p-name) stages i "main"))
              '(0 1) (list vk:shader-stage-vertex-bit vk:shader-stage-fragment-bit) (list vert frag))
    (for-each (lambda (i format offset)
                (vk-set! vk:vertex-input-attribute-description (location) attributes i i)
                (vk-set! vk:vertex-input-attribute-description (format) attributes i format)
                (vk-set! vk:vertex-input-attribute-description (offset) attributes i offset))
              '(0 1 2)
              (list vk:format-r32g32b32-sfloat vk:format-r32g32b32-sfloat vk:format-r32g32-sfloat)
              '(0 12 24))
    (bytevector-u32-native-set! color-format 0 surface-format)
    (bytevector-u32-native-set! dynamic-states 0 vk:dynamic-state-viewport)
    (bytevector-u32-native-set! dynamic-states 4 vk:dynamic-state-scissor)
    (let ((p (create
              'vkCreateGraphicsPipelines
              (lambda (out)
                (vk:create-graphics-pipelines
                 device 0 1
                 (vk-new vk:graphics-pipeline-create-info
                         (s-type vk:structure-type-graphics-pipeline-create-info)
                         (p-next (vk-new vk:pipeline-rendering-create-info
                                         (s-type vk:structure-type-pipeline-rendering-create-info)
                                         (color-attachment-count 1)
                                         (p-color-attachment-formats color-format)
                                         (depth-attachment-format depth-format)))
                         (stage-count 2)
                         (p-stages stages)
                         (p-vertex-input-state
                          (vk-new vk:pipeline-vertex-input-state-create-info
                                  (s-type vk:structure-type-pipeline-vertex-input-state-create-info)
                                  (vertex-binding-description-count 1)
                                  (p-vertex-binding-descriptions
                                   (vk-new vk:vertex-input-binding-description
                                           (binding 0) (stride 32) (input-rate vk:vertex-input-rate-vertex)))
                                  (vertex-attribute-description-count 3)
                                  (p-vertex-attribute-descriptions attributes)))
                         (p-input-assembly-state
                          (vk-new vk:pipeline-input-assembly-state-create-info
                                  (s-type vk:structure-type-pipeline-input-assembly-state-create-info)
                                  (topology vk:primitive-topology-triangle-list)))
                         (p-viewport-state
                          (vk-new vk:pipeline-viewport-state-create-info
                                  (s-type vk:structure-type-pipeline-viewport-state-create-info)
                                  (viewport-count 1) (scissor-count 1)))
                         (p-rasterization-state
                          (vk-new vk:pipeline-rasterization-state-create-info
                                  (s-type vk:structure-type-pipeline-rasterization-state-create-info)
                                  (polygon-mode vk:polygon-mode-fill)
                                  (cull-mode vk:cull-mode-back-bit)
                                  (front-face vk:front-face-counter-clockwise)
                                  (line-width 1.0)))
                         (p-multisample-state
                          (vk-new vk:pipeline-multisample-state-create-info
                                  (s-type vk:structure-type-pipeline-multisample-state-create-info)
                                  (rasterization-samples vk:sample-count-1-bit)))
                         (p-depth-stencil-state
                          (vk-new vk:pipeline-depth-stencil-state-create-info
                                  (s-type vk:structure-type-pipeline-depth-stencil-state-create-info)
                                  (depth-test-enable vk:true)
                                  (depth-write-enable vk:true)
                                  (depth-compare-op vk:compare-op-less)))
                         (p-color-blend-state
                          (vk-new vk:pipeline-color-blend-state-create-info
                                  (s-type vk:structure-type-pipeline-color-blend-state-create-info)
                                  (attachment-count 1)
                                  (p-attachments (vk-new vk:pipeline-color-blend-attachment-state
                                                         (color-write-mask
                                                          (logior vk:color-component-r-bit vk:color-component-g-bit
                                                                  vk:color-component-b-bit vk:color-component-a-bit))))))
                         (p-dynamic-state
                          (vk-new vk:pipeline-dynamic-state-create-info
                                  (s-type vk:structure-type-pipeline-dynamic-state-create-info)
                                  (dynamic-state-count 2)
                                  (p-dynamic-states dynamic-states)))
                         (layout pipeline-layout)
                         (base-pipeline-index -1))
                 #f out)))))
      (vk:destroy-shader-module device vert #f)
      (vk:destroy-shader-module device frag #f)
      p)))

;;; ------------------------------------------------------------------
;;; Swapchain, image views, depth buffer, per-image semaphores.

(define swapchain 0)
(define extent-width 0)
(define extent-height 0)
(define made-for-size #f)                 ; the window size the swapchain was made for
(define swapchain-images #())
(define swapchain-views #())
(define render-done #())
(define depth-image 0)
(define depth-memory 0)
(define depth-view 0)

(define (create-semaphore)
  (create 'vkCreateSemaphore
          (lambda (out)
            (vk:create-semaphore device (vk-new vk:semaphore-create-info
                                                (s-type vk:structure-type-semaphore-create-info))
                                 #f out))))

(define (destroy-swapchain-resources)
  (for-each (lambda (v) (vk:destroy-image-view device v #f)) (vector->list swapchain-views))
  (for-each (lambda (s) (vk:destroy-semaphore device s #f)) (vector->list render-done))
  (unless (zero? depth-view)
    (vk:destroy-image-view device depth-view #f)
    (vk:destroy-image device depth-image #f)
    (vk:free-memory device depth-memory #f)
    (set! depth-view 0)))

;; Returns #f when the window has no area, #t otherwise.
(define (create-swapchain!)
  (let ((caps (make-vk vk:surface-capabilities-khr)))
    (check 'vkGetPhysicalDeviceSurfaceCapabilitiesKHR
           (vk:get-physical-device-surface-capabilities-khr physical-device surface caps))
    (let* ((current-w (vk-ref vk:surface-capabilities-khr (current-extent width) caps))
           (clamp (lambda (v lo hi) (max lo (min hi v))))
           ;; On Wayland the extent is #xFFFFFFFF: the window takes the
           ;; swapchain's size, which follows the compositor's configure.
           (w (if (= current-w #xFFFFFFFF)
                  (clamp (car (wayland-window-size window))
                         (vk-ref vk:surface-capabilities-khr (min-image-extent width) caps)
                         (vk-ref vk:surface-capabilities-khr (max-image-extent width) caps))
                  current-w))
           (h (if (= current-w #xFFFFFFFF)
                  (clamp (cdr (wayland-window-size window))
                         (vk-ref vk:surface-capabilities-khr (min-image-extent height) caps)
                         (vk-ref vk:surface-capabilities-khr (max-image-extent height) caps))
                  (vk-ref vk:surface-capabilities-khr (current-extent height) caps))))
      (and (> w 0) (> h 0)
           (let* ((min-count (vk-ref vk:surface-capabilities-khr (min-image-count) caps))
                  (max-count (vk-ref vk:surface-capabilities-khr (max-image-count) caps))
                  (count (if (zero? max-count) (+ min-count 1) (min (+ min-count 1) max-count)))
                  (alpha-modes (vk-ref vk:surface-capabilities-khr (supported-composite-alpha) caps))
                  (alpha (if (logtest alpha-modes vk:composite-alpha-opaque-bit-khr)
                             vk:composite-alpha-opaque-bit-khr
                             (logand alpha-modes (- alpha-modes))))
                  (new-swapchain
                   (create 'vkCreateSwapchainKHR
                           (lambda (out)
                             (vk:create-swapchain-khr
                              device
                              (vk-new vk:swapchain-create-info-khr
                                      (s-type vk:structure-type-swapchain-create-info-khr)
                                      (surface surface)
                                      (min-image-count count)
                                      (image-format surface-format)
                                      (image-color-space color-space)
                                      (image-extent width w)
                                      (image-extent height h)
                                      (image-array-layers 1)
                                      (image-usage vk:image-usage-color-attachment-bit)
                                      (image-sharing-mode vk:sharing-mode-exclusive)
                                      (pre-transform (vk-ref vk:surface-capabilities-khr (current-transform) caps))
                                      (composite-alpha alpha)
                                      (present-mode present-mode)
                                      (clipped vk:true)
                                      (old-swapchain swapchain))
                              #f out)))))
             (destroy-swapchain-resources)
             (unless (zero? swapchain) (vk:destroy-swapchain-khr device swapchain #f))
             (set! swapchain new-swapchain)
             (set! extent-width w)
             (set! extent-height h)
             (set! made-for-size (wayland-window-size window))
             (set! swapchain-images
                   (call-with-values
                       (lambda () (enumerate 8 (lambda (count items)
                                                 (vk:get-swapchain-images-khr device swapchain count items))))
                     (lambda (n images)
                       (list->vector (map (lambda (i) (bytevector-u64-native-ref images (* 8 i))) (iota n))))))
             (set! swapchain-views
                   (list->vector (map (lambda (img) (create-view img surface-format vk:image-aspect-color-bit 1))
                                      (vector->list swapchain-images))))
             (set! render-done (list->vector (map (lambda (img) (create-semaphore)) (vector->list swapchain-images))))
             (set! depth-image
                   (create 'vkCreateImage
                           (lambda (out)
                             (vk:create-image device
                                              (vk-new vk:image-create-info
                                                      (s-type vk:structure-type-image-create-info)
                                                      (image-type vk:image-type-2d)
                                                      (format depth-format)
                                                      (extent width w) (extent height h) (extent depth 1)
                                                      (mip-levels 1) (array-layers 1)
                                                      (samples vk:sample-count-1-bit)
                                                      (tiling vk:image-tiling-optimal)
                                                      (usage vk:image-usage-depth-stencil-attachment-bit)
                                                      (sharing-mode vk:sharing-mode-exclusive)
                                                      (initial-layout vk:image-layout-undefined))
                                              #f out))))
             (let ((reqs (make-vk vk:memory-requirements)))
               (vk:get-image-memory-requirements device depth-image reqs)
               (set! depth-memory (allocate-memory reqs vk:memory-property-device-local-bit)))
             (check 'vkBindImageMemory (vk:bind-image-memory device depth-image depth-memory 0))
             (set! depth-view (create-view depth-image depth-format vk:image-aspect-depth-bit 1))
             #t)))))

;;; ------------------------------------------------------------------
;;; Per-frame objects, and structures reused every frame.

(define command-buffers
  (let ((out (make-bytevector (* 8 frames-in-flight) 0)))
    (check 'vkAllocateCommandBuffers
           (vk:allocate-command-buffers device
                                        (vk-new vk:command-buffer-allocate-info
                                                (s-type vk:structure-type-command-buffer-allocate-info)
                                                (command-pool command-pool)
                                                (level vk:command-buffer-level-primary)
                                                (command-buffer-count frames-in-flight))
                                        out))
    (list->vector (map (lambda (i) (bytevector-u64-native-ref out (* 8 i))) (iota frames-in-flight)))))

(define fences
  (list->vector
   (map (lambda (i)
          (create 'vkCreateFence
                  (lambda (out)
                    (vk:create-fence device (vk-new vk:fence-create-info
                                                    (s-type vk:structure-type-fence-create-info)
                                                    (flags vk:fence-create-signaled-bit))
                                     #f out))))
        (iota frames-in-flight))))

(define image-acquired (list->vector (map (lambda (i) (create-semaphore)) (iota frames-in-flight))))

(define begin-info (vk-new vk:command-buffer-begin-info
                           (s-type vk:structure-type-command-buffer-begin-info)
                           (flags vk:command-buffer-usage-one-time-submit-bit)))

(define to-attachment (make-vk vk:image-memory-barrier2 2))
(define to-present (make-vk vk:image-memory-barrier2))

(define (init-barrier! bv i src-stage src-access dst-stage dst-access old new aspect)
  (vk-set! vk:image-memory-barrier2 (s-type) bv i vk:structure-type-image-memory-barrier-2)
  (vk-set! vk:image-memory-barrier2 (src-stage-mask) bv i src-stage)
  (vk-set! vk:image-memory-barrier2 (src-access-mask) bv i src-access)
  (vk-set! vk:image-memory-barrier2 (dst-stage-mask) bv i dst-stage)
  (vk-set! vk:image-memory-barrier2 (dst-access-mask) bv i dst-access)
  (vk-set! vk:image-memory-barrier2 (old-layout) bv i old)
  (vk-set! vk:image-memory-barrier2 (new-layout) bv i new)
  (vk-set! vk:image-memory-barrier2 (src-queue-family-index) bv i vk:queue-family-ignored)
  (vk-set! vk:image-memory-barrier2 (dst-queue-family-index) bv i vk:queue-family-ignored)
  (vk-set! vk:image-memory-barrier2 (subresource-range aspect-mask) bv i aspect)
  (vk-set! vk:image-memory-barrier2 (subresource-range level-count) bv i 1)
  (vk-set! vk:image-memory-barrier2 (subresource-range layer-count) bv i 1))

(define depth-stages (logior vk:pipeline-stage-2-early-fragment-tests-bit
                             vk:pipeline-stage-2-late-fragment-tests-bit))
(init-barrier! to-attachment 0
               vk:pipeline-stage-2-color-attachment-output-bit vk:access-2-none
               vk:pipeline-stage-2-color-attachment-output-bit vk:access-2-color-attachment-write-bit
               vk:image-layout-undefined vk:image-layout-color-attachment-optimal
               vk:image-aspect-color-bit)
(init-barrier! to-attachment 1
               depth-stages vk:access-2-depth-stencil-attachment-write-bit
               depth-stages (logior vk:access-2-depth-stencil-attachment-read-bit
                                    vk:access-2-depth-stencil-attachment-write-bit)
               vk:image-layout-undefined vk:image-layout-depth-attachment-optimal
               vk:image-aspect-depth-bit)
(init-barrier! to-present 0
               vk:pipeline-stage-2-color-attachment-output-bit vk:access-2-color-attachment-write-bit
               vk:pipeline-stage-2-none vk:access-2-none
               vk:image-layout-color-attachment-optimal vk:image-layout-present-src-khr
               vk:image-aspect-color-bit)

(define to-attachment-dependency (vk-new vk:dependency-info
                                         (s-type vk:structure-type-dependency-info)
                                         (image-memory-barrier-count 2)
                                         (p-image-memory-barriers to-attachment)))
(define to-present-dependency (vk-new vk:dependency-info
                                      (s-type vk:structure-type-dependency-info)
                                      (image-memory-barrier-count 1)
                                      (p-image-memory-barriers to-present)))

(define color-attachment (vk-new vk:rendering-attachment-info
                                 (s-type vk:structure-type-rendering-attachment-info)
                                 (image-layout vk:image-layout-color-attachment-optimal)
                                 (load-op vk:attachment-load-op-clear)
                                 (store-op vk:attachment-store-op-store)
                                 (clear-value color float32 0 0.02)
                                 (clear-value color float32 1 0.02)
                                 (clear-value color float32 2 0.03)
                                 (clear-value color float32 3 1.0)))
(define depth-attachment (vk-new vk:rendering-attachment-info
                                 (s-type vk:structure-type-rendering-attachment-info)
                                 (image-layout vk:image-layout-depth-attachment-optimal)
                                 (load-op vk:attachment-load-op-clear)
                                 (store-op vk:attachment-store-op-dont-care)
                                 (clear-value depth-stencil depth 1.0)))
(define rendering-info (vk-new vk:rendering-info
                               (s-type vk:structure-type-rendering-info)
                               (layer-count 1)
                               (color-attachment-count 1)
                               (p-color-attachments color-attachment)
                               (p-depth-attachment depth-attachment)))
(define viewport (vk-new vk:viewport (min-depth 0.0) (max-depth 1.0)))
(define scissor (make-vk vk:rect2-d))
(define vertex-buffers (handle-array vertex-buffer))
(define vertex-offsets (make-bytevector 8 0))
(define descriptor-sets (handle-array descriptor-set))
(define push-constants (make-bytevector push-constant-size 0))

(define wait-stage (make-bytevector 4 0))
(bytevector-u32-native-set! wait-stage 0 vk:pipeline-stage-color-attachment-output-bit)
(define submit-wait (handle-array 0))
(define submit-signal (handle-array 0))
(define submit-command-buffer (handle-array 0))
(define submit-info (vk-new vk:submit-info
                            (s-type vk:structure-type-submit-info)
                            (wait-semaphore-count 1)
                            (p-wait-semaphores submit-wait)
                            (p-wait-dst-stage-mask wait-stage)
                            (command-buffer-count 1)
                            (p-command-buffers submit-command-buffer)
                            (signal-semaphore-count 1)
                            (p-signal-semaphores submit-signal)))
(define present-swapchain (handle-array 0))
(define present-index (make-bytevector 4 0))
(define present-info (vk-new vk:present-info-khr
                             (s-type vk:structure-type-present-info-khr)
                             (wait-semaphore-count 1)
                             (p-wait-semaphores submit-signal)
                             (swapchain-count 1)
                             (p-swapchains present-swapchain)
                             (p-image-indices present-index)))
(define fence-slot (handle-array 0))
(define image-index-slot (make-bytevector 4 0))

;;; ------------------------------------------------------------------
;;; Drawing.

(define spin 0.0)                         ; degrees around Y
(define tilt-x 25.0)                      ; arrow keys
(define tilt-y 0.0)

(define (record! cmd index)
  (check 'vkResetCommandBuffer (vk:reset-command-buffer cmd 0))
  (check 'vkBeginCommandBuffer (vk:begin-command-buffer cmd begin-info))
  (vk-set! vk:image-memory-barrier2 (image) to-attachment 0 (vector-ref swapchain-images index))
  (vk-set! vk:image-memory-barrier2 (image) to-attachment 1 depth-image)
  (vk-set! vk:image-memory-barrier2 (image) to-present (vector-ref swapchain-images index))
  (vk:cmd-pipeline-barrier2 cmd to-attachment-dependency)
  (vk-set! vk:rendering-attachment-info (image-view) color-attachment (vector-ref swapchain-views index))
  (vk-set! vk:rendering-attachment-info (image-view) depth-attachment depth-view)
  (vk-set! vk:rendering-info (render-area extent width) rendering-info extent-width)
  (vk-set! vk:rendering-info (render-area extent height) rendering-info extent-height)
  (vk:cmd-begin-rendering cmd rendering-info)
  (vk-set! vk:viewport (y) viewport (exact->inexact extent-height))
  (vk-set! vk:viewport (width) viewport (exact->inexact extent-width))
  (vk-set! vk:viewport (height) viewport (exact->inexact (- extent-height)))
  (vk:cmd-set-viewport cmd 0 1 viewport)
  (vk-set! vk:rect2-d (extent width) scissor extent-width)
  (vk-set! vk:rect2-d (extent height) scissor extent-height)
  (vk:cmd-set-scissor cmd 0 1 scissor)
  (vk:cmd-bind-pipeline cmd vk:pipeline-bind-point-graphics pipeline)
  (vk:cmd-bind-descriptor-sets cmd vk:pipeline-bind-point-graphics pipeline-layout 0 1 descriptor-sets 0 #f)
  (vk:cmd-bind-vertex-buffers cmd 0 1 vertex-buffers vertex-offsets)
  (let* ((projection (perspective 45.0 (/ (exact->inexact extent-width) extent-height) 0.1 100.0))
         (modelview (mat* (translate 0 0 -5)
                          (mat* (rotate-x tilt-x) (mat* (rotate-y tilt-y) (rotate-y spin))))))
    (write-matrix! push-constants 0 (mat* projection modelview))
    (write-normal-matrix! push-constants 64 modelview))
  (vk:cmd-push-constants cmd pipeline-layout vk:shader-stage-vertex-bit 0 push-constant-size push-constants)
  (vk:cmd-draw cmd vertex-count 1 0 0)
  (vk:cmd-end-rendering cmd)
  (vk:cmd-pipeline-barrier2 cmd to-present-dependency)
  (check 'vkEndCommandBuffer (vk:end-command-buffer cmd)))

(define swapchain-stale? (not (create-swapchain!)))

;; Draw one frame.  Returns #t if a frame was drawn, #f to retry at once
;; (the swapchain was out of date), or 'idle when no swapchain can be made.
(define (draw-frame! frame)
;; The extent may be clamped to the device's limits, so compare with the
  ;; window size the swapchain was made for.
  (unless (equal? (wayland-window-size window) made-for-size)
    (set! swapchain-stale? #t))
  (when swapchain-stale?
    (check 'vkDeviceWaitIdle (vk:device-wait-idle device))
    (set! swapchain-stale? (not (create-swapchain!))))
  (if swapchain-stale?
      'idle
      (let ((fence (vector-ref fences frame))
            (cmd (vector-ref command-buffers frame)))
        (bytevector-u64-native-set! fence-slot 0 fence)
        (check 'vkWaitForFences (vk:wait-for-fences device 1 fence-slot vk:true max-u64))
        (let ((r (vk:acquire-next-image-khr device swapchain max-u64 (vector-ref image-acquired frame) 0
                                            image-index-slot)))
          (cond
           ((= r vk:error-out-of-date-khr) (set! swapchain-stale? #t) #f)
           ((not (memv r (list vk:success vk:suboptimal-khr))) (check 'vkAcquireNextImageKHR r))
           (else
            (let ((index (bytevector-u32-native-ref image-index-slot 0)))
              (check 'vkResetFences (vk:reset-fences device 1 fence-slot))
              (record! cmd index)
              (bytevector-u64-native-set! submit-wait 0 (vector-ref image-acquired frame))
              (bytevector-u64-native-set! submit-signal 0 (vector-ref render-done index))
              (bytevector-u64-native-set! submit-command-buffer 0 cmd)
              (check 'vkQueueSubmit (vk:queue-submit queue 1 submit-info fence))
              (bytevector-u64-native-set! present-swapchain 0 swapchain)
              (bytevector-u32-native-set! present-index 0 index)
              (let ((p (vk:queue-present-khr queue present-info)))
                (if (memv p (list vk:error-out-of-date-khr vk:suboptimal-khr))
                    (set! swapchain-stale? #t)
                    (check 'vkQueuePresentKHR p)))
              (when (= r vk:suboptimal-khr) (set! swapchain-stale? #t))
              #t)))))))

;;; ------------------------------------------------------------------
;;; Main loop.

(define (now) (/ (get-internal-real-time) (exact->inexact internal-time-units-per-second)))

(define paused? #f)
(define space-was-down? #f)
(define quit? #f)

(define (handle-input! dt)
  (when (wayland-key-down? window key-escape) (set! quit? #t))
  (let ((space (wayland-key-down? window key-space)))
    (when (and space (not space-was-down?)) (set! paused? (not paused?)))
    (set! space-was-down? space))
  (let ((step (* 90.0 dt)))               ; degrees per second while held
    (when (wayland-key-down? window key-up) (set! tilt-x (- tilt-x step)))
    (when (wayland-key-down? window key-down) (set! tilt-x (+ tilt-x step)))
    (when (wayland-key-down? window key-left) (set! tilt-y (- tilt-y step)))
    (when (wayland-key-down? window key-right) (set! tilt-y (+ tilt-y step)))))

(let loop ((frame 0) (frames 0) (total-frames 0) (last (now)) (report-start (now)))
  (when (and (wayland-poll-events window #:on-resize (lambda (w h) (set! swapchain-stale? #t)))
             (not quit?)
             (not (and frame-limit (>= total-frames frame-limit))))
    (let* ((t (now)) (dt (- t last)))
      (handle-input! dt)
      (unless paused?
        (set! spin (let ((a (+ spin (if spin-step (exact->inexact spin-step) (* 45.0 dt)))))
                     (if (>= a 360.0) (- a 360.0) a))))
      (let* ((result (draw-frame! frame))
             (drawn (eq? result #t))
             (frames (if drawn (+ frames 1) frames))
             (total-frames (if drawn (+ total-frames 1) total-frames))
             (next (if drawn (modulo (+ frame 1) frames-in-flight) frame)))
        (when (eq? result 'idle) (usleep 50000))
        (if (>= (- t report-start) 5.0)
            (begin
              (format #t "~a frames in ~,1f seconds = ~,3f FPS~%"
                      frames (- t report-start) (/ frames (- t report-start)))
              (force-output)
              (loop next 0 total-frames t t))
            (loop next frames total-frames t report-start))))))

;;; ------------------------------------------------------------------
;;; Clean up.

(check 'vkDeviceWaitIdle (vk:device-wait-idle device))
(for-each (lambda (s) (vk:destroy-semaphore device s #f)) (vector->list image-acquired))
(for-each (lambda (f) (vk:destroy-fence device f #f)) (vector->list fences))
(destroy-swapchain-resources)
(unless (zero? swapchain) (vk:destroy-swapchain-khr device swapchain #f))
(vk:destroy-pipeline device pipeline #f)
(vk:destroy-pipeline-layout device pipeline-layout #f)
(vk:destroy-descriptor-pool device descriptor-pool #f)
(vk:destroy-descriptor-set-layout device set-layout #f)
(vk:destroy-sampler device sampler #f)
(vk:destroy-image-view device texture-view #f)
(vk:destroy-image device texture-image #f)
(vk:free-memory device texture-memory #f)
(vk:destroy-buffer device vertex-buffer #f)
(vk:free-memory device vertex-memory #f)
(vk:destroy-command-pool device command-pool #f)
(vk:destroy-device device #f)
(vk:destroy-surface-khr instance surface #f)
(vk:destroy-instance instance #f)
(wayland-close-window window)
