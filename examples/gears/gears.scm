;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; Gears: three spinning gears, the classic OpenGL demo redone on Vulkan
;;; with an X11 window.  Needs only Guile, the Vulkan loader and libX11
;;; (XWayland on Wayland desktops).  Run from the repository root:
;;;
;;;   guile -L . examples/gears/gears.scm [--device N] [--novsync] [--frames N]
;;;
;;; Keys: arrows rotate the view, z / Z rotate around the view axis, Esc quits.
;;; Shaders: glslangValidator -V examples/gears/gears.vert -o examples/gears/gears.vert.spv
;;;          glslangValidator -V examples/gears/gears.frag -o examples/gears/gears.frag.spv

(use-modules (vulkan) (vulkan constants) (vulkan xlib)
             (examples common) (examples math) (examples x11)
             (rnrs bytevectors) (srfi srfi-1) (srfi srfi-4) (ice-9 match) (ice-9 format))

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
(define frames-in-flight 2)

;;; ------------------------------------------------------------------
;;; Gear geometry: the classic gear() routine, emitted as triangles of
;;; (x y z nx ny nz).  Faces the original drew flat-shaded get one normal per
;;; triangle; the inner cylinder keeps per-vertex normals.

(define (gear-triangles inner-radius outer-radius width teeth tooth-depth)
  (let* ((r0 inner-radius)
         (r1 (- outer-radius (/ tooth-depth 2.0)))
         (r2 (+ outer-radius (/ tooth-depth 2.0)))
         (da (/ (* 2.0 pi) teeth 4.0))
         (hw (/ width 2.0))
         (out '()))
    (define (angle i) (/ (* i 2.0 pi) teeth))
    (define (p r a z) (list (* r (cos a)) (* r (sin a)) z))
    (define (emit! v n) (set! out (cons (append v n) out)))
    (define (tri! a b c n) (emit! a n) (emit! b n) (emit! c n))
    (define (quad! a b c d n) (tri! a b c n) (tri! a c d n))
    ;; GL quad strip: quad k is v2k v2k+1 v2k+3 v2k+2; flat shading takes the
    ;; normal of v2k+3.  Strip entries are (vertex . normal).
    (define (strip! vs flat?)
      (let loop ((vs vs))
        (when (>= (length vs) 4)
          (match vs
            ((a b d c . _)
             (if flat?
                 (quad! (car a) (car b) (car c) (car d) (cdr c))
                 (begin (emit! (car a) (cdr a)) (emit! (car b) (cdr b)) (emit! (car c) (cdr c))
                        (emit! (car a) (cdr a)) (emit! (car c) (cdr c)) (emit! (car d) (cdr d))))))
          (loop (cddr vs)))))
    (define front '(0.0 0.0 1.0))
    (define back '(0.0 0.0 -1.0))
    ;; front face
    (strip! (append-map (lambda (i)
                          (let ((a (angle i)))
                            (append (list (cons (p r0 a hw) front) (cons (p r1 a hw) front))
                                    (if (< i teeth)
                                        (list (cons (p r0 a hw) front) (cons (p r1 (+ a (* 3 da)) hw) front))
                                        '()))))
                        (iota (+ teeth 1)))
            #t)
    ;; front sides of teeth
    (for-each (lambda (i)
                (let ((a (angle i)))
                  (quad! (p r1 a hw) (p r2 (+ a da) hw) (p r2 (+ a (* 2 da)) hw) (p r1 (+ a (* 3 da)) hw) front)))
              (iota teeth))
    ;; back face
    (strip! (append-map (lambda (i)
                          (let ((a (angle i)))
                            (append (list (cons (p r1 a (- hw)) back) (cons (p r0 a (- hw)) back))
                                    (if (< i teeth)
                                        (list (cons (p r1 (+ a (* 3 da)) (- hw)) back) (cons (p r0 a (- hw)) back))
                                        '()))))
                        (iota (+ teeth 1)))
            #t)
    ;; back sides of teeth
    (for-each (lambda (i)
                (let ((a (angle i)))
                  (quad! (p r1 (+ a (* 3 da)) (- hw)) (p r2 (+ a (* 2 da)) (- hw))
                         (p r2 (+ a da) (- hw)) (p r1 a (- hw)) back)))
              (iota teeth))
    ;; outward faces of teeth
    (let ((pair (lambda (r a n) (list (cons (p r a hw) n) (cons (p r a (- hw)) n))))
          (flank (lambda (r-from a-from r-to a-to)
                   (let* ((u (- (* r-to (cos a-to)) (* r-from (cos a-from))))
                          (v (- (* r-to (sin a-to)) (* r-from (sin a-from))))
                          (len (sqrt (+ (* u u) (* v v)))))
                     (list (/ v len) (/ (- u) len) 0.0)))))
      (strip! (let loop ((i 0) (n back) (acc '()))
                (if (= i teeth)
                    (append acc (pair r1 0.0 n))
                    (let* ((a (angle i))
                           (radial (list (cos a) (sin a) 0.0)))
                      (loop (+ i 1) radial
                            (append acc
                                    (pair r1 a n)
                                    (pair r2 (+ a da) (flank r1 a r2 (+ a da)))
                                    (pair r2 (+ a (* 2 da)) radial)
                                    (pair r1 (+ a (* 3 da)) (flank r2 (+ a (* 2 da)) r1 (+ a (* 3 da)))))))))
              #t))
    ;; inside radius cylinder (smooth shaded)
    (strip! (append-map (lambda (i)
                          (let* ((a (angle i)) (n (list (- (cos a)) (- (sin a)) 0.0)))
                            (list (cons (p r0 a (- hw)) n) (cons (p r0 a hw) n))))
                        (iota (+ teeth 1)))
            #f)
    (reverse out)))

;; color, gear parameters, position, angle = scale * angle + offset
(define gear-specs
  '(((0.8 0.1 0.0 1.0) (1.0 4.0 1.0 20 0.7) (-3.0 -2.0) 1.0 0.0)
    ((0.0 0.8 0.2 1.0) (0.5 2.0 2.0 10 0.7) (3.1 -2.0) -2.0 -9.0)
    ((0.2 0.2 1.0 1.0) (1.3 2.0 0.5 10 0.7) (-3.1 4.2) -2.0 -25.0)))

;;; ------------------------------------------------------------------
;;; Window, instance, surface.

(define window (x11-open-window "guile gears" 300 300))

(define instance
  (create 'vkCreateInstance
          (lambda (out)
            (vk:create-instance
             (vk-new vk:instance-create-info
                     (s-type vk:structure-type-instance-create-info)
                     (p-application-info (vk-new vk:application-info
                                                 (s-type vk:structure-type-application-info)
                                                 (p-application-name "guile gears")
                                                 (application-version 1)
                                                 (api-version vk:api-version-1-3)))
                     (enabled-extension-count 2)
                     (pp-enabled-extension-names (vk-c-string-array '("VK_KHR_surface" "VK_KHR_xlib_surface"))))
             #f out))))

(define surface
  (create 'vkCreateXlibSurfaceKHR
          (lambda (out)
            (vk:create-xlib-surface-khr
             instance
             (vk-new vk:xlib-surface-create-info-khr
                     (s-type vk:structure-type-xlib-surface-create-info-khr)
                     (dpy (x11-window-display window))
                     (window (x11-window-id window)))
             #f out))))

;;; ------------------------------------------------------------------
;;; Physical device: Vulkan 1.3 (dynamic rendering, synchronization2),
;;; VK_KHR_swapchain and a graphics queue that can present to the surface.

(define (device-properties pd)
  (let ((props (make-vk vk:physical-device-properties)))
    (vk:get-physical-device-properties pd props)
    (values (vk-c-string-ref props (vk-offset vk:physical-device-properties (device-name)))
            (vk-ref vk:physical-device-properties (api-version) props))))

(define (has-device-extension? pd name)
  (call-with-values
      (lambda () (enumerate (vk-sizeof vk:extension-properties)
                            (lambda (count items)
                              (vk:enumerate-device-extension-properties pd #f count items))))
    (lambda (n exts)
      (any (lambda (i)
             (string=? name (vk-c-string-ref exts (+ (* i (vk-sizeof vk:extension-properties))
                                                     (vk-offset vk:extension-properties (extension-name))))))
           (iota n)))))

(define (graphics-present-family pd)
  (call-with-values
      (lambda () (enumerate (vk-sizeof vk:queue-family-properties)
                            (lambda (count items)
                              (vk:get-physical-device-queue-family-properties pd count items))))
    (lambda (n families)
      (let ((supported (make-bytevector 4 0)))
        (find (lambda (i)
                (and (logtest (vk-ref vk:queue-family-properties (queue-flags) families i) vk:queue-graphics-bit)
                     (begin (vk:get-physical-device-surface-support-khr pd i surface supported)
                            (= 1 (bytevector-u32-native-ref supported 0)))))
              (iota n))))))

(define (usable-device pd)
  (call-with-values (lambda () (device-properties pd))
    (lambda (name api)
      (and (>= api vk:api-version-1-3)
           (has-device-extension? pd "VK_KHR_swapchain")
           (graphics-present-family pd)))))

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

(call-with-values (lambda () (device-properties physical-device))
  (lambda (name api)
    (format #t "device: ~a (Vulkan ~a.~a)~%" name (vk:api-version-major api) (vk:api-version-minor api))))

(define device
  (let ((priority (make-f32vector 1 1.0)))
    (create 'vkCreateDevice
            (lambda (out)
              (vk:create-device
               physical-device
               (vk-new vk:device-create-info
                       (s-type vk:structure-type-device-create-info)
                       (p-next (vk-new vk:physical-device-vulkan13-features
                                       (s-type vk:structure-type-physical-device-vulkan-1-3-features)
                                       (synchronization2 vk:true)
                                       (dynamic-rendering vk:true)))
                       (queue-create-info-count 1)
                       (p-queue-create-infos (vk-new vk:device-queue-create-info
                                                     (s-type vk:structure-type-device-queue-create-info)
                                                     (queue-family-index queue-family)
                                                     (queue-count 1)
                                                     (p-queue-priorities priority)))
                       (enabled-extension-count 1)
                       (pp-enabled-extension-names (vk-c-string-array '("VK_KHR_swapchain"))))
               #f out)))))

(define queue
  (let ((slot (make-bytevector 8 0)))
    (vk:get-device-queue device queue-family 0 slot)
    (bytevector-u64-native-ref slot 0)))

;;; ------------------------------------------------------------------
;;; Memory.

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
            (vk:allocate-memory
             device
             (vk-new vk:memory-allocate-info
                     (s-type vk:structure-type-memory-allocate-info)
                     (allocation-size (vk-ref vk:memory-requirements (size) reqs))
                     (memory-type-index (find-memory-type (vk-ref vk:memory-requirements (memory-type-bits) reqs)
                                                          properties)))
             #f out))))

;;; ------------------------------------------------------------------
;;; Vertex buffer with all three gears (host-visible; written once).

(define-values (vertex-buffer vertex-memory gear-ranges)
  (let* ((meshes (map (match-lambda ((color params . _) (apply gear-triangles params))) gear-specs))
         (total (apply + (map length meshes)))
         (size (* total 6 4))
         (buffer (create 'vkCreateBuffer
                         (lambda (out)
                           (vk:create-buffer device
                                             (vk-new vk:buffer-create-info
                                                     (s-type vk:structure-type-buffer-create-info)
                                                     (size size)
                                                     (usage vk:buffer-usage-vertex-buffer-bit)
                                                     (sharing-mode vk:sharing-mode-exclusive))
                                             #f out))))
         (reqs (make-vk vk:memory-requirements)))
    (vk:get-buffer-memory-requirements device buffer reqs)
    (let ((memory (allocate-memory reqs (logior vk:memory-property-host-visible-bit
                                                vk:memory-property-host-coherent-bit))))
      (check 'vkBindBufferMemory (vk:bind-buffer-memory device buffer memory 0))
      (let ((mapped (vk-bytevector (create 'vkMapMemory
                                           (lambda (out) (vk:map-memory device memory 0 vk:whole-size 0 out)))
                                   size)))
        (fold (lambda (v i)
                (for-each (lambda (x j) (bytevector-ieee-single-native-set! mapped (* 4 (+ (* 6 i) j)) x))
                          v (iota 6))
                (+ i 1))
              0 (concatenate meshes))
        (vk:unmap-memory device memory)
        (values buffer memory
                (reverse (fold (lambda (mesh acc)
                                 (let ((first (if (null? acc) 0 (+ (caar acc) (cdar acc)))))
                                   (cons (cons first (length mesh)) acc)))
                               '() meshes)))))))

(format #t "~a triangles~%" (/ (apply + (map cdr gear-ranges)) 3))

;;; ------------------------------------------------------------------
;;; Surface format, depth format, present mode.

(define-values (surface-format color-space)
  (call-with-values
      (lambda () (enumerate (vk-sizeof vk:surface-format-khr)
                            (lambda (count items)
                              (vk:get-physical-device-surface-formats-khr physical-device surface count items))))
    (lambda (n formats)
      ;; UNORM, not SRGB: the classic demo's colors were written without gamma.
      (let ((i (or (find (lambda (i) (memv (vk-ref vk:surface-format-khr (format) formats i)
                                           (list vk:format-b8g8r8a8-unorm vk:format-r8g8b8a8-unorm)))
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

(define push-constant-size 128)           ; mat4 mvp, mat3 normal matrix, vec4 color

(define pipeline-layout
  (create 'vkCreatePipelineLayout
          (lambda (out)
            (vk:create-pipeline-layout
             device
             (vk-new vk:pipeline-layout-create-info
                     (s-type vk:structure-type-pipeline-layout-create-info)
                     (push-constant-range-count 1)
                     (p-push-constant-ranges (vk-new vk:push-constant-range
                                                     (stage-flags vk:shader-stage-vertex-bit)
                                                     (offset 0)
                                                     (size push-constant-size))))
             #f out))))

(define pipeline
  (let* ((vert (shader-module "examples/gears/gears.vert.spv"))
         (frag (shader-module "examples/gears/gears.frag.spv"))
         (stages (make-vk vk:pipeline-shader-stage-create-info 2))
         (attributes (make-vk vk:vertex-input-attribute-description 2))
         (color-format (make-bytevector 4 0))
         (dynamic-states (make-bytevector 8 0)))
    (for-each (lambda (i stage module)
                (vk-set! vk:pipeline-shader-stage-create-info (s-type) stages i
                         vk:structure-type-pipeline-shader-stage-create-info)
                (vk-set! vk:pipeline-shader-stage-create-info (stage) stages i stage)
                (vk-set! vk:pipeline-shader-stage-create-info (module) stages i module)
                (vk-set! vk:pipeline-shader-stage-create-info (p-name) stages i "main"))
              '(0 1) (list vk:shader-stage-vertex-bit vk:shader-stage-fragment-bit) (list vert frag))
    (for-each (lambda (i offset)
                (vk-set! vk:vertex-input-attribute-description (location) attributes i i)
                (vk-set! vk:vertex-input-attribute-description (format) attributes i vk:format-r32g32b32-sfloat)
                (vk-set! vk:vertex-input-attribute-description (offset) attributes i offset))
              '(0 1) '(0 12))
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
                                           (binding 0) (stride 24) (input-rate vk:vertex-input-rate-vertex)))
                                  (vertex-attribute-description-count 2)
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

(define (create-view image format aspect)
  (create 'vkCreateImageView
          (lambda (out)
            (vk:create-image-view device
                                  (vk-new vk:image-view-create-info
                                          (s-type vk:structure-type-image-view-create-info)
                                          (image image)
                                          (view-type vk:image-view-type-2d)
                                          (format format)
                                          (subresource-range aspect-mask aspect)
                                          (subresource-range level-count 1)
                                          (subresource-range layer-count 1))
                                  #f out))))

(define (destroy-swapchain-resources)
  (for-each (lambda (v) (vk:destroy-image-view device v #f)) (vector->list swapchain-views))
  (for-each (lambda (s) (vk:destroy-semaphore device s #f)) (vector->list render-done))
  (unless (zero? depth-view)
    (vk:destroy-image-view device depth-view #f)
    (vk:destroy-image device depth-image #f)
    (vk:free-memory device depth-memory #f)
    (set! depth-view 0)))

;; Returns #f when the window has no area (minimized), #t otherwise.
(define (create-swapchain!)
  (let ((caps (make-vk vk:surface-capabilities-khr)))
    (check 'vkGetPhysicalDeviceSurfaceCapabilitiesKHR
           (vk:get-physical-device-surface-capabilities-khr physical-device surface caps))
    (let* ((current-w (vk-ref vk:surface-capabilities-khr (current-extent width) caps))
           (clamp (lambda (v lo hi) (max lo (min hi v))))
           ;; #xFFFFFFFF: the surface takes its size from the swapchain.
           (w (if (= current-w #xFFFFFFFF)
                  (clamp (car (x11-window-size window))
                         (vk-ref vk:surface-capabilities-khr (min-image-extent width) caps)
                         (vk-ref vk:surface-capabilities-khr (max-image-extent width) caps))
                  current-w))
           (h (if (= current-w #xFFFFFFFF)
                  (clamp (cdr (x11-window-size window))
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
             (set! swapchain-images
                   (call-with-values
                       (lambda () (enumerate 8 (lambda (count items)
                                                 (vk:get-swapchain-images-khr device swapchain count items))))
                     (lambda (n images)
                       (list->vector (map (lambda (i) (bytevector-u64-native-ref images (* 8 i))) (iota n))))))
             (set! swapchain-views (list->vector (map (lambda (img) (create-view img surface-format vk:image-aspect-color-bit)) (vector->list swapchain-images))))
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
             (set! depth-view (create-view depth-image depth-format vk:image-aspect-depth-bit))
             #t)))))

;;; ------------------------------------------------------------------
;;; Per-frame objects, and structures reused every frame.

(define command-pool
  (create 'vkCreateCommandPool
          (lambda (out)
            (vk:create-command-pool device
                                    (vk-new vk:command-pool-create-info
                                            (s-type vk:structure-type-command-pool-create-info)
                                            (flags vk:command-pool-create-reset-command-buffer-bit)
                                            (queue-family-index queue-family))
                                    #f out))))

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

;; [0] swapchain image -> color attachment, [1] depth -> depth attachment
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
                                 (store-op vk:attachment-store-op-store)))
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

(define view-rot-x 20.0)
(define view-rot-y 30.0)
(define view-rot-z 0.0)
(define angle 0.0)

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
  ;; Flipped viewport: y up, as in OpenGL.
  (vk-set! vk:viewport (y) viewport (exact->inexact extent-height))
  (vk-set! vk:viewport (width) viewport (exact->inexact extent-width))
  (vk-set! vk:viewport (height) viewport (exact->inexact (- extent-height)))
  (vk:cmd-set-viewport cmd 0 1 viewport)
  (vk-set! vk:rect2-d (extent width) scissor extent-width)
  (vk-set! vk:rect2-d (extent height) scissor extent-height)
  (vk:cmd-set-scissor cmd 0 1 scissor)
  (vk:cmd-bind-pipeline cmd vk:pipeline-bind-point-graphics pipeline)
  (vk:cmd-bind-vertex-buffers cmd 0 1 vertex-buffers vertex-offsets)
  (let* ((h (/ (exact->inexact extent-height) extent-width))
         (projection (frustum -1.0 1.0 (- h) h 5.0 60.0))
         (view (mat* (translate 0 0 -40)
                     (mat* (rotate-x view-rot-x) (mat* (rotate-y view-rot-y) (rotate-z view-rot-z))))))
    (for-each
     (match-lambda*
       (((color params (x y) scale offset) (first . count))
        (let ((modelview (mat* view (mat* (translate x y 0) (rotate-z (+ (* scale angle) offset))))))
          (write-matrix! push-constants 0 (mat* projection modelview))
          (write-normal-matrix! push-constants 64 modelview)
          (for-each (lambda (c i) (bytevector-ieee-single-native-set! push-constants (+ 112 (* 4 i)) c))
                    color (iota 4))
          (vk:cmd-push-constants cmd pipeline-layout vk:shader-stage-vertex-bit 0 push-constant-size push-constants)
          (vk:cmd-draw cmd count 1 first 0))))
     gear-specs gear-ranges))
  (vk:cmd-end-rendering cmd)
  (vk:cmd-pipeline-barrier2 cmd to-present-dependency)
  (check 'vkEndCommandBuffer (vk:end-command-buffer cmd)))

(define swapchain-stale? (not (create-swapchain!)))

;; Draw one frame.  Returns #t if a frame was drawn, #f to retry at once
;; (the swapchain was out of date), or 'idle when no swapchain can be made
;; (e.g. the window has no area).
(define (draw-frame! frame)
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

(define running #t)

(define (on-key keysym)
  (cond ((= keysym xk-escape) (set! running #f))
        ((= keysym xk-left) (set! view-rot-y (+ view-rot-y 5.0)))
        ((= keysym xk-right) (set! view-rot-y (- view-rot-y 5.0)))
        ((= keysym xk-up) (set! view-rot-x (+ view-rot-x 5.0)))
        ((= keysym xk-down) (set! view-rot-x (- view-rot-x 5.0)))
        ((= keysym (char->integer #\z)) (set! view-rot-z (+ view-rot-z 5.0)))
        ((= keysym (char->integer #\Z)) (set! view-rot-z (- view-rot-z 5.0)))))

(define (on-resize width height) (set! swapchain-stale? #t))

(let loop ((frame 0) (frames 0) (total-frames 0) (last (now)) (report-start (now)))
  (when (and running (x11-poll-events window on-key on-resize)
             (not (and frame-limit (>= total-frames frame-limit))))
    (let* ((t (now)) (dt (- t last)))
      (set! angle (let ((a (+ angle (* 70.0 dt)))) (if (> a 3600.0) (- a 3600.0) a)))
      (let* ((result (draw-frame! frame))
             (drawn (eq? result #t))
             (frames (if drawn (+ frames 1) frames))
             (total-frames (if drawn (+ total-frames 1) total-frames))
             (next (if drawn (modulo (+ frame 1) frames-in-flight) frame)))
        (when (eq? result 'idle) (usleep 50000))   ; nothing to draw into: don't spin
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
(vk:destroy-command-pool device command-pool #f)
(destroy-swapchain-resources)
(unless (zero? swapchain) (vk:destroy-swapchain-khr device swapchain #f))
(vk:destroy-pipeline device pipeline #f)
(vk:destroy-pipeline-layout device pipeline-layout #f)
(vk:destroy-buffer device vertex-buffer #f)
(vk:free-memory device vertex-memory #f)
(vk:destroy-device device #f)
(vk:destroy-surface-khr instance surface #f)
(vk:destroy-instance instance #f)
(x11-close-window window)
