;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; (vulkan xcb) test: open a 320x240 XCB window, create a Vulkan surface on
;;; it and check that the surface has the window's size and that the device
;;; can present to it.  Needs an X server (e.g. xvfb-run) and libxcb.
;;;
;;;   guile -L . tests/xcb-surface.scm

(use-modules (vulkan) (vulkan constants) (vulkan xcb)
             (system foreign) (system foreign-library) (rnrs bytevectors))

(define (fail . args) (apply format #t args) (newline) (exit 1))

(define libxcb (load-foreign-library "libxcb.so.1"))
(define (xcb name ret args)
  (pointer->procedure ret (foreign-library-pointer libxcb name) args))

(define xcb-connect (xcb "xcb_connect" uintptr_t (list '* '*)))
(define xcb-connection-has-error (xcb "xcb_connection_has_error" int (list uintptr_t)))
(define xcb-get-setup (xcb "xcb_get_setup" uintptr_t (list uintptr_t)))
;; xcb_screen_iterator_t {xcb_screen_t *data; int rem; int index;}, returned by
;; value; Guile hands back a pointer to it.
(define xcb-setup-roots-iterator (xcb "xcb_setup_roots_iterator" (list uintptr_t int int) (list uintptr_t)))
(define xcb-generate-id (xcb "xcb_generate_id" uint32 (list uintptr_t)))
(define xcb-create-window
  (xcb "xcb_create_window" uint32
       (list uintptr_t uint8 uint32 uint32 int16 int16 uint16 uint16 uint16 uint16 uint32 uint32 '*)))
(define xcb-map-window (xcb "xcb_map_window" uint32 (list uintptr_t uint32)))
(define xcb-destroy-window (xcb "xcb_destroy_window" uint32 (list uintptr_t uint32)))
(define xcb-flush (xcb "xcb_flush" int (list uintptr_t)))
(define xcb-disconnect (xcb "xcb_disconnect" void (list uintptr_t)))

(define conn (xcb-connect %null-pointer %null-pointer))
(unless (zero? (xcb-connection-has-error conn)) (fail "cannot connect to the X server"))

;; xcb_screen_t: root at 0, root_visual at 32.
(define screen
  (let ((iterator (parse-c-struct (xcb-setup-roots-iterator (xcb-get-setup conn))
                                  (list uintptr_t int int))))
    (vk-bytevector (car iterator) 40)))
(define root (bytevector-u32-native-ref screen 0))
(define root-visual (bytevector-u32-native-ref screen 32))
(define window (xcb-generate-id conn))
(xcb-create-window conn 0 window root 0 0 320 240 0
                   1 root-visual 0 %null-pointer)  ; XCB_WINDOW_CLASS_INPUT_OUTPUT
(xcb-map-window conn window)
(xcb-flush conn)

(define (check who result)
  (unless (= result vk:success) (fail "~a failed: VkResult ~a" who result)))
(define (create who f)
  (let ((slot (make-bytevector 8 0)))
    (check who (f slot))
    (bytevector-u64-native-ref slot 0)))

(define instance
  (create 'vkCreateInstance
          (lambda (out)
            (vk:create-instance
             (vk-new vk:instance-create-info
                     (s-type vk:structure-type-instance-create-info)
                     (p-application-info (vk-new vk:application-info
                                                 (s-type vk:structure-type-application-info)
                                                 (api-version vk:api-version-1-0)))
                     (enabled-extension-count 2)
                     (pp-enabled-extension-names
                      (vk-c-string-array '("VK_KHR_surface" "VK_KHR_xcb_surface"))))
             #f out))))

(define physical-device
  (let ((count (make-bytevector 4 0)) (devices (make-bytevector 8 0)))
    (bytevector-u32-native-set! count 0 1)
    (vk:enumerate-physical-devices instance count devices)   ; VK_INCOMPLETE is fine: one is enough
    (when (zero? (bytevector-u32-native-ref count 0)) (fail "no Vulkan device"))
    (bytevector-u64-native-ref devices 0)))

(define surface
  (create 'vkCreateXcbSurfaceKHR
          (lambda (out)
            (vk:create-xcb-surface-khr
             instance
             (vk-new vk:xcb-surface-create-info-khr
                     (s-type vk:structure-type-xcb-surface-create-info-khr)
                     (connection conn)
                     (window window))
             #f out))))

(define caps (make-vk vk:surface-capabilities-khr))
(check 'vkGetPhysicalDeviceSurfaceCapabilitiesKHR
       (vk:get-physical-device-surface-capabilities-khr physical-device surface caps))
(define supported (make-bytevector 4 0))
(check 'vkGetPhysicalDeviceSurfaceSupportKHR
       (vk:get-physical-device-surface-support-khr physical-device 0 surface supported))
(define width (vk-ref vk:surface-capabilities-khr (current-extent width) caps))
(define height (vk-ref vk:surface-capabilities-khr (current-extent height) caps))
(define surface-support (bytevector-u32-native-ref supported 0))
(define presentable
  (vk:get-physical-device-xcb-presentation-support-khr physical-device 0 conn root-visual))
(format #t "xcb surface: ~ax~a, surface support ~a, presentation support ~a~%"
        width height surface-support presentable)

(vk:destroy-surface-khr instance surface #f)
(vk:destroy-instance instance #f)
(xcb-destroy-window conn window)
(xcb-disconnect conn)

(unless (and (= width 320) (= height 240) (= surface-support 1) (= presentable 1))
  (fail "FAIL: expected a 320x240 surface the device can present to"))
(format #t "ok~%")
