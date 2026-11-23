;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; (vulkan wayland) test: connect to the Wayland display and check that the
;;; device can present to it.  Exits with 77 (skipped) without a Wayland
;;; display.
;;;
;;;   guile -L . tests/wayland-presentation.scm

(use-modules (vulkan) (vulkan constants) (vulkan wayland)
             (system foreign) (system foreign-library) (rnrs bytevectors))

(define (fail . args) (apply format #t args) (newline) (exit 1))

(define libwayland (load-foreign-library "libwayland-client.so.0"))
(define wl-display-connect
  (pointer->procedure uintptr_t (foreign-library-pointer libwayland "wl_display_connect") (list '*)))
(define wl-display-disconnect
  (pointer->procedure void (foreign-library-pointer libwayland "wl_display_disconnect") (list uintptr_t)))

(define display (wl-display-connect %null-pointer))
(when (zero? display)
  (format #t "skipped: no Wayland display~%")
  (exit 77))

(define instance
  (let ((slot (make-bytevector 8 0)))
    (unless (= vk:success
               (vk:create-instance
                (vk-new vk:instance-create-info
                        (s-type vk:structure-type-instance-create-info)
                        (enabled-extension-count 2)
                        (pp-enabled-extension-names
                         (vk-c-string-array '("VK_KHR_surface" "VK_KHR_wayland_surface"))))
                #f slot))
      (fail "vkCreateInstance failed"))
    (bytevector-u64-native-ref slot 0)))

(define physical-device
  (let ((count (make-bytevector 4 0)) (devices (make-bytevector 8 0)))
    (bytevector-u32-native-set! count 0 1)
    (vk:enumerate-physical-devices instance count devices)
    (when (zero? (bytevector-u32-native-ref count 0)) (fail "no Vulkan device"))
    (bytevector-u64-native-ref devices 0)))

(define presentable
  (vk:get-physical-device-wayland-presentation-support-khr physical-device 0 display))
(format #t "wayland: presentation support ~a~%" presentable)

(vk:destroy-instance instance #f)
(wl-display-disconnect display)

(unless (= presentable 1) (fail "FAIL: the device cannot present to this Wayland display"))
(format #t "ok~%")
