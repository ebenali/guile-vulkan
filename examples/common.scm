;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; Helpers shared by the examples.

(define-module (examples common)
  #:use-module (vulkan)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:export (check create handle-array enumerate read-file-bytevector max-u64))

(define max-u64 #xFFFFFFFFFFFFFFFF)

(define (check who result)
  "Signal an error unless RESULT is VK_SUCCESS."
  (unless (= result vk:success)
    (error (format #f "~a failed with VkResult ~a" who result))))

(define (create who f)
  "Call F with an 8-byte slot, check its result, return the handle written."
  (let ((slot (make-bytevector 8 0)))
    (check who (f slot))
    (bytevector-u64-native-ref slot 0)))

(define (handle-array . handles)
  "A bytevector of 64-bit handles, for pointer-to-handles arguments and fields."
  (let ((bv (make-bytevector (* 8 (max 1 (length handles))) 0)))
    (let loop ((hs handles) (i 0))
      (unless (null? hs)
        (bytevector-u64-native-set! bv (* 8 i) (car hs))
        (loop (cdr hs) (+ i 1))))
    bv))

(define (enumerate element-size f)
  "Vulkan's two-call enumeration.  F takes (count-slot items-or-#f) and may
return a VkResult (checked).  Starts over if the count grew in between
(VK_INCOMPLETE).  Returns (values count items-bytevector)."
  (let ((count (make-bytevector 4 0)))
    (let retry ()
      (let ((r (f count #f)))
        (when (number? r) (check 'enumerate r)))
      (let* ((items (make-bytevector (* element-size (max 1 (bytevector-u32-native-ref count 0))) 0))
             (r (f count items)))
        (cond ((and (number? r) (= r vk:incomplete)) (retry))
              (else (when (number? r) (check 'enumerate r))
                    (values (bytevector-u32-native-ref count 0) items)))))))

(define (read-file-bytevector path)
  (call-with-input-file path get-bytevector-all #:binary #t))
