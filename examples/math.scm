;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; 4x4 matrices for the examples: column-major f32vectors (the layout GLSL
;;; expects; f32vectors are bytevectors, so they copy straight into push
;;; constants).  Element (row r, column c) is at index 4c+r.  Projections use
;;; Vulkan's 0..1 depth range; the examples flip the viewport (negative
;;; height) so Y points up as in OpenGL.

(define-module (examples math)
  #:use-module (srfi srfi-4)
  #:use-module (rnrs bytevectors)
  #:export (pi radians mat mat* translate rotate-x rotate-y rotate-z
            frustum perspective write-matrix! write-normal-matrix!))

(define pi (* 4 (atan 1)))
(define (radians deg) (/ (* deg pi) 180.0))

(define (mat . rows)
  "A matrix from 16 numbers, given row by row."
  (let ((m (make-f32vector 16 0.0)))
    (do ((r 0 (+ r 1))) ((= r 4) m)
      (do ((c 0 (+ c 1))) ((= c 4))
        (f32vector-set! m (+ (* 4 c) r) (exact->inexact (list-ref rows (+ (* 4 r) c))))))))

(define (mat* a b)
  (let ((m (make-f32vector 16 0.0)))
    (do ((c 0 (+ c 1))) ((= c 4) m)
      (do ((r 0 (+ r 1))) ((= r 4))
        (let loop ((k 0) (sum 0.0))
          (if (= k 4)
              (f32vector-set! m (+ (* 4 c) r) sum)
              (loop (+ k 1) (+ sum (* (f32vector-ref a (+ (* 4 k) r))
                                      (f32vector-ref b (+ (* 4 c) k)))))))))))

(define (translate x y z) (mat 1 0 0 x  0 1 0 y  0 0 1 z  0 0 0 1))

(define (rotate-x deg)
  (let ((c (cos (radians deg))) (s (sin (radians deg))))
    (mat 1 0 0 0  0 c (- s) 0  0 s c 0  0 0 0 1)))
(define (rotate-y deg)
  (let ((c (cos (radians deg))) (s (sin (radians deg))))
    (mat c 0 s 0  0 1 0 0  (- s) 0 c 0  0 0 0 1)))
(define (rotate-z deg)
  (let ((c (cos (radians deg))) (s (sin (radians deg))))
    (mat c (- s) 0 0  s c 0 0  0 0 1 0  0 0 0 1)))

(define (frustum l r b t n f)
  "glFrustum with a 0..1 depth range."
  (mat (/ (* 2 n) (- r l)) 0 (/ (+ r l) (- r l)) 0
       0 (/ (* 2 n) (- t b)) (/ (+ t b) (- t b)) 0
       0 0 (/ f (- n f)) (/ (* n f) (- n f))
       0 0 -1 0))

(define (perspective fov-y aspect n f)
  "gluPerspective (vertical field of view in degrees) with a 0..1 depth range."
  (let ((t (* n (tan (/ (radians fov-y) 2)))))
    (frustum (- (* t aspect)) (* t aspect) (- t) t n f)))

(define (write-matrix! bv offset m)
  "Store M as 16 floats in BV at OFFSET."
  (bytevector-copy! m 0 bv offset 64))

(define (write-normal-matrix! bv offset m)
  "Store the upper-left 3x3 of M as a GLSL mat3 (three columns, each padded
to a vec4: 48 bytes); for rotations and translations, the normal matrix."
  (do ((c 0 (+ c 1))) ((= c 3))
    (bytevector-copy! m (* 16 c) bv (+ offset (* 16 c)) 12)))
