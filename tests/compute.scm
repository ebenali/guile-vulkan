;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; (vulkan compute) test: every operation against a plain-Guile reference,
;;; at sizes that do not fit the shaders' tiles and workgroups evenly.
;;;
;;;   guile -L . tests/compute.scm [device-index]

(use-modules (vulkan compute) (srfi srfi-1) (srfi srfi-4) (ice-9 match) (ice-9 format))

(define context
  (match (cdr (command-line))
    ((index) (make-compute-context #:device (string->number index)))
    (_ (make-compute-context))))
(current-compute-context context)
(format #t "device: ~a~%" (compute-context-device-name context))

(define state (seed->random-state 42))
(define (random-list n) (map (lambda (_) (- (random 2.0 state) 1.0)) (iota n)))

(define failures 0)
(define (close? x y) (<= (abs (- x y)) (+ 1e-5 (* 1e-4 (max (abs x) (abs y))))))
(define (test name got expected)
  (let ((ok (and (= (length got) (length expected)) (every close? got expected))))
    (unless ok
      (set! failures (+ failures 1))
      (let ((bad (find (lambda (i) (not (close? (list-ref got i) (list-ref expected i))))
                       (iota (min (length got) (length expected))))))
        (format #t "FAIL ~a: ~a values vs ~a expected~@[, first difference at ~a~]~%"
                name (length got) (length expected) bad)))
    (when ok (format #t "ok   ~a~%" name))))

;;; Plain-Guile references on row-major lists.

(define (ref m r c) (lambda (i j) (list-ref m (+ (* i c) j))))
(define (matmul a b m k n trans-a trans-b alpha)
  ;; op(A) is m x k, op(B) is k x n.
  (let ((av (list->vector a)) (bv (list->vector b)))
    (append-map
     (lambda (i)
       (map (lambda (j)
              (* alpha
                 (fold (lambda (p acc)
                         (+ acc (* (vector-ref av (if trans-a (+ (* p m) i) (+ (* i k) p)))
                                   (vector-ref bv (if trans-b (+ (* j k) p) (+ (* p n) j))))))
                       0 (iota k))))
            (iota n)))
     (iota m))))
(define (rows lst cols) (if (null? lst) '() (cons (list-head lst cols) (rows (list-tail lst cols) cols))))
(define (softmax-row row)
  (let* ((m (apply max row)) (e (map (lambda (x) (exp (- x m))) row)) (s (apply + e)))
    (map (lambda (x) (/ x s)) e)))
(define (sigmoid x) (/ 1 (+ 1 (exp (- x)))))

;;; matmul

(for-each
 (match-lambda
   ((m k n)
    (for-each
     (match-lambda
       ((ta tb)
        (let* ((a (random-list (* m k))) (b (random-list (* k n)))
               (ta-shape (if ta (list k m) (list m k)))
               (tb-shape (if tb (list n k) (list k n))))
          (test (format #f "matmul ~ax~ax~a~:[~; A^T~]~:[~; B^T~]" m k n ta tb)
                (tensor->list (tensor-matmul (list->tensor a ta-shape) (list->tensor b tb-shape)
                                             #:transpose-a? ta #:transpose-b? tb #:alpha 0.5))
                (matmul a b m k n ta tb 0.5)))))
     '((#f #f) (#t #f) (#f #t) (#t #t)))))
 '((37 53 29) (1 1 1) (16 16 16) (100 3 7)))

;;; elementwise

(let* ((a (random-list (* 13 300))) (b (random-list (* 13 300))) (row (random-list 300))
       (ta (list->tensor a '(13 300))) (tb (list->tensor b '(13 300))) (trow (list->tensor row)))
  (test "add" (tensor->list (tensor-add ta tb)) (map + a b))
  (test "sub" (tensor->list (tensor-sub ta tb)) (map - a b))
  (test "mul" (tensor->list (tensor-mul ta tb)) (map * a b))
  (test "div" (tensor->list (tensor-div ta (tensor-add-scalar tb 3.0))) (map (lambda (x y) (/ x (+ y 3))) a b))
  (test "add row (bias)" (tensor->list (tensor-add ta trow))
        (append-map (lambda (r) (map + r row)) (rows a 300)))
  (test "relu-backward" (tensor->list (tensor-relu-backward ta tb)) (map (lambda (x g) (if (> x 0) g 0)) a b))
  (test "relu" (tensor->list (tensor-relu ta)) (map (lambda (x) (max x 0)) a))
  (test "scale" (tensor->list (tensor-scale ta -2.5)) (map (lambda (x) (* x -2.5)) a))
  (test "add-scalar" (tensor->list (tensor-add-scalar ta 0.25)) (map (lambda (x) (+ x 0.25)) a))
  (test "exp" (tensor->list (tensor-exp ta)) (map exp a))
  (test "tanh" (tensor->list (tensor-tanh ta)) (map tanh a))
  (test "sigmoid" (tensor->list (tensor-sigmoid ta)) (map sigmoid a))
  (test "copy" (tensor->list (tensor-copy ta)) a))

;;; reductions

(let ((a (random-list (* 7 300))))
  (test "softmax-rows 7x300" (tensor->list (tensor-softmax-rows (list->tensor (map (lambda (x) (* 8 x)) a) '(7 300))))
        (append-map softmax-row (rows (map (lambda (x) (* 8 x)) a) 300))))
(let ((a (random-list (* 513 37))))
  (test "sum-rows 513x37" (tensor->list (tensor-sum-rows (list->tensor a '(513 37))))
        (fold (lambda (r acc) (map + r acc)) (make-list 37 0) (rows a 37))))

;;; tensors

(test "make-tensor fill" (tensor->list (make-tensor '(3 5) #:fill 1.5)) (make-list 15 1.5))
(let ((a (random-list 12)))
  (test "reshape (view)" (tensor->list (tensor-sum-rows (tensor-reshape (list->tensor a) '(4 3))))
        (fold (lambda (r acc) (map + r acc)) '(0 0 0) (rows a 3))))

;; 3000 dispatches recorded before one read: more than the descriptor pool
;; holds, so it flushes on the way.
(test "3000 chained operations"
      (tensor->list (fold (lambda (_ t) (tensor-add-scalar t 1.0)) (make-tensor '(5)) (iota 3000)))
      (make-list 5 3000.0))

;; The same with garbage collections in between: tensors that become
;; unreachable mid-chain must not be freed before the GPU is done with them.
(test "chained operations with GC pressure"
      (tensor->list (fold (lambda (i t)
                            (when (zero? (modulo i 40)) (gc))
                            (tensor-add (tensor-add-scalar t 1.0) (make-tensor '(5))))
                          (make-tensor '(5)) (iota 2500)))
      (make-list 5 2500.0))

;; 200 temporaries of 64 MB each, never freed by hand: 12.8 GB in all, more
;; than any GPU here.  The forced collections must reclaim them.
(test "200 x 64 MB temporaries"
      (tensor->list (fold (lambda (i t)
                            (make-tensor (list (* 16 1024 1024)) #:fill (exact->inexact i))
                            (tensor-add-scalar t 1.0))
                          (make-tensor '(3)) (iota 200)))
      (make-list 3 200.0))

;; Thousands of small tensors on a device allowing only 4096 live
;; allocations (common on Windows).  make-tensor records no descriptor sets,
;; so nothing else flushes: only the allocation count can trigger the
;; collections that keep it under the limit.
(let* ((limited (make-compute-context #:device (and (pair? (cdr (command-line)))
                                                     (string->number (cadr (command-line))))
                                       #:max-allocations 4096))
       (peak 0))
  (parameterize ((current-compute-context limited))
    (for-each (lambda (i)
                (make-tensor '(4) #:context limited)
                (set! peak (max peak (compute-context-allocation-count limited))))
              (iota 6000))
    (test "6000 small tensors under a 4096-allocation limit"
          (tensor->list (make-tensor '(2) #:fill 7.0 #:context limited)) '(7.0 7.0)))
  (unless (<= peak 4096)
    (set! failures (+ failures 1))
    (format #t "FAIL   live allocations peaked at ~a~%" peak))
  (compute-context-destroy! limited))

;; More elements than 65535 workgroups of 256: the grid-stride loops.
(let* ((n 20000000)
       (v (tensor->f32vector (tensor-add-scalar (make-tensor (list n) #:fill 2.0) 1.0))))
  (test "20M elements" (list (f32vector-ref v 0) (f32vector-ref v (quotient n 2)) (f32vector-ref v (- n 1)))
        '(3.0 3.0 3.0)))

(compute-context-destroy! context)
(format #t "~a~%" (if (zero? failures) "all passed" (format #f "~a FAILED" failures)))
(exit (if (zero? failures) 0 1))
