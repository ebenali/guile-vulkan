;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; Spirals: trains a small neural network on the GPU with (vulkan compute)
;;; to tell three interleaved spirals apart, then draws what it learned.
;;; Run from the repository root:
;;;
;;;   guile -L . examples/spirals/spirals.scm [--device N] [--steps N] [--output FILE]
;;;
;;; The network is 2 -> 64 -> 64 -> 3 with ReLU and softmax, trained by
;;; full-batch gradient descent on the cross-entropy loss.  The forward pass,
;;; backpropagation and updates all run on the GPU; only the loss reports and
;;; the final picture are read back.  The picture (a PPM, FILE, by default
;;; spirals.ppm) shows each point of the plane colored by the class the
;;; network predicts, with the training points on top.

(use-modules (vulkan compute) (srfi srfi-1) (srfi srfi-4) (ice-9 match) (ice-9 format)
             (ice-9 binary-ports) (rnrs bytevectors))

;;; ------------------------------------------------------------------
;;; Options.

(define (option name convert default)
  (let loop ((args (cdr (command-line))))
    (match args
      (() default)
      (((? (lambda (a) (string=? a name))) value . _) (convert value))
      ((_ . rest) (loop rest)))))

(define device (option "--device" string->number #f))
(define steps (option "--steps" string->number 3000))
(define output (option "--output" identity "spirals.ppm"))

(current-compute-context (make-compute-context #:device device))
(format #t "device: ~a~%" (compute-context-device-name (current-compute-context)))

;;; ------------------------------------------------------------------
;;; Data: three spirals of 300 points (as in the CS231n notes).

(define classes 3)
(define per-class 300)
(define n (* classes per-class))
(define state (seed->random-state 1))

(define points                           ; ((x y class) ...)
  (append-map
   (lambda (c)
     (map (lambda (i)
            (let* ((r (/ i (- per-class 1.0)))
                   (t (+ (* c 4.0) (* 4.0 r) (* 0.2 (random:normal state)))))
              (list (* r (sin t)) (* r (cos t)) c)))
          (iota per-class)))
   (iota classes)))

(define x (list->tensor (append-map (match-lambda ((px py _) (list px py))) points) (list n 2)))
(define y (list->tensor (append-map (match-lambda ((_ _ c) (map (lambda (k) (if (= k c) 1.0 0.0)) (iota classes))))
                                    points)
                        (list n classes)))

;;; ------------------------------------------------------------------
;;; The network.

(define hidden 64)

(define (he-init rows cols)              ; weights ~ N(0, 2/rows)
  (list->tensor (map (lambda (_) (* (sqrt (/ 2.0 rows)) (random:normal state))) (iota (* rows cols)))
                (list rows cols)))

(define w1 (he-init 2 hidden))
(define b1 (make-tensor (list hidden)))
(define w2 (he-init hidden hidden))
(define b2 (make-tensor (list hidden)))
(define w3 (he-init hidden classes))
(define b3 (make-tensor (list classes)))

(define (forward input)
  "=> (values h1 h2 probabilities)"
  (let* ((h1 (tensor-relu (tensor-add (tensor-matmul input w1) b1)))
         (h2 (tensor-relu (tensor-add (tensor-matmul h1 w2) b2)))
         (p (tensor-softmax-rows (tensor-add (tensor-matmul h2 w3) b3))))
    (values h1 h2 p)))

(define learning-rate 0.5)
(define l2 1e-3)

;; One step of gradient descent on the whole batch.  The output of the
;; last call is the batch's predicted probabilities.
(define (train-step!)
  (call-with-values (lambda () (forward x))
    (lambda (h1 h2 p)
      (let* ((d-scores (tensor-scale (tensor-sub p y) (/ 1.0 n)))      ; d loss / d scores
             (d-w3 (tensor-add (tensor-matmul h2 d-scores #:transpose-a? #t) (tensor-scale w3 l2)))
             (d-b3 (tensor-sum-rows d-scores))
             (d-h2 (tensor-relu-backward h2 (tensor-matmul d-scores w3 #:transpose-b? #t)))
             (d-w2 (tensor-add (tensor-matmul h1 d-h2 #:transpose-a? #t) (tensor-scale w2 l2)))
             (d-b2 (tensor-sum-rows d-h2))
             (d-h1 (tensor-relu-backward h1 (tensor-matmul d-h2 w2 #:transpose-b? #t)))
             (d-w1 (tensor-add (tensor-matmul x d-h1 #:transpose-a? #t) (tensor-scale w1 l2)))
             (d-b1 (tensor-sum-rows d-h1))
             (step (lambda (param grad) (tensor-sub param (tensor-scale grad learning-rate)))))
        (set! w1 (step w1 d-w1)) (set! b1 (step b1 d-b1))
        (set! w2 (step w2 d-w2)) (set! b2 (step b2 d-b2))
        (set! w3 (step w3 d-w3)) (set! b3 (step b3 d-b3))
        p))))

;; Cross-entropy loss and accuracy of probabilities P (read back).
(define (loss-and-accuracy p)
  (let ((probs (tensor->f32vector p)))
    (let loop ((points points) (i 0) (loss 0.0) (right 0))
      (match points
        (() (values (/ loss n) (/ right (exact->inexact n))))
        (((_ _ c) . rest)
         (let* ((row (map (lambda (k) (f32vector-ref probs (+ (* i classes) k))) (iota classes)))
                (best (list-index (lambda (v) (= v (apply max row))) row)))
           (loop rest (+ i 1)
                 (- loss (log (max 1e-30 (list-ref row c))))
                 (if (= best c) (+ right 1) right))))))))

;;; ------------------------------------------------------------------
;;; Training.

(define (now) (/ (get-internal-real-time) (exact->inexact internal-time-units-per-second)))

(define start (now))
(let loop ((i 1))
  (when (<= i steps)
    (let ((p (train-step!)))
      (when (or (= i 1) (zero? (modulo i 500)) (= i steps))
        (call-with-values (lambda () (loss-and-accuracy p))
          (lambda (loss accuracy)
            (format #t "step ~5d  loss ~,4f  accuracy ~,1f%~%" i loss (* 100 accuracy))))))
    (loop (+ i 1))))
(compute-flush!)
(define elapsed (- (now) start))
(format #t "~a steps in ~,2f s (~a steps/s)~%" steps elapsed (inexact->exact (round (/ steps elapsed))))

;;; ------------------------------------------------------------------
;;; The picture: the predicted class over a grid, then the training points.

(define size 360)                          ; pixels, and grid points per side
(define extent 1.1)                        ; the plane from -extent to extent
(define (pixel->coord p) (- (* 2 extent (/ (+ p 0.5) size)) extent))
(define (coord->pixel v) (inexact->exact (floor (* size (/ (+ v extent) (* 2 extent))))))

(define predictions
  (let* ((grid (list->tensor (append-map (lambda (row)
                                           (append-map (lambda (col)
                                                         (list (pixel->coord col) (- (pixel->coord row))))
                                                       (iota size)))
                                         (iota size))
                             (list (* size size) 2)))
         (probs (call-with-values (lambda () (forward grid)) (lambda (h1 h2 p) (tensor->f32vector p)))))
    (lambda (i)                            ; the predicted class of grid point i
      (let* ((row (map (lambda (k) (f32vector-ref probs (+ (* i classes) k))) (iota classes))))
        (list-index (lambda (v) (= v (apply max row))) row)))))

(define light-colors '((250 200 190) (190 225 200) (195 205 245)))
(define dark-colors '((200 40 30) (30 140 60) (40 60 200)))

(let ((image (make-bytevector (* 3 size size) 0)))
  (define (set-pixel! col row color)
    (when (and (< -1 col size) (< -1 row size))
      (for-each (lambda (v k) (bytevector-u8-set! image (+ (* 3 (+ (* row size) col)) k) v)) color '(0 1 2))))
  (do ((i 0 (+ i 1))) ((= i (* size size)))
    (set-pixel! (modulo i size) (quotient i size) (list-ref light-colors (predictions i))))
  (for-each (match-lambda
              ((px py c)
               (let ((col (coord->pixel px)) (row (coord->pixel (- py))))
                 (do ((dy -2 (+ dy 1))) ((> dy 2))
                   (do ((dx -2 (+ dx 1))) ((> dx 2))
                     (when (<= (+ (* dx dx) (* dy dy)) 5)
                       (set-pixel! (+ col dx) (+ row dy)
                                   (if (= 5 (+ (* dx dx) (* dy dy))) '(255 255 255) (list-ref dark-colors c)))))))))
            points)
  (call-with-output-file output
    (lambda (port)
      (format port "P6~%~a ~a~%255~%" size size)
      (put-bytevector port image))
    #:binary #t))
(format #t "wrote ~a~%" output)
