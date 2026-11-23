;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; Checks that guile-vulkan and chez-vulkan bind the same Vulkan API: every
;;; name (vulkan) exports also exists in chez-vulkan's (vulkan), and every
;;; struct, union, directly bound function and vk:load-* procedure of
;;; chez-vulkan exists here.  (chez-vulkan also exports type names, such as
;;; vk:flags, which Guile has no use for.)
;;;
;;;   guile tests/compare-with-chez-vulkan.scm path/to/chez-vulkan/vulkan.ss

(use-modules (ice-9 match) (srfi srfi-1))

(define chez-file
  (match (command-line)
    ((_ file) file)
    (_ (error "usage: compare-with-chez-vulkan.scm CHEZ-VULKAN/vulkan.ss"))))

(define (first-form-headed head file)
  (call-with-input-file file
    (lambda (port)
      (let loop ()
        (let ((form (read port)))
          (cond ((eof-object? form) (error "not found in" file head))
                ((and (pair? form) (eq? (car form) head)) form)
                (else (loop))))))))

(define (vk-symbol? x) (and (symbol? x) (string-prefix? "vk:" (symbol->string x))))

;; chez-vulkan: (library (vulkan) (export name ...) (import ...) body ...)
(define chez (first-form-headed 'library chez-file))
(define chez-exports (filter vk-symbol? (cdr (third chez))))
(define chez-body (list-tail chez 4))
(define (chez-defined head pred)
  (filter-map (lambda (form)
                (match form
                  (((? (lambda (h) (eq? h head))) name . rest) (and (pred rest) name))
                  (_ #f)))
              chez-body))
(define chez-records
  (filter vk-symbol? (chez-defined 'define-ftype
                                   (match-lambda (((kind . _)) (memq kind '(struct union))) (_ #f)))))
(define chez-procedures (chez-defined 'define-vk-procedure (const #t)))
(define chez-loaders (chez-defined 'define-vk-loader (const #t)))

;; guile-vulkan: (define-module (vulkan) ... #:export (name ...))
(define guile-exports
  (let loop ((rest (cddr (first-form-headed 'define-module "vulkan.scm"))))
    (match rest
      ((#:export names . _) names)
      ((_ . rest) (loop rest))
      (() (error "no #:export in vulkan.scm")))))

(define failures 0)
(define (expect-subset what names of-names)
  (let ((missing (lset-difference eq? names of-names)))
    (if (null? missing)
        (format #t "ok   ~a: all ~a present~%" what (length names))
        (begin
          (set! failures (+ failures 1))
          (format #t "FAIL ~a: ~a missing, e.g. ~a~%" what (length missing) (list-head missing (min 5 (length missing))))))))

(expect-subset "guile-vulkan names in chez-vulkan" guile-exports chez-exports)
(expect-subset "chez-vulkan structs and unions in guile-vulkan" chez-records guile-exports)
(expect-subset "chez-vulkan bound functions in guile-vulkan" chez-procedures guile-exports)
(expect-subset "chez-vulkan vk:load-* procedures in guile-vulkan" chez-loaders guile-exports)
(exit (if (zero? failures) 0 1))
