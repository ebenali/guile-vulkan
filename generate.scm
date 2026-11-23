;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; Generates vulkan.scm and vulkan/{xlib,xcb,wayland}.scm from a CastXML
;;; parse of the Vulkan headers, and vulkan/win32.scm from a second parse
;;; for 64-bit Windows.  Run from the repository root:
;;;
;;;   castxml --castxml-gccxml -DVK_USE_PLATFORM_XLIB_KHR \
;;;     -DVK_USE_PLATFORM_XLIB_XRANDR_EXT -DVK_USE_PLATFORM_XCB_KHR \
;;;     -DVK_USE_PLATFORM_WAYLAND_KHR -o res/vulkan.xml /usr/include/vulkan/vulkan.h
;;;   guile generate.scm
;;;
;;;   res/make-win32-xml.sh            # see there; needs the Windows SDK headers
;;;   guile generate.scm res/vulkan-win32.xml
;;;
;;; Each run writes the modules whose headers are in its input.  The Windows
;;; parse (recognized by vulkan_win32.h) does not rewrite vulkan.scm: the core
;;; module generated from it must be identical to the existing one.
;;;
;;; Struct layouts (sizes and field offsets) are taken from CastXML as they
;;; are.  Functions every Vulkan loader exports (core 1.0-1.4 and the
;;; window-system extensions, decided from the header section each one is
;;; declared in) are bound directly; every function also gets a vk:load-*
;;; procedure that fetches it with vkGet*ProcAddr.  The headers must be where
;;; CastXML read them.

(use-modules (sxml simple)
             (ice-9 match)
             (ice-9 rdelim)
             (ice-9 pretty-print)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-13)
             (system foreign-library))

(define default-input "res/vulkan.xml")
(define input-file
  (match (command-line) ((_ file . _) file) (_ default-input)))

;; Each generated module: its name, output file and the Vulkan headers
;; whose declarations it holds.
(define libraries
  '((core    (vulkan)         "vulkan.scm"         ("vulkan_core.h" "vulkan.h" "vk_platform.h" "vk_video/"))
    (xlib    (vulkan xlib)    "vulkan/xlib.scm"    ("vulkan_xlib.h" "vulkan_xlib_xrandr.h"))
    (xcb     (vulkan xcb)     "vulkan/xcb.scm"     ("vulkan_xcb.h"))
    (wayland (vulkan wayland) "vulkan/wayland.scm" ("vulkan_wayland.h"))
    (win32   (vulkan win32)   "vulkan/win32.scm"   ("vulkan_win32.h"))))

;; What (vulkan) passes on from (vulkan runtime).
(define runtime-api
  '(vk-ref vk-set! vk-sizeof vk-offset make-vk vk-new
    vk-address vk-bytevector vk-c-string vk-c-string-array vk-c-string-ref))

;;; ------------------------------------------------------------------
;;; The CastXML tree.

(define doc
  (let ((top (call-with-input-file input-file
               (lambda (port) (xml->sxml port #:trim-whitespace? #t)))))
    (or (find (lambda (n) (and (pair? n) (memq (car n) '(GCC_XML CastXML)))) (cdr top))
        (error "no GCC_XML/CastXML root element in" input-file))))

(define (tag n) (car n))
(define (has-attrs? n) (and (pair? (cdr n)) (pair? (cadr n)) (eq? (caadr n) '@)))
(define (attr n name)
  (and (has-attrs? n) (let ((p (assq name (cdadr n)))) (and p (cadr p)))))
(define (children n)
  (filter pair? (if (has-attrs? n) (cddr n) (cdr n))))

(define nodes (children doc))

(define by-id (make-hash-table))
(for-each (lambda (n) (let ((id (attr n 'id))) (when id (hash-set! by-id id n)))) nodes)
(define (lookup id) (or (hash-ref by-id id) (error "unknown id" id)))

(define (named? n) (let ((nm (attr n 'name))) (and nm (not (string-null? nm)))))

;;; ------------------------------------------------------------------
;;; Names (identical to chez-vulkan's, so both bind the same names).

(define (split-alnum str)
  (string-tokenize str char-set:letter+digit))

;; VK_FOO_BAR -> vk:foo-bar
(define (vk-enum-symbol str)
  (let ((dstr (string-downcase (string-join (split-alnum str) "-"))))
    (string->symbol (if (string-prefix? "vk-" dstr)
                        (string-append "vk:" (substring dstr 3))
                        dstr))))

;; VkFooBar -> vk:foo-bar: a dash before every upper-case letter that does
;; not follow another upper-case letter.
(define (vk-symbol str)
  (let loop ((cs (string->list str)) (prev #f) (acc '()))
    (if (null? cs)
        (vk-enum-symbol (list->string (reverse acc)))
        (let ((c (car cs)))
          (loop (cdr cs) c
                (if (and prev (char-upper-case? c) (not (char-upper-case? prev)))
                    (cons* c #\- acc)
                    (cons c acc)))))))

(define (loader-symbol name)
  (let ((s (symbol->string (vk-symbol name))))
    (string->symbol (string-append "vk:load-" (substring s 3)))))

;;; ------------------------------------------------------------------
;;; Which module each declaration belongs to.

(define file-libraries                   ; file id -> library
  (let ((ht (make-hash-table)))
    (for-each
     (lambda (n)
       (when (eq? (tag n) 'File)
         (let ((name (attr n 'name)))
           (when (or (string-contains name "/vulkan/") (string-contains name "/vk_video/"))
             (let ((lib (find (lambda (l)
                                (any (lambda (pat)
                                       (if (string-suffix? "/" pat)
                                           (string-contains name (string-append "/" pat))
                                           (string-suffix? (string-append "/" pat) name)))
                                     (fourth l)))
                              libraries)))
               (if lib
                   (hash-set! ht (attr n 'id) (car lib))
                   (format #t "note: ignoring ~a (no module for it)~%" name)))))))
     nodes)
    ht))

(define (node-library n) (let ((f (attr n 'file))) (and f (hash-ref file-libraries f))))
(define (in-input? lib) (hash-count (lambda (k v) (eq? v lib)) file-libraries))

;; A few structs mangle to the same name as an enum value (e.g.
;; VkPipelineCacheHeaderVersionOne); those get a -struct suffix.
(define value-names
  (let ((ht (make-hash-table)))
    (for-each (lambda (n)
                (when (node-library n)
                  (case (tag n)
                    ((Enumeration)
                     (for-each (lambda (v) (hashq-set! ht (vk-enum-symbol (attr v 'name)) #t))
                               (filter (lambda (c) (eq? (tag c) 'EnumValue)) (children n))))
                    ((Variable) (hashq-set! ht (vk-enum-symbol (attr n 'name)) #t)))))
              nodes)
    ht))

(define (type-symbol name)
  (let ((sym (vk-symbol name)))
    (if (hashq-ref value-names sym)
        (symbol-append sym '-struct)
        sym)))

;;; ------------------------------------------------------------------
;;; Header sections: the last "#define VK_xxx 1" before each declaration.

(define section-starts                   ; file id -> ((line . section) ...), descending
  (let ((ht (make-hash-table)))
    (for-each
     (lambda (n)
       (when (and (eq? (tag n) 'File) (hash-ref file-libraries (attr n 'id)))
         (let ((path (attr n 'name)))
           (unless (file-exists? path)
             (error "header not found; regenerate the CastXML parse on this machine:" path))
           (call-with-input-file path
             (lambda (port)
               (let loop ((line 1) (acc '()))
                 (let ((text (read-line port)))
                   (if (eof-object? text)
                       (hash-set! ht (attr n 'id) acc)
                       (loop (+ line 1)
                             (match (string-tokenize text)
                               ;; VK_VERSION_x_y and extension names (which have
                               ;; lower-case letters), not e.g. ..._SPEC_VERSION 1.
                               (("#define" name "1")
                                (if (or (string-prefix? "VK_VERSION_" name)
                                        (and (string-prefix? "VK_" name)
                                             (string-any char-lower-case? name)))
                                    (cons (cons line name) acc)
                                    acc))
                               (_ acc)))))))))))
     nodes)
    ht))

(define (function-section fn)
  (let* ((line (string->number (attr fn 'line)))
         (hit (find (lambda (p) (< (car p) line)) (or (hash-ref section-starts (attr fn 'file)) '()))))
    (and hit (cdr hit))))

;; What every Vulkan loader exports.
(define (loader-export? fn)
  (let ((section (function-section fn)))
    (and section
         (or (string-prefix? "VK_VERSION_" section)
             (member section '("VK_KHR_surface" "VK_KHR_swapchain" "VK_KHR_display"
                               "VK_KHR_display_swapchain" "VK_KHR_get_display_properties2"
                               "VK_KHR_get_surface_capabilities2" "VK_EXT_headless_surface"))
             (and (string-prefix? "VK_KHR_" section) (string-suffix? "_surface" section)))
         #t)))

;;; ------------------------------------------------------------------
;;; Types.

;; Integer types by size, not C name (long is 32 bits on Windows).
(define (fundamental-kind n)
  (let ((name (attr n 'name)) (size (string->number (attr n 'size))))
    (cond ((string=? name "float") 'f32)
          ((string=? name "double") 'f64)
          ((string=? name "void") 'void)
          ((member name '("char" "unsigned char")) 'u8)
          ((string=? name "signed char") 's8)
          ((member name '("short int" "short unsigned int" "int" "unsigned int"
                          "long int" "long unsigned int" "long long int" "long long unsigned int"))
           (symbol-append (if (string-contains name "unsigned") 'u 's)
                          (string->symbol (number->string size))))
          (else (error "unsupported fundamental type" name)))))

(define (underlying n)
  (case (tag n)
    ((Typedef CvQualifiedType ElaboratedType) (underlying (lookup (attr n 'type))))
    (else n)))

;; A field's type, as define-vk-struct describes it.
(define (field-type id)
  (let ((n (lookup id)))
    (case (tag n)
      ((FundamentalType) (list (fundamental-kind n)))
      ((CvQualifiedType ElaboratedType Typedef) (field-type (attr n 'type)))
      ((PointerType) '(ptr))
      ((Enumeration) '(s32))
      ((ArrayType)
       (let ((max (attr n 'max)))
         (list 'array (if (or (not max) (string-null? max)) 0 (+ 1 (string->number max)))
               (field-type (attr n 'type)))))
      ((Struct Union)
       (if (named? n)
           (list 'struct (type-symbol (attr n 'name)))
           (error "anonymous struct or union field" id)))
      (else (error "unsupported field type" (tag n) id)))))

;; The (system foreign) type for an argument or result.
(define (ffi-type id)
  (match (field-type id)
    (('ptr) 'uintptr_t) (('void) 'void) (('f32) 'float) (('f64) 'double)
    (('u8) 'uint8) (('s8) 'int8) (('u16) 'uint16) (('s16) 'int16)
    (('u32) 'uint32) (('s32) 'int32) (('u64) 'uint64) (('s64) 'int64)
    (t (error "unsupported argument type (struct by value?)" t id))))

(define (record-fields n)
  (let ((members (attr n 'members)))
    (if members
        (filter (lambda (m) (and (eq? (tag m) 'Field) (named? m)))
                (map lookup (string-tokenize members)))
        '())))

(define (field-entry f)
  (let ((name (vk-symbol (attr f 'name)))
        (offset (string->number (attr f 'offset))))     ; in bits
    (if (attr f 'bits)
        (let* ((u (underlying (lookup (attr f 'type))))
               (unit-bits (string->number (attr u 'size)))
               (start (* unit-bits (quotient offset unit-bits))))
          `(,name (bits ,(symbol-append 'u (string->symbol (number->string unit-bits)))
                        ,(- offset start) ,(string->number (attr f 'bits))
                        ,(and (eq? (tag u) 'FundamentalType)
                              (not (string-contains (attr u 'name) "unsigned"))))
                  ,(quotient start 8)))
        `(,name ,(field-type (attr f 'type)) ,(quotient offset 8)))))

;; Struct sizes, to check that every field fits.
(define struct-sizes (make-hash-table))
(define scalar-sizes '((u8 . 1) (s8 . 1) (u16 . 2) (s16 . 2) (u32 . 4) (s32 . 4)
                       (u64 . 8) (s64 . 8) (f32 . 4) (f64 . 8) (ptr . 8)))
(define (spec-size spec)
  (match spec
    (('struct name) (or (hashq-ref struct-sizes name) (error "struct used before its size is known" name)))
    (('array n elem) (* n (spec-size elem)))
    (('bits unit . _) (assq-ref scalar-sizes unit))
    ((kind) (assq-ref scalar-sizes kind))))

;;; ------------------------------------------------------------------
;;; Generate one module's definitions => (values exports forms).

(define (struct-target n)                ; the Struct/Union a typedef names, or #f
  (let ((u (underlying n)))
    (and (memq (tag u) '(Struct Union)) (named? u) (not (attr u 'incomplete)) u)))

(define (generate-library lib)
  (define forms '())
  (define exports '())
  ;; Only vk: names are exported, as in chez-vulkan; the video codec types
  ;; (std-video-...) stay internal and are reached through vk: structs.
  (define (emit! form name)
    (set! forms (cons form forms))
    (when (string-prefix? "vk:" (symbol->string name))
      (set! exports (cons name exports))))
  ;; Structs and unions, then aliases (typedef VkFoo VkFooKHR).
  (for-each
   (lambda (n)
     (when (and (eq? (node-library n) lib) (memq (tag n) '(Struct Union)) (named? n)
                (not (attr n 'incomplete)))
       (let ((name (type-symbol (attr n 'name)))
             (size (quotient (string->number (attr n 'size)) 8))
             (fields (map field-entry (record-fields n))))
         (hashq-set! struct-sizes name size)
         (emit! `(define-vk-struct ,name ,size ,@fields) name))))
   nodes)
  (for-each
   (lambda (n)
     (when (and (eq? (node-library n) lib) (eq? (tag n) 'Typedef))
       (let ((target (struct-target n)))
         (when (and target (not (string=? (attr target 'name) (attr n 'name))))
           (let ((name (type-symbol (attr n 'name))))
             (emit! `(define-vk-alias ,name ,(type-symbol (attr target 'name))) name))))))
   nodes)
  ;; Enum values and 64-bit flag constants.
  (for-each
   (lambda (n)
     (when (eq? (node-library n) lib)
       (case (tag n)
         ((Enumeration)
          (for-each (lambda (v)
                      (let ((name (vk-enum-symbol (attr v 'name))))
                        (emit! `(define ,name ,(string->number (attr v 'init))) name)))
                    (filter (lambda (c) (eq? (tag c) 'EnumValue)) (children n))))
         ((Variable)
          (when (attr n 'init)
            (let* ((init (string-trim-right (attr n 'init) (char-set #\u #\U #\l #\L)))
                   (value (if (string-prefix? "0x" init)
                              (string->number (substring init 2) 16)
                              (string->number init)))
                   (name (vk-enum-symbol (attr n 'name))))
              (unless value (error "cannot parse constant" (attr n 'name) init))
              (emit! `(define ,name ,value) name)))))))
   nodes)
  ;; Functions.
  (for-each
   (lambda (n)
     (when (and (eq? (node-library n) lib) (eq? (tag n) 'Function))
       (let* ((name (attr n 'name))
              (args (filter (lambda (c) (eq? (tag c) 'Argument)) (children n)))
              (arg-types (map (lambda (a) (ffi-type (attr a 'type))) args))
              (ret (ffi-type (attr n 'returns)))
              (device? (and (pair? args)
                            (let ((t (lookup (attr (car args) 'type))))
                              (and (eq? (tag t) 'Typedef)
                                   (member (attr t 'name) '("VkDevice" "VkQueue" "VkCommandBuffer")))))))
         (when (loader-export? n)
           (emit! `(define-vk-procedure ,(vk-symbol name) ,name ,arg-types ,ret) (vk-symbol name)))
         (emit! `(define-vk-loader ,(loader-symbol name)
                   ,(if device? 'device-proc-addr 'instance-proc-addr)
                   ,name ,arg-types ,ret)
                (loader-symbol name)))))
   nodes)
  (values (reverse exports) (reverse forms)))

;; Every field must lie inside its struct.
(define (check-layouts forms)
  (for-each
   (match-lambda
     (('define-vk-struct name size fields ...)
      (for-each (match-lambda
                  ((field spec offset)
                   (unless (<= (+ offset (spec-size spec)) size)
                     (error "field outside its struct:" name field offset size))))
                fields))
     (_ #t))
   forms))

;;; ------------------------------------------------------------------
;;; Write the modules.

(define (module-text lib name exports forms)
  (call-with-output-string
   (lambda (port)
     (format port ";;; Generated by generate.scm from a CastXML parse of the Vulkan headers -- do not edit.~%")
     (format port ";;; SPDX-License-Identifier: LGPL-3.0-or-later~%~%")
     ;; The header by hand: keyword and value on one line, lists wrapped.
     (define (write-list items indent)
       (let loop ((items items) (column indent) (first? #t))
         (unless (null? items)
           (let ((text (symbol->string (car items))))
             (cond (first? (display text port) (loop (cdr items) (+ column (string-length text)) #f))
                   ((> (+ column 1 (string-length text)) 96)
                    (format port "~%~a~a" (make-string indent #\space) text)
                    (loop (cdr items) (+ indent (string-length text)) #f))
                   (else (format port " ~a" text)
                         (loop (cdr items) (+ column 1 (string-length text)) #f)))))))
     (format port "(define-module ~s~%" name)
     (format port "  #:use-module (system foreign)~%")
     (format port "  #:use-module (vulkan runtime)~%")
     (unless (eq? lib 'core) (format port "  #:use-module (vulkan)~%"))
     (when (eq? lib 'core)
       (format port "  #:re-export (")
       (write-list runtime-api 15)
       (format port ")~%"))
     (format port "  #:export (")
     (write-list exports 12)
     (format port "))~%~%")
     (for-each (lambda (form) (write form port) (newline port)) forms))))

(define (main)
  (for-each
   (match-lambda
     ((lib name file _)
      (when (positive? (in-input? lib))
        (call-with-values (lambda () (generate-library lib))
          (lambda (exports forms)
            (check-layouts forms)
            (let ((text (module-text lib name exports forms)))
              (cond
               ;; The Windows parse only checks the core.
               ((and (eq? lib 'core) (positive? (in-input? 'win32)))
                (if (and (file-exists? file)
                         (string=? text (call-with-input-file file get-string-all)))
                    (format #t "~a: the core module from ~a is identical to ~a~%" input-file input-file file)
                    (begin (format #t "error: the core module from ~a differs from ~a~%" input-file file)
                           (exit 1))))
               (else
                (call-with-output-file file (lambda (port) (display text port)))
                (format #t "wrote ~a: ~a exports, ~a structs, ~a bound functions, ~a loaders~%"
                        file (length exports)
                        (count (lambda (f) (eq? (car f) 'define-vk-struct)) forms)
                        (count (lambda (f) (eq? (car f) 'define-vk-procedure)) forms)
                        (count (lambda (f) (eq? (car f) 'define-vk-loader)) forms))))))))))
   libraries))

(main)
