;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; Runtime support for the generated modules: struct layouts as macros over
;;; bytevectors, binding loader-exported functions, and fetching the rest
;;; with vkGet*ProcAddr.
;;;
;;; Structs are bytevectors.  Each generated struct name is a macro that
;;; knows the struct's layout, so field access compiles to a direct
;;; bytevector access at a constant offset:
;;;
;;;   (define info (make-vk vk:application-info))
;;;   (vk-set! vk:application-info (api-version) info vk:api-version-1-3)
;;;   (vk-ref vk:physical-device-properties (limits max-image-dimension2-d) props)
;;;   (vk-ref vk:physical-device-memory-properties (memory-types i property-flags) props)
;;;
;;; A path names fields; after an array field comes an index (any
;;; expression).  An optional index after the bytevector selects an element
;;; of an array of structs: (vk-ref vk:queue-family-properties (queue-flags) bv i).
;;;
;;; Pointers and handles are integers.  Wherever Vulkan takes a pointer, a
;;; bytevector can be given instead, and it is kept alive for you:
;;;
;;;   - a pointer field set with vk-set!/vk-new accepts a bytevector, a
;;;     string (stored as a NUL-terminated UTF-8 copy) or #f (null).  The
;;;     object stays alive as long as the struct holding it, until that
;;;     field is set again;
;;;   - a pointer or handle argument of a Vulkan function accepts a
;;;     bytevector or #f, and the bytevector stays alive during the call.
;;;
;;; An address given as an integer (e.g. from vk-address) is not tracked:
;;; then the caller must keep the object reachable while Vulkan may use it.
;;; Guile's collector does not move objects, so addresses stay valid.

(define-module (vulkan runtime)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:export (define-vk-struct define-vk-alias
            vk-ref vk-set! vk-sizeof vk-offset make-vk vk-new
            vk-address vk-bytevector vk-c-string vk-c-string-array vk-c-string-ref
            define-vk-procedure define-vk-loader vk-pointer-value
            vulkan-loader instance-proc-addr device-proc-addr))

;;; ------------------------------------------------------------------
;;; Struct layouts.
;;;
;;; (define-vk-struct NAME SIZE (FIELD TYPE OFFSET) ...)
;;; TYPE is one of
;;;   (u8) (s8) (u16) (s16) (u32) (s32) (u64) (s64) (f32) (f64) (ptr)
;;;   (struct NAME)              a nested struct or union
;;;   (array N TYPE)             N elements of TYPE
;;;   (bits UNIT SHIFT WIDTH SIGNED?)  a C bitfield in a UNIT-sized word
;;;                              (UNIT one of u8 u16 u32 u64) at OFFSET

(eval-when (expand load eval)
  (define scalar-sizes
    '((u8 . 1) (s8 . 1) (u16 . 2) (s16 . 2) (u32 . 4) (s32 . 4)
      (u64 . 8) (s64 . 8) (f32 . 4) (f64 . 8) (ptr . 8)))

  (define (scalar-ref kind)
    (case kind
      ((u8) #'bytevector-u8-ref) ((s8) #'bytevector-s8-ref)
      ((u16) #'bytevector-u16-native-ref) ((s16) #'bytevector-s16-native-ref)
      ((u32) #'bytevector-u32-native-ref) ((s32) #'bytevector-s32-native-ref)
      ((u64 ptr) #'bytevector-u64-native-ref) ((s64) #'bytevector-s64-native-ref)
      ((f32) #'bytevector-ieee-single-native-ref) ((f64) #'bytevector-ieee-double-native-ref)))

  (define (scalar-set kind)
    (case kind
      ((u8) #'bytevector-u8-set!) ((s8) #'bytevector-s8-set!)
      ((u16) #'bytevector-u16-native-set!) ((s16) #'bytevector-s16-native-set!)
      ((u32) #'bytevector-u32-native-set!) ((s32) #'bytevector-s32-native-set!)
      ((u64 ptr) #'bytevector-u64-native-set!) ((s64) #'bytevector-s64-native-set!)
      ((f32) #'bytevector-ieee-single-native-set!) ((f64) #'bytevector-ieee-double-native-set!)))

  ;; Code for the element size of TYPE (a constant, or (vk-sizeof T)).
  ;; CTX is the defining struct's name, so nested names resolve where it
  ;; was defined.
  (define (type-size type ctx)
    (match type
      ((kind) (datum->syntax ctx (assq-ref scalar-sizes kind)))
      (('struct name) #`(vk-sizeof #,(datum->syntax ctx name)))
      (('array n elem) #`(* #,n #,(type-size elem ctx)))
      (('bits unit . _) (datum->syntax ctx (assq-ref scalar-sizes unit)))))

  ;; Expand an access of the value at OFFSET (code) with TYPE, following
  ;; the rest of PATH.  OP is #:ref or #:set!; VALUE is the new value.
  (define (expand-access ctx op type bv offset path value)
    (syntax-case path ()
      (()
       (match type
         (('ptr)
          (if (eq? (syntax->datum op) #:ref)
              #`(bytevector-u64-native-ref #,bv #,offset)
              #`(let ((o #,offset))
                  (bytevector-u64-native-set! #,bv o (vk-pointer-value #,bv o #,value)))))
         ((kind)
          (if (eq? (syntax->datum op) #:ref)
              #`(#,(scalar-ref kind) #,bv #,offset)
              #`(#,(scalar-set kind) #,bv #,offset #,value)))
         (('bits unit shift width signed?)
          (let ((mask (- (ash 1 width) 1)))
            (if (eq? (syntax->datum op) #:ref)
                #`(let ((v (logand (ash (#,(scalar-ref unit) #,bv #,offset) #,(- shift)) #,mask)))
                    #,(if signed?
                          #`(if (logbit? #,(- width 1) v) (- v #,(ash 1 width)) v)
                          #'v))
                #`(let ((o #,offset))
                    (#,(scalar-set unit) #,bv o
                     (logior (logand (#,(scalar-ref unit) #,bv o) #,(lognot (ash mask shift)))
                             (ash (logand #,value #,mask) #,shift)))))))
         (_ (syntax-violation 'vk-ref "path ends at a struct or array; name a field" path))))
      ((step . rest)
       (match type
         (('struct name)
          #`(#,(datum->syntax ctx name) #:access #,op #,bv #,offset (step . rest) #,value))
         (('array n elem)
          (expand-access ctx op elem bv #`(+ #,offset (* step #,(type-size elem ctx))) #'rest value))
         (_ (syntax-violation 'vk-ref "field is not a struct or array" path))))))

  ;; The transformer for the struct named by identifier CTX: SIZE bytes, FIELDS.
  (define (make-struct-transformer ctx size fields)
    (define name (syntax->datum ctx))
    (lambda (x)
      (syntax-case x ()
        ((k #:size) (datum->syntax #'k size))
        ((k #:offset (field . rest))
         (let ((f (assq (syntax->datum #'field) fields)))
           (unless f (syntax-violation name "no such field" #'field))
           (match (cadr f)
             (('struct sub) (if (null? (syntax->datum #'rest))
                                (datum->syntax #'k (caddr f))
                                #`(+ #,(caddr f) (#,(datum->syntax ctx sub) #:offset rest))))
             (_ (if (null? (syntax->datum #'rest))
                    (datum->syntax #'k (caddr f))
                    (syntax-violation name "vk-offset: only struct fields can be followed" #'rest))))))
        ((k #:access op bv offset (field . rest) value)
         (let ((f (assq (syntax->datum #'field) fields)))
           (unless f (syntax-violation name "no such field" #'field))
           (expand-access ctx #'op (cadr f) #'bv #`(+ offset #,(caddr f)) #'rest #'value)))))))

(define-syntax define-vk-struct
  (lambda (x)
    (syntax-case x ()
      ((_ name size (field type offset) ...)
       #'(define-syntax name
           (make-struct-transformer #'name size '((field type offset) ...)))))))

;; (define-vk-alias NEW OLD): another name for a struct (e.g. ...-khr).
(define-syntax define-vk-alias
  (syntax-rules ()
    ((_ new old) (define-syntax new (syntax-rules () ((_ . args) (old . args)))))))

(define-syntax vk-sizeof
  (syntax-rules () ((_ T) (T #:size))))

;; Byte offset of a (nested) field: (vk-offset T (field sub-field ...)).
(define-syntax vk-offset
  (syntax-rules () ((_ T path) (T #:offset path))))

(define-syntax vk-ref
  (syntax-rules ()
    ((_ T path bv) (T #:access #:ref bv 0 path #f))
    ((_ T path bv i) (T #:access #:ref bv (* i (vk-sizeof T)) path #f))))

(define-syntax vk-set!
  (syntax-rules ()
    ((_ T path bv value) (T #:access #:set! bv 0 path value))
    ((_ T path bv i value) (T #:access #:set! bv (* i (vk-sizeof T)) path value))))

;; A zeroed bytevector for one T, or COUNT of them.
(define-syntax make-vk
  (syntax-rules ()
    ((_ T) (make-bytevector (vk-sizeof T) 0))
    ((_ T count) (make-bytevector (* count (vk-sizeof T)) 0))))

;; (vk-new T (field ... value) ...): a zeroed T with the given fields set.
(define-syntax vk-new
  (syntax-rules ()
    ((_ T (field ... value) ...)
     (let ((bv (make-vk T)))
       (vk-set! T (field ...) bv value) ...
       bv))))

;;; ------------------------------------------------------------------
;;; Memory.

;; The address to store for VALUE in OWNER's pointer field at OFFSET,
;; keeping a bytevector or string alive with OWNER (one per field).
(define (vk-pointer-value owner offset value)
  (define (keep! obj)
    (let ((table (or (object-property owner 'vk-keep)
                     (let ((t (make-hash-table)))
                       (set-object-property! owner 'vk-keep t)
                       t))))
      (if obj (hashv-set! table offset obj) (hashv-remove! table offset))))
  (cond ((bytevector? value) (keep! value) (vk-address value))
        ((string? value) (let ((c (vk-c-string value))) (keep! c) (vk-address c)))
        ((not value) (keep! #f) 0)
        ((exact-integer? value) (keep! #f) value)
        (else (error "not a pointer value (bytevector, string, #f or address):" value))))

(define (vk-address bv)
  "The address of bytevector BV, to pass to Vulkan."
  (pointer-address (bytevector->pointer bv)))

(define (vk-bytevector address size)
  "A bytevector over SIZE bytes of foreign memory at ADDRESS (e.g. mapped memory)."
  (pointer->bytevector (make-pointer address) size))

;; Strings handed to Vulkan, kept alive with whatever refers to them.
(define (vk-c-string s)
  "A NUL-terminated UTF-8 copy of S."
  (let* ((utf8 (string->utf8 s))
         (bv (make-bytevector (+ 1 (bytevector-length utf8)) 0)))
    (bytevector-copy! utf8 0 bv 0 (bytevector-length utf8))
    bv))

(define (vk-c-string-array strings)
  "A char*[] for STRINGS (e.g. for ppEnabledExtensionNames).  The strings
stay alive as long as the returned bytevector does."
  (let* ((cs (map vk-c-string strings))
         (bv (make-bytevector (* 8 (max 1 (length cs))) 0)))
    (let loop ((cs cs) (i 0))
      (unless (null? cs)
        (bytevector-u64-native-set! bv (* 8 i) (vk-address (car cs)))
        (loop (cdr cs) (+ i 1))))
    (set-object-property! bv 'vk-strings cs)
    bv))

(define* (vk-c-string-ref bv #:optional (offset 0))
  "The NUL-terminated UTF-8 string in BV starting at OFFSET (e.g. deviceName)."
  (let loop ((end offset))
    (if (or (= end (bytevector-length bv)) (zero? (bytevector-u8-ref bv end)))
        (let ((out (make-bytevector (- end offset))))
          (bytevector-copy! bv offset out 0 (- end offset))
          (utf8->string out))
        (loop (+ end 1)))))

;;; ------------------------------------------------------------------
;;; The Vulkan loader, by its name on each platform.

(define vulkan-loader
  (let* ((candidates
          (cond ((string-contains %host-type "mingw") '("vulkan-1"))
                ((string-contains %host-type "darwin")
                 '("libvulkan.1.dylib" "/usr/local/lib/libvulkan.1.dylib"
                   "/opt/homebrew/lib/libvulkan.1.dylib" "libMoltenVK.dylib"))
                (else '("libvulkan.so.1" "libvulkan.so")))))
    (let try ((names candidates))
      (if (null? names)
          (error "cannot load the Vulkan loader; tried" candidates)
          (or (false-if-exception (load-foreign-library (car names)))
              (try (cdr names)))))))

(define (entry name)
  (false-if-exception (foreign-library-pointer vulkan-loader name)))

;; Keeps its arguments alive up to the call; mutable, so never inlined away.
(define %keep (lambda args #f))
(set! %keep (lambda args #f))

;; A procedure for the function at POINTER that takes bytevectors (and #f)
;; for its pointer and handle arguments, keeping them alive during the call.
(define (vk-foreign-procedure ret pointer types)
  (let ((f (pointer->procedure ret pointer types)))
    (if (not (memv uintptr_t types))
        f
        (lambda args
          (let ((r (apply f (map (lambda (type arg)
                                   (if (eqv? type uintptr_t)
                                       (cond ((bytevector? arg) (vk-address arg))
                                             ((not arg) 0)
                                             (else arg))
                                       arg))
                                 types args))))
            (%keep args)
            r)))))

;; (define-vk-procedure NAME "vkName" (ARG-TYPE ...) RET-TYPE)
;; A function the loader exports.  If this loader is older and lacks it,
;; calling it raises an error (its vk:load-* can still fetch it).
(define-syntax define-vk-procedure
  (syntax-rules ()
    ((_ name cname (arg ...) ret)
     (define name
       (let ((p (entry cname)))
         (if p
             (vk-foreign-procedure ret p (list arg ...))
             (lambda args (error "not exported by the Vulkan loader:" cname))))))))

(define instance-proc-addr
  (let ((f (pointer->procedure uintptr_t (entry "vkGetInstanceProcAddr") (list uintptr_t '*))))
    (lambda (instance name) (f (or instance 0) (string->pointer name)))))

(define device-proc-addr
  (let ((f (pointer->procedure uintptr_t (entry "vkGetDeviceProcAddr") (list uintptr_t '*))))
    (lambda (device name) (f (or device 0) (string->pointer name)))))

;; (define-vk-loader NAME GET-PROC-ADDR "vkName" (ARG-TYPE ...) RET-TYPE)
;; (NAME handle) => the function as a procedure, or #f if unavailable.
(define-syntax define-vk-loader
  (syntax-rules ()
    ((_ name get-proc-addr cname (arg ...) ret)
     (define (name handle)
       (let ((addr (get-proc-addr handle cname)))
         (and (not (zero? addr))
              (vk-foreign-procedure ret (make-pointer addr) (list arg ...))))))))
