;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; Minimal Xlib bindings for the examples: one window, key presses, resizes
;;; and the window manager's close button.  Only libX11 is needed (on
;;; Wayland desktops it runs through XWayland).

(define-module (examples x11)
  #:use-module (vulkan runtime)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (x11-open-window x11-close-window x11-poll-events
            x11-window-display x11-window-id x11-window-size
            xk-escape xk-left xk-up xk-right xk-down))

(define libx11 (load-foreign-library "libX11.so.6"))
(define (x name ret args) (pointer->procedure ret (foreign-library-pointer libx11 name) args))

;; Window, Atom and KeySym are unsigned long; Bool is int.  Layouts as on
;; LP64 (sizeof(XEvent) is 192).
(define-vk-struct x-any-event 40 (type (s32) 0) (window (u64) 32))
(define-vk-struct x-key-event 96 (state (u32) 80) (keycode (u32) 84))
(define-vk-struct x-configure-event 88 (width (s32) 56) (height (s32) 60))
(define-vk-struct x-client-message-event 96 (message-type (u64) 40) (data0 (u64) 56))
(define xevent-size 192)

(define key-press 2)
(define configure-notify 22)
(define client-message 33)
(define key-press-mask 1)
(define structure-notify-mask (ash 1 17))
(define shift-mask 1)

(define xk-escape #xff1b)
(define xk-left #xff51)
(define xk-up #xff52)
(define xk-right #xff53)
(define xk-down #xff54)

(define XOpenDisplay (x "XOpenDisplay" uintptr_t (list uintptr_t)))
(define XCloseDisplay (x "XCloseDisplay" int (list uintptr_t)))
(define XDefaultScreen (x "XDefaultScreen" int (list uintptr_t)))
(define XRootWindow (x "XRootWindow" unsigned-long (list uintptr_t int)))
(define XBlackPixel (x "XBlackPixel" unsigned-long (list uintptr_t int)))
(define XCreateSimpleWindow
  (x "XCreateSimpleWindow" unsigned-long
     (list uintptr_t unsigned-long int int unsigned-int unsigned-int unsigned-int
           unsigned-long unsigned-long)))
(define XDestroyWindow (x "XDestroyWindow" int (list uintptr_t unsigned-long)))
(define XStoreName (x "XStoreName" int (list uintptr_t unsigned-long '*)))
(define XSelectInput (x "XSelectInput" int (list uintptr_t unsigned-long long)))
(define XInternAtom (x "XInternAtom" unsigned-long (list uintptr_t '* int)))
(define XSetWMProtocols (x "XSetWMProtocols" int (list uintptr_t unsigned-long '* int)))
(define XMapWindow (x "XMapWindow" int (list uintptr_t unsigned-long)))
(define XFlush (x "XFlush" int (list uintptr_t)))
(define XPending (x "XPending" int (list uintptr_t)))
(define XNextEvent (x "XNextEvent" int (list uintptr_t '*)))
(define XLookupKeysym (x "XLookupKeysym" unsigned-long (list '* int)))

(define-record-type <x11-window>
  (make-x11-window display id wm-delete event size)
  x11-window?
  (display x11-window-display)
  (id x11-window-id)
  (wm-delete x11-window-wm-delete)
  (event x11-window-event)                ; a bytevector holding one XEvent
  (size x11-window-size set-x11-window-size!))

(define (x11-open-window title width height)
  (let ((dpy (XOpenDisplay 0)))
    (when (zero? dpy) (error "cannot open the X display (is DISPLAY set?)"))
    (let* ((screen (XDefaultScreen dpy))
           (win (XCreateSimpleWindow dpy (XRootWindow dpy screen) 0 0 width height 0
                                     (XBlackPixel dpy screen) (XBlackPixel dpy screen)))
           (wm-delete (XInternAtom dpy (string->pointer "WM_DELETE_WINDOW") 0))
           (atoms (make-bytevector 8)))
      (XStoreName dpy win (string->pointer title))
      (XSelectInput dpy win (logior key-press-mask structure-notify-mask))
      (bytevector-u64-native-set! atoms 0 wm-delete)
      (XSetWMProtocols dpy win (bytevector->pointer atoms) 1)
      (XMapWindow dpy win)
      (XFlush dpy)
      (make-x11-window dpy win wm-delete (make-bytevector xevent-size 0) (cons width height)))))

(define (x11-close-window w)
  (XDestroyWindow (x11-window-display w) (x11-window-id w))
  (XCloseDisplay (x11-window-display w)))

(define (x11-poll-events w on-key on-resize)
  "Handle all pending events: (ON-KEY keysym) for key presses, (ON-RESIZE
width height) when the size changes.  Returns #f once the window manager
asked to close the window, #t otherwise."
  (let ((dpy (x11-window-display w))
        (ev (x11-window-event w)))
    (let loop ((open #t))
      (if (zero? (XPending dpy))
          open
          (begin
            (XNextEvent dpy (bytevector->pointer ev))
            (let ((type (vk-ref x-any-event (type) ev)))
              (cond
               ((= type key-press)
                ;; Index 1 is the shifted symbol (Z for z), if the key has one.
                (let* ((shifted (if (logtest (vk-ref x-key-event (state) ev) shift-mask)
                                    (XLookupKeysym (bytevector->pointer ev) 1)
                                    0)))
                  (on-key (if (zero? shifted) (XLookupKeysym (bytevector->pointer ev) 0) shifted)))
                (loop open))
               ((= type configure-notify)
                (let ((size (cons (vk-ref x-configure-event (width) ev)
                                  (vk-ref x-configure-event (height) ev))))
                  (unless (equal? size (x11-window-size w))
                    (set-x11-window-size! w size)
                    (on-resize (car size) (cdr size))))
                (loop open))
               ((= type client-message)
                (loop (and open (not (= (vk-ref x-client-message-event (data0) ev)
                                        (x11-window-wm-delete w))))))
               (else (loop open)))))))))
