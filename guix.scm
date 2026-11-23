;;; guile-vulkan --- Vulkan bindings for GNU Guile
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;;
;;; A Guix package for this checkout (the files git tracks):
;;;
;;;   guix build -f guix.scm
;;;   guix shell -f guix.scm -- guile -c '(use-modules (vulkan))'
;;;
;;; The bindings find the Vulkan loader by its store path; Vulkan drivers are
;;; found as usual (e.g. Mesa's, through VK_DRIVER_FILES or XDG_DATA_DIRS).
;;; The manual is installed as Info: `info guile-vulkan'.

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix build-system guile)
             ((guix licenses) #:prefix license:)
             (gnu packages guile)
             (gnu packages texinfo)
             (gnu packages vulkan))

(define %source-directory (dirname (current-filename)))

(package
  (name "guile-vulkan")
  (version "0.1.0-git")
  (source (local-file %source-directory "guile-vulkan-checkout"
                      #:recursive? #t
                      #:select? (git-predicate %source-directory)))
  (build-system guile-build-system)
  (arguments
   (list
    #:phases
    #~(modify-phases %standard-phases
        (add-after 'unpack 'keep-only-the-library
          (lambda _
            (for-each (lambda (file)
                        (when (file-exists? file)
                          (delete-file-recursively file)))
                      '("examples" "tests" "res" "shaders" "generate.scm" "guix.scm"))))
        (add-after 'keep-only-the-library 'hardcode-vulkan-loader
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "vulkan/runtime.scm"
              (("\"libvulkan\\.so\\.1\"")
               (string-append "\"" (search-input-file inputs "lib/libvulkan.so.1") "\"")))))
        (add-after 'build 'install-manual       ; guile-build-system's build also installs
          (lambda _
            (let ((info (string-append #$output "/share/info")))
              (mkdir-p info)
              (invoke "makeinfo" "-o" info "doc/guile-vulkan.texi")))))))
  (native-inputs (list guile-3.0 texinfo))
  (inputs (list guile-3.0 vulkan-loader))
  (synopsis "Vulkan bindings for GNU Guile")
  (description
   "Guile-Vulkan provides bindings to the Vulkan 1.4 graphics and compute API
for GNU Guile, generated from the Vulkan headers.  Structs are bytevectors with
field accessors that compile to direct bytevector accesses, and pointer fields
and arguments accept bytevectors, keeping them alive while Vulkan may use them.
Every function is available, either directly or through a @code{vk:load-*}
procedure that fetches it at run time.")
  (home-page "https://github.com/ebenali/guile-vulkan")
  (license license:lgpl3+))
