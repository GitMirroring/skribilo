;;; skribilo-website.scm -- Guix build recipe for the skribilo website
;;;
;;; Copyright © 2024, 2026 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of Skribilo.
;;;
;;; Skribilo is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Skribilo is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Skribilo.  If not, see <http://www.gnu.org/licenses/>.

(define-module (skribilo-website)
  #:use-module (skribilo-package)
  #:use-module (guix build-system guile)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles))

(define skribilo-website-gexp
  (let ((development-profile
         (profile
          (content (package->development-manifest skribilo)))))
    (with-imported-modules %guile-build-system-modules
      #~(begin
          (use-modules (guix build guile-build-system)
                       (guix build utils))

          (define (patch-shebangs)
            (for-each patch-shebang
                      (find-files "."
                                  (lambda (file stat)
                                    (eq? 'regular (stat:type stat))))))

          (copy-recursively #$(package-source skribilo)
                            (getcwd))
          (set-path-environment-variable
           "PATH" (list "/bin") (list #$development-profile))
          (set-path-environment-variable
           "LIBRARY_PATH" (list "/lib") (list #$development-profile))
          (set-path-environment-variable
           "ACLOCAL_PATH" (list "/share/aclocal") (list #$development-profile))
          (set-path-environment-variable
           "PKG_CONFIG_PATH" (list "/lib/pkgconfig") (list #$development-profile))
          (set-path-environment-variable
           "GUILE_LOAD_PATH"
           (list (string-append "/share/guile/site/"
                                (target-guile-effective-version)))
           (list #$development-profile))
          (set-path-environment-variable
           "GUILE_LOAD_COMPILED_PATH"
           (list (string-append "/lib/guile/" (target-guile-effective-version) "/site-ccache"))
           (list #$development-profile))
          (substitute* "doc/user/Makefile.am"
            (("skribilo = [^\n]*")
             (string-append "skribilo = " #$(file-append skribilo "/bin/skribilo"))))
          (patch-shebangs)
          (invoke "autoreconf" "--verbose" "--install" "--force")
          (patch-shebangs)
          (invoke "./configure" "CONFIG_SHELL=sh" "SHELL=sh")
          (invoke "make"
                  "--directory" "doc/user")
          (invoke "make"
                  (string-append "SKRIBILO=" #$skribilo "/bin/skribilo")
                  "--directory" "web")
          ;; Install website.
          (for-each (lambda (file)
                      (install-file file (string-append #$output "/doc")))
                    (find-files "doc/user"
                                (lambda (file stat)
                                  (and (string=? (basename (dirname file)) "user")
                                       (or (string-suffix? ".html" file)
                                           (string-suffix? ".png" file))))))
          (copy-recursively "doc/user/static"
                            (string-append #$output "/doc/static"))
          (install-file "web/index.html" #$output)
          (copy-recursively "doc/user/static"
                            (string-append #$output "/static"))))))

(define-public skribilo-website
  (computed-file "skribilo-website" skribilo-website-gexp))

skribilo-website
