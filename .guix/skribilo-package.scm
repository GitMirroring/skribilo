;;; guix.scm  --  Build recipe for GNU Guix.
;;;
;;; Copyright © 2020, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2023–2024 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (skribilo-package)
  #:use-module ((gnu packages autotools) #:select (autoconf automake))
  #:use-module ((gnu packages gettext) #:select (gnu-gettext))
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages guile-xyz)
                #:select (guile-lib guile-reader))
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages lout)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages plotutils)
  #:use-module ((gnu packages skribilo) #:select (skribilo) #:prefix guix:)
  #:use-module ((guix build-system guile) #:select (%guile-build-system-modules))
  #:use-module (guix gexp)
  #:use-module ((guix git-download) #:select (git-predicate))
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix utils))

(define-public skribilo
  (package
    (inherit guix:skribilo)
    (version (string-append (package-version guix:skribilo) "-git"))
    (source (local-file ".." "skribilo-checkout"
                        #:recursive? #t
                        #:select?
                        (or (git-predicate (dirname (current-source-directory)))
                            (const #t))))
    (native-inputs (list autoconf automake gnu-gettext pkg-config))
    (inputs (list guile-3.0
                  imagemagick
                  ghostscript ; for 'convert'
                  ploticus
                  lout))

    ;; The 'skribilo' command needs them, and for people using Skribilo as a
    ;; library, these inputs are needed as well.
    (propagated-inputs (list guile-reader guile-lib))))

(define with-guile-2.0
  (package-input-rewriting/spec `(("guile" . ,(const guile-2.0)))
                                #:deep? #f))

(define with-guile-2.2
  (package-input-rewriting/spec `(("guile" . ,(const guile-2.2)))
                                #:deep? #f))

(define-public skribilo/guile-2.2
  (package
    (inherit (with-guile-2.2 skribilo))
    (name "guile2.2-skribilo")
    ;; Arrange to not trigger a rebuild of Automake & co.
    (inputs
     (modify-inputs (package-inputs skribilo)
       (replace "guile" guile-2.2)))
    (native-inputs
     (package-native-inputs skribilo))))

(define-public skribilo/guile-2.0
  (package
    (inherit (with-guile-2.0 skribilo))
    (name "guile2.0-skribilo")
    (inputs
     (modify-inputs (package-inputs skribilo)
       (replace "guile" guile-2.0)))
    (native-inputs
     (package-native-inputs skribilo))))

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

skribilo
