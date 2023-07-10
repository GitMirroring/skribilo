;;; guix.scm  --  Build recipe for GNU Guix.
;;;
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2023 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module (gnu packages)
  #:use-module ((gnu packages autotools) #:select (autoconf automake))
  #:use-module ((gnu packages gettext) #:select (gnu-gettext))
  #:use-module ((gnu packages skribilo) #:select (skribilo) #:prefix guix:)
  #:use-module (guix gexp)
  #:use-module ((guix git-download) #:select (git-predicate))
  #:use-module (guix packages)
  #:use-module (guix utils))

(define S specification->package)

(define-public skribilo
  (package
    (inherit guix:skribilo)
    (version (string-append (package-version guix:skribilo) "-git"))
    (source (local-file ".." "skribilo-checkout"
                        #:recursive? #t
                        #:select?
                        (or (git-predicate (dirname (current-source-directory)))
                            (const #t))))
    (native-inputs
     (modify-inputs (package-native-inputs guix:skribilo)
       (prepend autoconf)
       (prepend automake)
       (prepend gnu-gettext)))))

(define with-guile-2.0
  (package-input-rewriting/spec `(("guile" . ,(const (S "guile@2.0"))))
                                #:deep? #f))

(define with-guile-2.2
  (package-input-rewriting/spec `(("guile" . ,(const (S "guile@2.2"))))
                                #:deep? #f))

(define-public skribilo/guile-2.2
  (package
    (inherit (with-guile-2.2 skribilo))
    (name "guile2.2-skribilo")
    ;; Arrange to not trigger a rebuild of Automake & co.
    (inputs
     (modify-inputs (package-inputs skribilo)
       (replace "guile" (S "guile@2.2"))))
    (native-inputs
     (package-native-inputs skribilo))))

(define-public skribilo/guile-2.0
  (package
    (inherit (with-guile-2.0 skribilo))
    (name "guile2.0-skribilo")
    (inputs
     (modify-inputs (package-inputs skribilo)
       (replace "guile" (S "guile@2.0"))))
    (native-inputs
     (package-native-inputs skribilo))))

skribilo
