;;; template.scm  --  Template system for bibliography entries.
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2006, 2007, 2015, 2018, 2022 Ludovic Courtès <ludo@gnu.org>
;;;
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

(define-module (skribilo biblio template)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)

  #:use-module (skribilo ast)
  #:autoload   (skribilo output) (output)
  #:use-module (skribilo biblio)

  #:use-module (ice-9 optargs)

  #:use-module (skribilo utils syntax)

  #:export (bibliography-template
            output-bib-entry-template
            make-bib-entry-template/default
            make-bib-entry-template/skribe))

(skribilo-module-syntax)

;;; Author: Manuel Serrano, Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides a helper procedure to output bibliography entries
;;; according to a given template, as well as ready-to-use templates.  A
;;; template only contains part of the style information for a bibliography
;;; entry.  Specific style information can be added by modifying the markup
;;; writers for `&bib-entry-author', `&bib-entry-title', etc. (see `(skribilo
;;; package base)' for details).
;;;
;;; Code:


;;;
;;; Outputting a bibliography entry template for a specific entry.
;;;

(define-syntax-rule (define-template-engine instantiate literal ...)
  "Define INSTANTIATE as a macro that, given a template, produces a
one-argument procedure to instantiate that template given a '&bib-entry'
node.  LITERAL... is the list of literals, the name of valid markup options."
  (begin
    (define-public literal
      (lambda (s)
        (syntax-violation 'literal
                          "template literal used outside of 'bibliography-template'"
                          s)))
    ...

    (define-syntax instantiate-body
      (lambda (s)
        (define (literal? id)
          (any (lambda (l)
                 (and (identifier? id)
                      (free-identifier=? id l)))
               #'(literal ...)))

        (syntax-case s (literal ... or if G_)
          ((_ node str rest (... ...))
           (string? (syntax->datum #'str))
           #'(cons str (instantiate-body node rest (... ...))))
          ((_ node (G_ str) rest (... ...))
           (string? (syntax->datum #'str))
           #'(cons (G_ str) (instantiate-body node rest (... ...))))
          ((_ node (or options (... ...)) rest (... ...))
           (every literal? #'(options (... ...)))
           #'(cons (or (markup-option node 'options) (... ...))
                   (instantiate-body node rest (... ...))))
          ((_ node (if cond a b) rest (... ...))
           (literal? #'cond)
           #'(cons (if (markup-option node 'cond)
                       (instantiate-body node a)
                       (instantiate-body node b))
                   (instantiate-body node rest (... ...))))
          ((_ node (lst (... ...)) rest (... ...))
           #'(append (let ((body (instantiate-body node lst (... ...))))
                       (if (every ->bool body)
                           body
                           '()))
                     (instantiate-body node rest (... ...))))
          ((_ node literal rest (... ...))
           #'(cons (markup-option node 'literal)
                   (instantiate-body node rest (... ...))))
          ...
          ((_ node)
           #''()))))

    (define-syntax-rule (instantiate body (... ...))
      (lambda (node)
        (instantiate-body node body (... ...))))))

;; Define 'bibliography-template' as a macro that builds a procedure to
;; instantiate a template from a '&bib-entry' node.
(define-template-engine bibliography-template

  ;; Keywords that may appear in the template.
  author title url documenturl type
  journal number volume series booktitle editor
  school institution address
  month year day
  pages chapter publisher
  doi note)


(define* (output-bib-entry-template bib engine template)
  ;; Output the fields of BIB (a bibliography entry) for ENGINE according to
  ;; TEMPLATE.  Example of templates are found below (e.g.,
  ;; `make-bib-entry-template/default').
  (output (template bib) engine))


;;;
;;; Example bibliography entry templates.
;;;

(define (make-bib-entry-template/default kind)
  ;; The default bibliography entry template.

  (case kind
    ((techreport)
     (bibliography-template author ". " (or title url documenturl) ". "
                            ;; TRANSLATORS: The next few msgids are fragments of
                            ;; bibliography items.
                            (G_ "Technical Report") " " number
                            (", " institution)
                            (", " address)
                            (", " month) " " year
                            (", pp. " pages) "."))
    ((article)
     (bibliography-template author ". " (or title url documenturl) ". "
                            (G_ "In ") journal ", " volume
                            ("(" number ") ")", "
                            (address ", ") month " " year ", "
                            ("pp. " pages) "."))
    ((inproceedings)
     (bibliography-template author ". " (or title url documenturl) ". "
                            (G_ "In ") booktitle ", "
                            (series ", ")
                            ("(" number ")")
                            ("pp. " pages ", ")
                            (publisher ", ")
                            (month " ") year "."))
    ((book) ;; FIXME:  Title should be in italics
     (bibliography-template (or author editor)
                            ". " (or title url documenturl) ". "
                            publisher
                            (", " address)
                            (", " month)
                            ", " year
                            (", pp. " pages) "."))
    ((inbook)
     (bibliography-template author ". " (or title url documenturl) ". "
                            (G_ "In ") booktitle ", " publisher
                            (", " editor " (" (G_ "editor") ")")
                            (", " (G_ "Chapter ") chapter)
                            (", pp. " pages) ", "
                            (month " ") year "."))
    ((phdthesis)
     (bibliography-template author ". " (or title url documenturl)
                            ", " (G_ "PhD Thesis")
                            (", " (or school institution))
                            (", " address)
                            (", " month)
                            (if month " " ", ") year "."))
    ((misc)
     (bibliography-template author ". " (or title url documenturl) ". "
                            (institution ", ")
                            (publisher ", ")
                            (address ", ")
                            (month " ") year ". "
                            (url ".")))
    (else
     (bibliography-template author ". " (or title url documenturl) ". "
                            (publisher ", ")
                            (address ", ")
                            (month " ") year ", "
                            ("pp. " pages) "."))))

(define (make-bib-entry-template/skribe kind)
  ;; The awful template found by default in Skribe.
  (case kind
    ((techreport)
     (bibliography-template author " -- " (or title url documenturl) " -- "
                            (G_ "Technical Report") " " number ", " institution ", "
                            address ", " month ", " year ", "
                            ("pp. " pages) "."))
    ((article)
     (bibliography-template author " -- " (or title url documenturl) " -- "
                            journal ", " volume "" ("(" number ")") ", "
                            address ", " month ", " year ", "
                            ("pp. " pages) "."))
    ((inproceedings)
     (bibliography-template author " -- " (or title url documenturl) " -- "
                            booktitle ", " series ", " ("(" number ")") ", "
                            address ", " month ", " year ", "
                            ("pp. " pages) "."))
    ((book)
     (bibliography-template author " -- " (or title url documenturl) " -- "
                            publisher ", " address
                            ", " month ", " year ", " ("pp. " pages) "."))
    ((phdthesis)
     (bibliography-template author " -- " (or title url documenturl) " -- " type ", "
                            school ", " address
                            ", " month ", " year"."))
    ((misc)
     (bibliography-template author " -- " (or title url documenturl) " -- "
                            publisher ", " address
                            ", " month ", " year"."))
    (else
     (bibliography-template author " -- " (or title url documenturl) " -- "
                            publisher ", " address
                            ", " month ", " year ", " ("pp. " pages) "."))))


;;; arch-tag: 5931579f-b606-442d-9a45-6047c94da5a2

;;; template.scm ends here
