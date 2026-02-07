;;; html.scm  --  HTML implementation of the `slide' package.
;;;
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2026 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (skribilo package slide html)
  #:use-module (skribilo utils syntax)

  #:use-module (skribilo ast)
  #:use-module (skribilo engine)
  #:use-module (skribilo writer)
  #:autoload   (skribilo resolve)     (resolve!)
  #:autoload   (skribilo output)      (output)
  #:autoload   (skribilo evaluator)   (evaluate-document)
  #:autoload   (skribilo engine html) (html-open html-close style-declaration
                                       html-width html-title-authors
                                       html-color-spec?)

  #:use-module (skribilo package slide)
  #:use-module (skribilo package base))


(skribilo-module-syntax)



(define-public (%slide-html-initialize!)
  (let ((he (find-engine 'html)))
    (display "HTML slides setup...\n" (current-error-port))

    ;; &html-page-title
    (markup-writer '&html-document-title he
       ;;:predicate (lambda (n e) %slide-initialized)
       :action html-slide-title)

    ;; slide
    (markup-writer 'slide he
       :options '(:title :number :transition :toc :bg)
       :before (lambda (n e)
                  (display "<br>\n")
		  (format #t "<a name=\"~a\">" (markup-ident n)))
       :action (lambda (n e)
		  (let ((nb (markup-option n :number))
			(t (markup-option n :title))
                        (class (markup-class n)))
                     (if class
                         (let ((title-class (string-append class "-title")))
                           ;; When a class is specified, let the user play
                           ;; with CSS.
                           (format #t "\n<div class=\"~a\">" class)
                           (format #t "<div class=\"~a\">" title-class)
                           (format #t "~a / ~a -- " nb (slide-number))
                           (output t e)
                           (display "</div>\n")
                           (output (markup-body n) e)
                           (display "\n</div>\n"))
                         ;; When no class is specified, do HTML tricks.
                         (evaluate-document
                          (center
                           (color :width (slide-body-width e)
                                  :bg (or (markup-option n :bg) "#ffffff")
                                  (table :width 100.
                                         (tr (th :align 'left
                                                 (list
                                                  (if nb
                                                      (format #f "~a / ~a -- "
                                                              nb
                                                              (slide-number)))
                                                  t)))
                                         (tr (td (hrule)))
                                         (tr (td :width 100. :align 'left
                                                 (markup-body n))))
                                  (linebreak)))
                          e))))
       :after "<br>")

    ;; slide-vspace
    (markup-writer 'slide-vspace he
       :action (lambda (n e) (display "<br>")))))


;*---------------------------------------------------------------------*/
;*    slide-body-width ...                                             */
;*---------------------------------------------------------------------*/
(define (slide-body-width e)
   (let ((w (engine-custom e 'body-width)))
      (if (or (number? w) (string? w)) w 95.)))

;*---------------------------------------------------------------------*/
;*    html-slide-title ...                                             */
;*---------------------------------------------------------------------*/
(define (html-slide-title n e)
  (let* ((title (markup-body n))
	 (authors (markup-option n 'author))
	 (title-background (engine-custom e 'title-background))
	 (title-foreground (engine-custom e 'title-foreground))
	 (title-font (engine-custom e 'title-font)))
    (when title
      (html-open 'h1
                 `((class . "skribilo-title")
                   (style . ,(style-declaration
                              `((color . ,(and (html-color-spec? title-foreground)
                                               title-foreground))
                                (background-color . ,(and (html-color-spec? title-background)
                                                          title-background))
                                (font-family . ,title-font)
                                (width . ,(html-width (slide-body-width e))))))))
      (output title e)
      (html-close 'h1))
    (when authors
      (html-title-authors authors e))))



;;;
;;; Slide topics/subtopics.
;;;

(markup-writer 'slide-topic (find-engine 'html)
   :options '(:title :outline? :class :ident)
   :action (lambda (n e)
	      (let ((title (markup-option n :title))
                    (class (markup-class n)))
                 ;; top-level class
                 (if class (format #t "\n<div class=\"~a\">" class))

                 ;; the title
                 (if class
                     (format #t "\n<div class=\"~a-title\">" class)
                     (display "\n<h2 class=\"slide-topic:title\">"))
		 (if (markup-ident n)
		     (format #t "<a name=\"~a\"></a>" (markup-ident n)))
		 (output title e)
                 (if class
                     (display "</div>\n")
                     (display "</h2> <br>\n"))

                 ;; pointers to the slides
                 (if class
                     (format #t "\n<div class=\"~a-slide-list\">"
                             class)
                     (display "\n<div class=\"slide-topic:slide-list\">"))
		 (for-each (lambda (s)
                             (let* ((title (markup-option s :title))
                                    (ident (markup-ident s))
                                    (sref (ref :text title :ident ident))
                                    (sref* (resolve! sref e `((parent ,n)))))
                               (output sref* e)
                               (display "&nbsp;--&nbsp;")))
			   (filter (lambda (n)
				      (or (is-markup? n 'slide-subtopic)
					  (is-markup? n 'slide)))
				   (markup-body n)))
		 (display "\n</div> <!-- slide-topic:slide-list -->")

                 (if class
                     (display "\n</div> <!-- slide-topic -->\n")
                     (display "\n<hr><br>\n"))

		 ;; the slides
		 (output (markup-body n) e))))


;;;
;;; Initialization.
;;;

(%slide-html-initialize!)


;;; arch-tag: 8be0cdf2-b755-4baa-baf6-739cdd00e193
