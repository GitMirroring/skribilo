;;; gemtext.scm  --  A reader for the Gemini protocol's Gemtext markup
;;;
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (skribilo reader gemtext)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 textual-ports) #:select (unget-char unget-string))
  #:use-module (skribilo reader)
  #:use-module (skribilo utils syntax)
  #:export (reader-specification
            make-gemtext-reader))

(skribilo-module-syntax)

;;; Author: Arun Isaac
;;;
;;; Commentary:
;;;
;;; A reader for gemtext, the lightweight markup language used by the
;;; Gemini protocol
;;;
;;; Code:

(define %join-lines?
  (make-parameter #f))

(define %section-numbers?
  (make-parameter #f))

(define (string-blank? str)
  "Return #t if STR contains only whitespace characters.  Else, return
#f."
  (string-every char-set:whitespace str))

(define (string-remove-prefix prefix str)
  "Return STR with PREFIX removed.  If PREFIX is not a prefix of STR,
return #f."
  (and (string-prefix? prefix str)
       (substring str (string-length prefix))))

(define (string-partition str char-pred)
  "Return the part of STR before and after the first occurrence of
CHAR-PRED as two values."
  (let ((partition-index (string-index str char-pred)))
    (if partition-index
        (values (substring str 0 partition-index)
                (substring str partition-index))
        (values str #f))))

(define (unget-line port line)
  "Place the string LINE in PORT so that subsequent read operations
will read LINE followed by a newline character."
  (unget-char port #\newline)
  (unget-string port line))

(define (read-preformatted-text in out)
  "Read preformatted text from port IN and write it to port OUT."
  (let ((line (get-line in)))
    (unless (or (eof-object? line)
                (string-prefix? "```" line))
      (put-string out line)
      (newline out)
      (read-preformatted-text in out))))

(define (heading-level line)
  "Return the level of the heading in LINE. If LINE is not a heading,
return #f."
  (cond
   ((string-prefix? "### " line) 3)
   ((string-prefix? "## " line) 2)
   ((string-prefix? "# " line) 1)
   (else #f)))

(define (read-section-children level port)
  "Read section elements of LEVEL from PORT. Return as a list."
  (let ((line (get-line port)))
    (cond
     ;; End of file
     ((eof-object? line) (list))
     ;; If another heading of same or higher level begins, unget line
     ;; and end section.
     ((let ((heading-level (heading-level line)))
        (and heading-level
             (<= heading-level level)))
      (unget-line port line)
      (list))
     ;; If blank line, continue.
     ((string-blank? line)
      (read-section-children level port))
     ;; Else, add element and continue.
     (else
      (unget-line port line)
      (cons (read-gemtext-element port)
            (read-section-children level port))))))

(define (paragraph-line? line)
  "Return #t if LINE is a paragraph line. Else, return #f."
  (not (or (string-blank? line)
           (heading-level line)
           (string-prefix? "* " line)
           (string-prefix? ">" line)
           (string-prefix? "=>" line)
           (string-prefix? "```" line))))

(define (link-line->item line)
  "Convert link LINE to a skribilo ref expression."
  (let* ((trimmed-line (string-trim (string-remove-prefix "=>" line)))
         (url text (string-partition trimmed-line (char-set #\space #\tab))))
    (if text
        `(item (ref #:url ,url #:text ,(string-trim text)))
        `(item (ref #:url ,url)))))

(define (retf-unget-line port result line)
  "Unget LINE to PORT and return RESULT. This function is used as an
argument to ttake-while."
  (unget-line port line)
  result)

(define (read-gemtext-element port)
  "Read next gemtext element from PORT."
  (let ((line (get-line port)))
    (cond
     ;; End of file
     ((eof-object? line) line)
     ;; Section
     ((heading-level line)
      => (lambda (level)
           `(,(case level
                ((1) 'section)
                ((2) 'subsection)
                ((3) 'subsubsection))
             #:title ,(substring line (1+ level))
             #:number ,(%section-numbers?)
             ,@(read-section-children level port))))
     ;; List
     ((string-remove-prefix "* " line)
      => (lambda (first-item)
           `(itemize
             ,@(port-transduce (compose (ttake-while (cut string-prefix? "* " <>)
                                                     (cut retf-unget-line port <> <>))
                                        (tmap (lambda (line)
                                                `(item ,(string-remove-prefix "* " line)))))
                               rcons
                               (list `(item ,first-item))
                               get-line
                               port))))
     ;; Blockquote
     ((string-remove-prefix ">" line)
      => (lambda (first-line)
           (list 'blockquote
                 (if (%join-lines?)
                     (string-join
                      (port-transduce (compose (ttake-while (cut string-prefix? ">" <>)
                                                            (cut retf-unget-line port <> <>))
                                               (tmap (cut string-remove-prefix ">" <>)))
                                      rcons
                                      (list first-line)
                                      get-line
                                      port)
                      " ")
                     line))))
     ;; Link
     ((string-prefix? "=>" line)
      (cons 'itemize
            (port-transduce (compose (ttake-while (cut string-prefix? "=>" <>)
                                                  (cut retf-unget-line port <> <>))
                                     (tmap link-line->item))
                            rcons
                            (list (link-line->item line))
                            get-line
                            port)))
     ;; Preformatted text
     ((string-remove-prefix "```" line)
      => (lambda (alt-text)
           ;; We don't use the alt text.
           `(pre ,(call-with-output-string
                    (cut read-preformatted-text port <>)))))
     ;; Ignore blank lines.
     ((string-blank? line) (read-gemtext-element port))
     ;; Paragraph
     (else
      (list 'paragraph
            (if (%join-lines?)
                (string-join
                 (port-transduce (ttake-while paragraph-line?
                                              (cut retf-unget-line port <> <>))
                                 rcons
                                 (list line)
                                 get-line
                                 port)
                 " ")
                line))))))

(define* (make-gemtext-reader :key join-lines? section-numbers?)
  "Return a gemtext reader.

If JOIN-LINES? is #t, lines which are not separated by a blank line
are joined into a single paragraph.

If SECTION-NUMBERS? is #t, sections are numbered. Else, they are not."
  (lambda (port)
    (parameterize ((%join-lines? join-lines?)
                   (%section-numbers? section-numbers?))
      (match (port-transduce (tmap identity)
                             rcons
                             read-gemtext-element
                             port)
        (() (eof-object))
        (elements `(document ,@elements))))))

(define-reader gemtext "0.1" make-gemtext-reader)
