;;; commonmark.scm  --  Reader for the CommonMark syntax.
;;;
;;; Copyright © 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (skribilo reader commonmark)
  #:autoload   (commonmark) (commonmark->sxml)
  #:autoload   (sxml match) (sxml-match)
  #:use-module (skribilo reader)
  #:use-module (skribilo utils syntax)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:export (reader-specification
            make-commonmark-reader))

;;; Commentary:
;;;
;;; This is a reader for the CommonMark syntax, a subset of what's usually
;;; referred to as "Markdown".
;;;
;;; Code:

(define (sxml->skribilo sxml)
  (define sectioning
    `((h1 . chapter)
      (h2 . section)
      (h3 . subsection)
      (h4 . subsubsection)))

  (define (heading? tag)
    (assq tag sectioning))

  (define (tagged? tag)
    (match-lambda
      ((head . _) (eq? head tag))
      (_ #f)))

  (let loop ((sxml sxml)
             (result '()))
    (match sxml
      (()
       (reverse result))
      ((((? heading? tag) . title) . rest)
       (let ((section (assoc-ref sectioning tag)))
         (let-values (((body rest)
                       (break (tagged? tag) rest)))
           (loop rest
                 (cons `(,section #:title (list ,@title)
                                  ,@(sxml->skribilo body))
                       result)))))
      ((('p . body) . rest)
       (loop rest
             (cons `(paragraph ,@(sxml->skribilo body))
                   result)))
      ((('pre ('code ('@ _ ...) . body)) . rest)
       (loop rest
             (cons `(pre (prog ,@body)) result)))
      ((('em . body) . rest)
       (loop rest
             (cons `(emph ,@(sxml->skribilo body))
                   result)))
      ((('ul ('li . items) ...) . rest)
       (loop rest
             (cons `(itemize ,@(map (lambda (body)
                                      `(item ,@(sxml->skribilo body)))
                                    items))
                   result)))
      ((('ol ('li . items) ...) . rest)
       (loop rest
             (cons `(enumerate ,@(map (lambda (body)
                                        `(item ,@(sxml->skribilo body)))
                                      items))
                   result)))
      ((('a ('@ ('href url)) . body) . rest)
       (loop rest
             (cons `(ref #:url ,url
                         #:text (list ,@(sxml->skribilo body)))
                   result)))
      ((('code . body) . rest)
       (loop rest
             (cons `(tt ,@(sxml->skribilo body))
                   result)))
      ((('blockquote . body) . rest)
       (loop rest
             (cons `(blockquote ,@(sxml->skribilo body))
                   result)))
      ((('strong . body) . rest)
       (loop rest
             (cons `(bold ,@(sxml->skribilo body))
                   result)))
      ((('img ('@ ('src url) ('alt text) . _) . _) . rest)
       (loop rest
             (cons `(image #:url ,url #:text ,text)
                   result)))
      ((('hr) . rest)
       (loop rest result))
      (((? string? str) . rest)
       (loop rest (cons str result)))
      ((lst ...)
       `(list ,@(fold loop result lst))))))

(define (string-split-at str char-pred)
  "Split STR at the first character that matches CHAR-PRED and return
a list of one or two strings.  Two strings are returned if the string
was able to be split, with the character matching CHAR-PRED removed.
A list containing only STR is returned if CHAR-PRED does not match any
charcter."
  (let ((i (string-index str char-pred)))
    (if i
        (list (string-take str i)
              (string-drop str (1+ i)))
        (list str))))

(define (read-headers port)
  "Read Haunt-style headers from PORT.  Return a list of key/value pairs."
  (let loop ((headers '()))
    (match (read-line port)
      ((? eof-object?)                            ;premature end-of-file
       (reverse headers))
      ("---"
       (reverse headers))
      (line
       (match (map string-trim-both (string-split-at line #\:))
         (((= string->symbol key) value)
          (loop (alist-cons key value headers)))
         (_
          (raise (condition
                  (&message
                   (message (G_ "invalid CommonMark header")))))))))))

(define (starts-with-headers? port)
  "Return true if PORT starts with Haunt-style headers.  Do not actually
consume those bytes."
  (let ((line (read-line port 'concat)))
    (and (not (eof-object? line))
         (begin
           (unread-string line port)
           (string-prefix? "title:" (string-trim line))))))

(define (raise-sectioning body)
  "Raise sectioning in BODY: sections become chapters, subsections become
section, and so on."
  (define mapping
    '((subsubsection . subsection)
      (subsection . section)
      (section . chapter)
      (chapter . chapter)))

  (define (section? tag)
    (assq tag mapping))

  (map (lambda (item)
         (match item
           (((? section? tag) #:title title . body)
            (let ((tag (assq-ref mapping tag)))
              `(,tag #:title ,title
                     ,@(raise-sectioning body))))
           (_
            item)))
       body))

(define (read-commonmark-document port)
  "Read a CommonMark document from PORT.  Return Scheme code as an s-expression."
  (set-port-encoding! port "UTF-8")
  (match (peek-char port)
    ((? eof-object? eof)
     eof)
    (_
     (let* ((headers (if (starts-with-headers? port)
                         (read-headers port)
                         '()))
            (sexp (sxml->skribilo (commonmark->sxml port)))
            (title (or (assoc-ref headers 'title)
                       (match sexp
                         ((('chapter #:title title _ ...) _ ...)
                          title)
                         (_ #f))))
            (body (if (assoc-ref headers 'title)
                      sexp
                      (match sexp
                        ((('chapter #:title _ body ...))
                         body)
                        (_
                         sexp)))))
       `(document
         #:title ,title
         #:author (list ,@(map (lambda (name)
                                 `(author #:name ,name))
                               (if (assoc-ref headers 'author)
                                   (string-split (assoc-ref headers 'author)
                                                 #\,)
                                   '())))

         ,@(if (assoc-ref headers 'title)
               body
               (raise-sectioning body)))))))

(define make-commonmark-reader
  (const read-commonmark-document))

(define-reader commonmark "0.1" make-commonmark-reader)
