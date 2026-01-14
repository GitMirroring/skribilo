;;; html.scm  --  HTML engine.
;;;
;;; Copyright 2005, 2006, 2007, 2008, 2009, 2011, 2012, 2018, 2020  Ludovic Courtès <ludo@gnu.org>
;;; Copyright 2003, 2004  Manuel Serrano
;;; Copyright 2022, 2023, 2026 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (skribilo engine html)
  #:use-module (skribilo lib)
  #:use-module (skribilo ast)
  #:use-module (skribilo config)
  #:use-module (skribilo engine)
  #:use-module (skribilo writer)
  #:use-module (skribilo location)
  #:use-module (skribilo utils strings)
  #:use-module (skribilo utils syntax)
  #:use-module (skribilo package base)
  #:autoload   (skribilo utils images)  (convert-image)
  #:autoload   (skribilo utils files)   (file-prefix file-suffix)
  #:autoload   (skribilo parameters)    (*destination-file*)
  #:autoload   (skribilo evaluator)     (evaluate-document)
  #:autoload   (skribilo output)        (output)
  #:autoload   (skribilo debug)         (*debug*)
  #:autoload   (skribilo sui)           (document-sui)
  #:autoload   (ice-9 rdelim)           (read-line)
  #:autoload   (ice-9 regex)            (regexp-substitute/global)
  #:use-module (ice-9 match)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-39)

  #:use-module (rnrs exceptions)
  #:use-module (rnrs io ports)

  #:export (html-engine html-title-engine html-file
           html-width html-class html-markup-class
           html-title-authors))

(skribilo-module-syntax)



;; Keep a reference to the base engine.
(define base-engine (find-engine 'base))

(if (not (engine? base-engine))
    (error "bootstrap problem: base engine broken" base-engine))

(define unspecified?
  ;; XXX: Hack to recognize the unspecified value as understood by
  ;; `engine-custom' et al.
  (let ((really-unspecified? (@ (guile) unspecified?)))
    (lambda (x)
      (or (really-unspecified? x)
          (eq? x 'unspecified)))))

;*---------------------------------------------------------------------*/
;*    html-file-default ...                                            */
;*---------------------------------------------------------------------*/
(define html-file-default
   ;; Default implementation of the `file-name-proc' custom.
   (let ((table '())
	 (filename (gensym "filename")))
      (define (get-file-name base suf)
	(let* ((c (assoc base table))
	       (n (if (pair? c)
		      (let ((n (+ 1 (cdr c))))
			 (set-cdr! c n)
			 n)
		      (begin
			 (set! table (cons (cons base 1) table))
			 1))))
	   (format #f "~a-~a.~a" base n suf)))
      (lambda (node engine)
	(let ((f (markup-option node filename))
	      (file (markup-option node :file)))
	   (cond
	      ((string? f)
	       f)
	      ((string? file)
	       file)
	      ((or file
		   (and (is-markup? node 'chapter)
			(engine-custom engine 'chapter-file))
		   (and (is-markup? node 'section)
			(engine-custom engine 'section-file))
		   (and (is-markup? node 'subsection)
			(engine-custom engine 'subsection-file))
		   (and (is-markup? node 'subsubsection)
			(engine-custom engine 'subsubsection-file)))
	       (let* ((b (or (and (string? (*destination-file*))
				  (file-prefix (*destination-file*)))
			     ""))
		      (s (or (and (string? (*destination-file*))
				  (file-suffix (*destination-file*)))
			     "html"))
		      (nm (get-file-name b s)))
		  (markup-option-add! node filename nm)
		  nm))
	      ((document? node)
	       (*destination-file*))
	      (else
	       (let ((p (ast-parent node)))
		  (if (container? p)
		      (let ((file (html-file p engine)))
			 (if (string? file)
			     (begin
				(markup-option-add! node filename file)
				file)
			     #f))
		      #f))))))))

;*---------------------------------------------------------------------*/
;*    html-engine ...                                                  */
;*---------------------------------------------------------------------*/
(define html-engine
   ;; setup the html engine
   (default-engine-set!
      (make-engine 'html
	 :version 1.0
	 :format "html"
	 :delegate (find-engine 'base)
	 :filter (make-string-replace '((#\< "&lt;")
					(#\> "&gt;")
					(#\& "&amp;")
					(#\" "&quot;")
					(#\@ "&#x40;")))
	 :custom `(;; the icon associated with the URL
		   (favicon #f)
		   ;; charset used
		   (charset "UTF-8")
		   ;; enable/disable Javascript
		   (javascript #f)
		   ;; user html head
		   (head #f)
		   ;; user CSS
		   (css ())
		   ;; user inlined CSS
		   (inline-css ())
		   ;; user JS
		   (js ())
		   ;; emit-sui
		   (emit-sui #f)
		   ;; the body
		   (background #f)
		   (foreground #f)
		   ;; the margins
		   (margin-padding 3)
		   (left-margin #f)
		   (chapter-left-margin #f)
		   (section-left-margin #f)
		   (left-margin-font #f)
		   (left-margin-size 17.)
		   (left-margin-background #f)
		   (left-margin-foreground #f)
		   (right-margin #f)
		   (chapter-right-margin #f)
		   (section-right-margin #f)
		   (right-margin-font #f)
		   (right-margin-size 17.)
		   (right-margin-background #f)
		   (right-margin-foreground #f)
		   ;; author configuration
		   (author-font #f)
		   ;; title configuration
		   (title-font #f)
		   (title-background #f)
		   (title-foreground #f)
		   (file-title-separator ,(! " &#8212; ")) ;; an "em dash"
		   ;; html file naming
		   (file-name-proc ,html-file-default)
		   ;; index configuration
		   (index-header-font-size #f) ;; +2.
		   ;; chapter configuration
		   (chapter-number->string number->string)
		   (chapter-file #f)
		   ;; section configuration
		   (section-title-start "<h2>")
		   (section-title-stop "</h2>")
		   (section-title-background #f)
		   (section-title-foreground #f)
		   (section-title-number-separator " ")
		   (section-number->string number->string)
		   (section-file #f)
		   ;; subsection configuration
		   (subsection-title-start "<h3>")
		   (subsection-title-stop "</h3>")
		   (subsection-title-background #f)
		   (subsection-title-foreground #f)
		   (subsection-title-number-separator " ")
		   (subsection-number->string number->string)
		   (subsection-file #f)
		   ;; subsubsection configuration
		   (subsubsection-title-start "<h4>")
		   (subsubsection-title-stop "</h4>")
		   (subsubsection-title-background #f)
		   (subsubsection-title-foreground #f)
		   (subsubsection-title-number-separator " ")
		   (subsubsection-number->string number->string)
		   (subsubsection-file #f)
		   ;; source fontification
		   (source-color #t)
		   (source-comment-color "#ffa600")
		   (source-error-color "red")
		   (source-define-color "#6959cf")
		   (source-module-color "#1919af")
		   (source-markup-color "#1919af")
		   (source-thread-color "#ad4386")
		   (source-string-color "red")
		   (source-bracket-color "red")
		   (source-type-color "#00cf00")
		   ;; image
		   (image-format ("png" "gif" "jpg" "jpeg")))
	 :symbol-table '(("iexcl" "&#161;")
			 ("cent" "&#162;")
			 ("pound" "&#163;")
			 ("currency" "&#164;")
			 ("yen" "&#165;")
			 ("section" "&#167;")
			 ("mul" "&#168;")
			 ("copyright" "&#169;")
			 ("female" "&#170;")
			 ("lguillemet" "&#171;")
			 ("not" "&#172;")
			 ("registered" "&#174;")
			 ("degree" "&#176;")
			 ("plusminus" "&#177;")
			 ("micro" "&#181;")
			 ("paragraph" "&#182;")
			 ("middot" "&#183;")
			 ("male" "&#184;")
			 ("rguillemet" "&#187;")
			 ("1/4" "&#188;")
			 ("1/2" "&#189;")
			 ("3/4" "&#190;")
			 ("iquestion" "&#191;")
			 ("Agrave" "&#192;")
			 ("Aacute" "&#193;")
			 ("Acircumflex" "&#194;")
			 ("Atilde" "&#195;")
			 ("Amul" "&#196;")
			 ("Aring" "&#197;")
			 ("AEligature" "&#198;")
			 ("Oeligature" "&#338;")
			 ("Ccedilla" "&#199;")
			 ("Egrave" "&#200;")
			 ("Eacute" "&#201;")
			 ("Ecircumflex" "&#202;")
			 ("Euml" "&#203;")
			 ("Igrave" "&#204;")
			 ("Iacute" "&#205;")
			 ("Icircumflex" "&#206;")
			 ("Iuml" "&#207;")
			 ("ETH" "&#208;")
			 ("Ntilde" "&#209;")
			 ("Ograve" "&#210;")
			 ("Oacute" "&#211;")
			 ("Ocurcumflex" "&#212;")
			 ("Otilde" "&#213;")
			 ("Ouml" "&#214;")
			 ("times" "&#215;")
			 ("Oslash" "&#216;")
			 ("Ugrave" "&#217;")
			 ("Uacute" "&#218;")
			 ("Ucircumflex" "&#219;")
			 ("Uuml" "&#220;")
			 ("Yacute" "&#221;")
			 ("THORN" "&#222;")
			 ("szlig" "&#223;")
			 ("agrave" "&#224;")
			 ("aacute" "&#225;")
			 ("acircumflex" "&#226;")
			 ("atilde" "&#227;")
			 ("amul" "&#228;")
			 ("aring" "&#229;")
			 ("aeligature" "&#230;")
			 ("oeligature" "&#339;")
			 ("ccedilla" "&#231;")
			 ("egrave" "&#232;")
			 ("eacute" "&#233;")
			 ("ecircumflex" "&#234;")
			 ("euml" "&#235;")
			 ("igrave" "&#236;")
			 ("iacute" "&#237;")
			 ("icircumflex" "&#238;")
			 ("iuml" "&#239;")
			 ("eth" "&#240;")
			 ("ntilde" "&#241;")
			 ("ograve" "&#242;")
			 ("oacute" "&#243;")
			 ("ocurcumflex" "&#244;")
			 ("otilde" "&#245;")
			 ("ouml" "&#246;")
			 ("divide" "&#247;")
			 ("oslash" "&#248;")
			 ("ugrave" "&#249;")
			 ("uacute" "&#250;")
			 ("ucircumflex" "&#251;")
			 ("uuml" "&#252;")
			 ("yacute" "&#253;")
			 ("thorn" "&#254;")
			 ("ymul" "&#255;")
			 ;; Greek
			 ("Alpha" "&#913;")
			 ("Beta" "&#914;")
			 ("Gamma" "&#915;")
			 ("Delta" "&#916;")
			 ("Epsilon" "&#917;")
			 ("Zeta" "&#918;")
			 ("Eta" "&#919;")
			 ("Theta" "&#920;")
			 ("Iota" "&#921;")
			 ("Kappa" "&#922;")
			 ("Lambda" "&#923;")
			 ("Mu" "&#924;")
			 ("Nu" "&#925;")
			 ("Xi" "&#926;")
			 ("Omicron" "&#927;")
			 ("Pi" "&#928;")
			 ("Rho" "&#929;")
			 ("Sigma" "&#931;")
			 ("Tau" "&#932;")
			 ("Upsilon" "&#933;")
			 ("Phi" "&#934;")
			 ("Chi" "&#935;")
			 ("Psi" "&#936;")
			 ("Omega" "&#937;")
			 ("alpha" "&#945;")
			 ("beta" "&#946;")
			 ("gamma" "&#947;")
			 ("delta" "&#948;")
			 ("epsilon" "&#949;")
			 ("zeta" "&#950;")
			 ("eta" "&#951;")
			 ("theta" "&#952;")
			 ("iota" "&#953;")
			 ("kappa" "&#954;")
			 ("lambda" "&#955;")
			 ("mu" "&#956;")
			 ("nu" "&#957;")
			 ("xi" "&#958;")
			 ("omicron" "&#959;")
			 ("pi" "&#960;")
			 ("rho" "&#961;")
			 ("sigmaf" "&#962;")
			 ("sigma" "&#963;")
			 ("tau" "&#964;")
			 ("upsilon" "&#965;")
			 ("phi" "&#966;")
			 ("chi" "&#967;")
			 ("psi" "&#968;")
			 ("omega" "&#969;")
			 ("thetasym" "&#977;")
			 ("piv" "&#982;")
			 ;; punctuation
			 ("bullet" "&#8226;")
			 ("ellipsis" "&#8230;")
			 ("weierp" "&#8472;")
			 ("image" "&#8465;")
			 ("real" "&#8476;")
			 ("tm" "&#8482;")
			 ("alef" "&#8501;")
			 ("<-" "&#8592;")
			 ("<--" "&#8592;")
			 ("uparrow" "&#8593;")
			 ("->" "&#8594;")
			 ("-->" "&#8594;")
			 ("downarrow" "&#8595;")
			 ("<->" "&#8596;")
			 ("<-->" "&#8596;")
			 ("<+" "&#8629;")
			 ("<=" "&#8656;")
			 ("<==" "&#8656;")
			 ("Uparrow" "&#8657;")
			 ("=>" "&#8658;")
			 ("==>" "&#8658;")
			 ("Downarrow" "&#8659;")
			 ("<=>" "&#8660;")
			 ("<==>" "&#8660;")
			 ;; Mathematical operators
			 ("forall" "&#8704;")
			 ("partial" "&#8706;")
			 ("exists" "&#8707;")
			 ("emptyset" "&#8709;")
			 ("infinity" "&#8734;")
			 ("nabla" "&#8711;")
			 ("in" "&#8712;")
			 ("notin" "&#8713;")
			 ("ni" "&#8715;")
			 ("prod" "&#8719;")
			 ("sum" "&#8721;")
			 ("asterisk" "&#8727;")
			 ("sqrt" "&#8730;")
			 ("propto" "&#8733;")
			 ("angle" "&#8736;")
			 ("and" "&#8743;")
			 ("or" "&#8744;")
			 ("cap" "&#8745;")
			 ("cup" "&#8746;")
			 ("integral" "&#8747;")
			 ("therefore" "&#8756;")
			 ("models" "|=")
			 ("vdash" "|-")
			 ("dashv" "-|")
			 ("sim" "&#8764;")
			 ("cong" "&#8773;")
			 ("approx" "&#8776;")
			 ("neq" "&#8800;")
			 ("equiv" "&#8801;")
			 ("le" "&#8804;")
			 ("ge" "&#8805;")
			 ("subset" "&#8834;")
			 ("supset" "&#8835;")
			 ("nsupset" "&#8835;")
			 ("subseteq" "&#8838;")
			 ("supseteq" "&#8839;")
			 ("oplus" "&#8853;")
			 ("otimes" "&#8855;")
			 ("perp" "&#8869;")
			 ("mid" "|")
			 ("lceil" "&#8968;")
			 ("rceil" "&#8969;")
			 ("lfloor" "&#8970;")
			 ("rfloor" "&#8971;")
			 ("langle" "&#9001;")
			 ("rangle" "&#9002;")
			 ;; Misc
			 ("loz" "&#9674;")
			 ("spades" "&#9824;")
			 ("clubs" "&#9827;")
			 ("hearts" "&#9829;")
			 ("diams" "&#9830;")
			 ("euro" "&#8464;")
			 ;; LaTeX
			 ("dag" "dag")
			 ("ddag" "ddag")
			 ("circ" "o")
			 ("top" "T")
			 ("bottom" "&#8869;")
			 ("lhd" "<")
			 ("rhd" ">")
			 ("parallel" "||")))))

;*---------------------------------------------------------------------*/
;*    html-file ...                                                    */
;*---------------------------------------------------------------------*/
(define (html-file node engine)
  (let ((proc (or (engine-custom engine 'file-name-proc)
                  html-file-default)))
    (proc node engine)))

;*---------------------------------------------------------------------*/
;*    html-title-engine ...                                            */
;*---------------------------------------------------------------------*/
(define html-title-engine
   (copy-engine 'html-title base-engine
      :filter (make-string-replace '((#\< "&lt;")
				     (#\> "&gt;")
				     (#\& "&amp;")
				     (#\" "&quot;")))))

;*---------------------------------------------------------------------*/
;*    html-browser-title ...                                           */
;*---------------------------------------------------------------------*/
(define (html-browser-title node)
   (and (markup? node)
	(or (markup-option node :html-title)
	    (if (document? node)
		(markup-option node :title)
		(html-browser-title (ast-parent node))))))


;*---------------------------------------------------------------------*/
;*    html-container-number ...                                        */
;*    -------------------------------------------------------------    */
;*    Returns a string representing the container number               */
;*---------------------------------------------------------------------*/
(define (html-container-number c engine)
   (define (html-number n proc)
      (cond
	 ((string? n)
	  n)
	 ((number? n)
	  (if (procedure? proc)
	      (proc n)
	      (number->string n)))
	 (else
	  "")))
   (define (html-chapter-number c)
      (html-number (markup-option c :number)
		   (engine-custom engine 'chapter-number->string)))
   (define (html-section-number c)
      (let ((p (ast-parent c))
	    (s (html-number (markup-option c :number)
			    (engine-custom engine 'section-number->string))))
	 (cond
	    ((is-markup? p 'chapter)
	     (string-append (html-chapter-number p) "." s))
	    (else
	     s))))
   (define (html-subsection-number c)
      (let ((p (ast-parent c))
	    (s (html-number (markup-option c :number)
			    (engine-custom engine 'subsection-number->string))))
	 (cond
	    ((is-markup? p 'section)
	     (string-append (html-section-number p) "." s))
	    (else
	     (string-append "." s)))))
   (define (html-subsubsection-number c)
      (let ((p (ast-parent c))
	    (s (html-number (markup-option c :number)
			    (engine-custom engine 'subsubsection-number->string))))
	 (cond
	    ((is-markup? p 'subsection)
	     (string-append (html-subsection-number p) "." s))
	    (else
	     (string-append ".." s)))))
   (define (inner-html-container-number c)
      (html-number (markup-option c :number) #f))
   (let ((n (markup-option c :number)))
      (if (not n)
	  ""
	  (case (markup-markup c)
	     ((chapter)
	      (html-chapter-number c))
	     ((section)
	      (html-section-number c))
	     ((subsection)
	      (html-subsection-number c))
	     ((subsubsection)
	      (html-subsubsection-number c))
	     (else
	      (if (container? c)
		  (inner-html-container-number c)
		  (skribe-error 'html-container-number
				"Not a container"
				(markup-markup c))))))))


;*---------------------------------------------------------------------*/
;*    html-width ...                                                   */
;*---------------------------------------------------------------------*/
(define (html-width width)
   (cond
      ((and (integer? width) (exact? width))
       (format #f "~A" width))
      ((real? width)
       (format #f "~A%" (inexact->exact (round width))))
      ((string? width)
       width)
      (else
       (skribe-error 'html-width "bad width" width))))

;*---------------------------------------------------------------------*/
;*    html-class ...                                                   */
;*---------------------------------------------------------------------*/
(define (html-class m)
   (if (markup? m)
       (let ((c (markup-class m)))
	  (if (or (string? c) (symbol? c) (number? c))
	      (format #t " class=\"~a\"" c)))))

;*---------------------------------------------------------------------*/
;*    html-open ...                                                    */
;*---------------------------------------------------------------------*/
(define* (html-open tag #:optional (attributes '()) (newline? #t))
  "Output opening TAG with ATTRIBUTES, an association list mapping
attribute names to their values. Attribute names may be symbols or
strings. Values may be symbols, strings or numbers. Attributes with
unspecified or #f values are ignored. If NEWLINE? is #t, add a newline
at the end."
  (display "<")
  (display tag)
  (for-each (match-lambda
              ((name . value)
               (when (and value
                          (not (unspecified? value)))
                 (format #t " ~a=\"~a\"" name value))))
            attributes)
  (display ">")
  (when newline?
    (newline)))

;*---------------------------------------------------------------------*/
;*    html-close ...                                                   */
;*---------------------------------------------------------------------*/
(define (html-close tag)
  "Output closing TAG."
  (display "</")
  (display tag)
  (display ">")
  (newline))

;*---------------------------------------------------------------------*/
;*    style-declaration ...                                            */
;*---------------------------------------------------------------------*/
(define (style-declaration properties)
  "Return a style declaration with PROPERTIES, an association list
mapping property names to their values. Property names may be symbols
or strings. Values may be strings or numbers. Properties with #f
values are ignored. If PROPERTIES is empty or all of its elements were
ignored, return #f."
  (match (filter-map (match-lambda
                       ((name . value)
                        (and value
                             (format #f "~a: ~a;" name value))))
                     properties)
    (() #f)
    (serialized-properties
     (string-join serialized-properties))))

;*---------------------------------------------------------------------*/
;*    html-markup-class ...                                            */
;*---------------------------------------------------------------------*/
(define* (html-markup-class m #:optional (newline? #t))
  (lambda (node engine)
    (html-open m
               `((class . ,(markup-class node)))
               newline?)))

;*---------------------------------------------------------------------*/
;*    html-color-spec? ...                                             */
;*---------------------------------------------------------------------*/
(define (html-color-spec? v)
   (and v
	(not (unspecified? v))
	(or (not (string? v)) (> (string-length v) 0))))

;*---------------------------------------------------------------------*/
;*    document ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'document
   :options '(:title :author :ending :html-title :env :keywords)

   :action (lambda (node engine)
	      (let* ((id (markup-ident node))
		     (title (new markup
			       (markup '&html-document-title)
			       (parent node)
			       (ident (string-append id "-title"))
			       (class (markup-class node))
			       (options `((author ,(markup-option node :author))))
			       (body (markup-option node :title)))))

                 ;; Record the file name, for use by `html-file-default'.
                 (markup-option-add! node :file (*destination-file*))
		 (&html-generic-document node title engine)))

   :after (lambda (node engine)
	     (if (engine-custom engine 'emit-sui)
		 (document-sui node engine))))

;*---------------------------------------------------------------------*/
;*    &html-html ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&html-html
   :before "<!DOCTYPE html>
<html>\n"
   :after "</html>")

;*---------------------------------------------------------------------*/
;*    &html-head ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&html-head
   :before (lambda (node engine)
             (html-open 'head)
             (html-open 'meta
                        `((http-equiv . "Content-Type")
                          (content . "text/html;")
                          (charset . ,(engine-custom (find-engine 'html)
                                                     'charset))))
             (let ((head (engine-custom engine 'head)))
               (when head
                 (display head)
                 (newline))))
   :after "</head>\n\n")

;*---------------------------------------------------------------------*/
;*    &html-meta ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&html-meta
   :before "<meta name=\"keywords\" content=\""
   :action (lambda (node engine)
             (let ((kw* (map ast->string (or (markup-body node) '()))))
               (output (keyword-list->comma-separated kw*) engine)))
   :after  "\">\n")

;*---------------------------------------------------------------------*/
;*    &html-body ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&html-body
   :before (lambda (node engine)
	      (let ((bg (engine-custom engine 'background)))
                (html-open 'body
                           `((class . ,(markup-class node))
                             (bgcolor . ,(and (html-color-spec? bg)
                                              bg))))))
   :after "</body>\n")

;*---------------------------------------------------------------------*/
;*    &html-page ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&html-page
   :action (lambda (node engine)
	      (define (html-margin m fn size bg fg cla)
                (html-open 'td
                           `((align . "left")
                             (valign . "top")
                             (class . ,cla)
                             (width . ,(and size (html-width size)))
                             (bgcolor . ,(and (html-color-spec? bg)
                                              bg))))
                (html-open 'div
                           `((class . ,cla)))
		(cond
		 ((and (string? fg) (string? fn))
		  (format #t "<font color=\"~a\" \"~a\">" fg fn))
		 ((string? fg)
                  (html-open 'font
                             `((color . ,fg))))
		 ((string? fn)
		  (format #t "<font \"~a\">" fn)))
		(if (procedure? m)
		    (evaluate-document (m node engine)
                                       engine)
		    (output m engine))
		(if (or (string? fg) (string? fn))
                    (html-close 'font))
                (html-close 'div)
                (html-close 'td))
	      (let ((body (markup-body node))
		    (lm (engine-custom engine 'left-margin))
		    (lmfn (engine-custom engine 'left-margin-font))
		    (lms (engine-custom engine 'left-margin-size))
		    (lmbg (engine-custom engine 'left-margin-background))
		    (lmfg (engine-custom engine 'left-margin-foreground))
		    (rm (engine-custom engine 'right-margin))
		    (rmfn (engine-custom engine 'right-margin-font))
		    (rms (engine-custom engine 'right-margin-size))
		    (rmbg (engine-custom engine 'right-margin-background))
		    (rmfg (engine-custom engine 'right-margin-foreground)))
		 (cond
		    ((and lm rm)
		     (let* ((ep (engine-custom engine 'margin-padding))
			    (ac (if (number? ep) ep 0)))
                       (html-open 'table
                                  `((cellpadding . ,ac)
                                    (cellspacing . "0")
                                    (width . "100%")
                                    (class . "skribilo-margins")))
                       (html-open 'tr))
		     (html-margin lm lmfn lms lmbg lmfg "skribilo-left-margin")
		     (html-margin body #f #f #f #f "skribilo-body")
		     (html-margin rm rmfn rms rmbg rmfg "skribilo-right-margin")
                     (html-close 'tr)
                     (html-close 'table))
		    (lm
		     (let* ((ep (engine-custom engine 'margin-padding))
			    (ac (if (number? ep) ep 0)))
                       (html-open 'table
                                  `((cellpadding . ,ac)
                                    (cellspacing . "0")
                                    (width . "100%")
                                    (class . "skribilo-margins")))
                       (html-open 'tr))
		     (html-margin lm lmfn lms lmbg lmfg "skribilo-left-margin")
		     (html-margin body #f #f #f #f "skribilo-body")
                     (html-close 'tr)
                     (html-close 'table))
		    (rm
                     (html-open 'table
                                `((cellspacing . "0")
                                  (width . "100%")
                                  (class . "skribilo-margins")))
                     (html-open 'tr)
		     (html-margin body #f #f #f #f "skribilo-body")
		     (html-margin rm rmfn rms rmbg rmfg "skribilo-right-margin")
                     (html-close 'tr)
                     (html-close 'table))
		    (else
                     (html-open 'div
                                '((class . "skribilo-body")))
		     (output body engine)
                     (html-close 'div))))))

;*---------------------------------------------------------------------*/
;*    &html-generic-header ...                                         */
;*---------------------------------------------------------------------*/
(define (&html-generic-header node engine)
   (let* ((ic (engine-custom engine 'favicon))
	  (id (markup-ident node)))
      (unless (string? id)
	 (skribe-error '&html-generic-header
		       (format #f "Invalid identifier '~a'" id)
		       node))
      ;; title
      (output (new markup
		 (markup '&html-header-title)
		 (parent node)
		 (ident (string-append id "-title"))
		 (class (markup-class node))
		 (body (markup-body node)))
	      engine)
      ;; favicon
      (output (new markup
		 (markup '&html-header-favicon)
		 (parent node)
		 (ident (string-append id "-favicon"))
		 (body (cond
			  ((string? ic)
			   ic)
			  ((procedure? ic)
			   (ic id engine))
			  (else #f))))
	      engine)
      ;; style
      (output (new markup
		 (markup '&html-header-style)
		 (parent node)
		 (ident (string-append id "-style"))
		 (class (markup-class node)))
	      engine)
      ;; css
      (output (new markup
		 (markup '&html-header-css)
		 (parent node)
		 (ident (string-append id "-css"))
		 (body (let ((c (engine-custom engine 'css)))
			  (if (string? c)
			      (list c)
			      c))))
	      engine)
      ;; javascript
      (output (new markup
		 (markup '&html-header-javascript)
		 (parent node)
		 (ident (string-append id "-javascript")))
	      engine)))

(markup-writer '&html-header-title
   :before "<title>"
   :action (lambda (node engine)
	      (output (markup-body node)
                      html-title-engine))
   :after "</title>\n")

(markup-writer '&html-header-favicon
   :action (lambda (node engine)
	      (let ((i (markup-body node)))
		 (when i
                   (html-open 'link
                              `((rel . "shortcut icon")
                                (href . ,i)))))))

(markup-writer '&html-header-css
   :action (lambda (node engine)
	      (let ((css (markup-body node)))
		(when (pair? css)
		  (for-each (lambda (css)
                              (html-open 'link
                                         `((rel . "stylesheet")
                                           (type . "text/css")
                                           (href . ,css))))
			    css)))))

(markup-writer '&html-header-style
   :before " <style type=\"text/css\">\n  <!--\n"
   :action (lambda (node engine)
	      (display "  pre { font-family: monospace }\n")
	      (display "  tt { font-family: monospace }\n")
	      (display "  code { font-family: monospace }\n")
	      (display "  p.flushright { text-align: right }\n")
	      (display "  p.flushleft { text-align: left }\n")
	      (display "  span.sc { font-variant: small-caps }\n")
	      (display "  span.sf { font-family: sans-serif }\n")
	      (display "  span.skribetitle { font-family: sans-serif; font-weight: bolder; font-size: x-large; }\n")
              (display "  li.skribilo-toc-item::marker { content: attr(skribilo-toc-item-marker) }\n")
	      (for-each (lambda (css)
                          (display (guard (c (else (skribe-error
				                    'html-css
				                    "Can't open CSS file for input"
				                    css)))
                                     (call-with-input-file css
                                       get-string-all))))
                        (let ((inline-css (engine-custom engine 'inline-css)))
			  (if (string? inline-css)
			      (list inline-css)
			      inline-css))))
   :after "  -->\n </style>\n")

(markup-writer '&html-header-javascript
   :action (lambda (node engine)
	      (when (engine-custom engine 'javascript)
                 (html-open 'script
                            '((language . "JavaScript")
                              (type . "text/javascript")))
		 (display " <!--\n")
		 (display "  function skribenospam( n, d, f ) {\n")
		 (display "    nn=n.replace( / /g , \".\" );\n" )
		 (display "    dd=d.replace( / /g , \".\" );\n" )
		 (display "    document.write( \"<a href=\\\"mailto:\" + nn + \"@\" + dd + \"\\\">\" );\n")
		 (display "    if( f ) {\n")
		 (display "      document.write( \"<tt>\" + nn + \"@\" + dd + \"</\" + \"tt><\" + \"/a>\" );\n")
		 (display "    }\n")
		 (display "  }\n")
		 (display " -->\n")
                 (html-close 'script))
	      (let* ((ejs (engine-custom engine 'js))
		     (js (cond
			    ((string? ejs)
			     (list ejs))
			    ((list? ejs)
			     ejs)
			    (else
			     '()))))
		 (for-each (lambda (s)
                             (html-open 'script
                                        `((type . "text/javascript")
                                          (src . ,s)))
                             (html-close 'script))
			   js))))


;*---------------------------------------------------------------------*/
;*    &html-header ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer '&html-document-header :action &html-generic-header)
(markup-writer '&html-chapter-header :action &html-generic-header)
(markup-writer '&html-section-header :action &html-generic-header)
(markup-writer '&html-subsection-header :action &html-generic-header)
(markup-writer '&html-subsubsection-header :action &html-generic-header)

;*---------------------------------------------------------------------*/
;*    &html-ending ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer '&html-ending
   :before "<div class=\"skribilo-ending\">"
   :action (lambda (node engine)
	      (let ((body (markup-body node)))
		 (if body
		     (output body engine)
		     (evaluate-document (list "(made with "
                                              (ref :text "skribilo"
                                                   :url (skribilo-url))
                                              ")")
                                        engine))))
   :after "</div>\n")

;*---------------------------------------------------------------------*/
;*    &html-generic-title ...                                          */
;*---------------------------------------------------------------------*/
(define (&html-generic-title node engine)
   (let* ((title (markup-body node))
	  (authors (markup-option node 'author))
	  (title-background (engine-custom engine 'title-background))
	  (title-foreground (engine-custom engine 'title-foreground))
	  (title-font (engine-custom engine 'title-font)))
      (when title
        (html-open 'h1
                   `((class . "skribilo-title")
                     (style . ,(style-declaration
                                `((color . ,(and (html-color-spec? title-foreground)
                                                 title-foreground))
                                  (background-color . ,(and (html-color-spec? title-background)
                                                            title-background))
                                  (font-family . ,title-font))))))
        (output title engine)
        (html-close 'h1)
        (when authors
          (html-title-authors authors engine)))))

;*---------------------------------------------------------------------*/
;*    &html-document-title ...                                         */
;*---------------------------------------------------------------------*/
(markup-writer '&html-document-title :action &html-generic-title)
(markup-writer '&html-chapter-title :action &html-generic-title)
(markup-writer '&html-section-title :action &html-generic-title)
(markup-writer '&html-subsection-title :action &html-generic-title)
(markup-writer '&html-subsubsection-title :action &html-generic-title)

;*---------------------------------------------------------------------*/
;*    &html-footnotes                                                  */
;*---------------------------------------------------------------------*/
(markup-writer '&html-footnotes
   :before (lambda (node engine)
	      (let ((footnotes (markup-body node)))
		 (when (pair? footnotes)
                   (html-open 'div
                              '((class . "skribilo-footnote")))
                   (html-open 'hr
                              '((width . "20%")
                                (size . "2")
                                (align . "left"))))))
   :action (lambda (node engine)
             (let ((footnotes (markup-body node)))
               (for-each (lambda (fn)
                           (html-open 'div
                                      '((class . "footnote")))
                           ;; Note: the <a> tags must not be nested.
                           (html-open 'a
                                      `((name . ,(string-append "footnote-"
                                                                (string-canonicalize
                                                                 (container-ident fn))))))
                           (html-close 'a)
                           (html-open 'a
                                      `((href . ,(string-append "#footnote-site-"
                                                                (string-canonicalize
                                                                 (container-ident fn))))))
                           (html-open 'sup)
                           (html-open 'small)
                           (display (markup-option fn :label))
                           (html-close 'small)
                           (html-close 'sup)
                           (html-close 'a)
			   (output (markup-body fn) engine)
                           (html-close 'div))
                         footnotes)
               (when (pair? footnotes)
                 (html-close 'div)))))

;*---------------------------------------------------------------------*/
;*    html-title-authors ...                                           */
;*---------------------------------------------------------------------*/
(define (html-title-authors authors engine)
  (match authors
    ((single-author)
     (html-title-authors (list single-author)
                         engine))
    (authors
     (html-open 'div
                `((style . ,(style-declaration
                             '((text-align . "center"))))))
     (display (string-join (map (lambda (author)
                                  (with-output-to-string
                                    (cut output author engine)))
                                authors)
                           ", "))
     (html-close 'div))))

;*---------------------------------------------------------------------*/
;*    author ...                                                       */
;*---------------------------------------------------------------------*/
(markup-writer 'author
   :options '(:name :title :affiliation :email :url :address :phone :photo :align)
   :action (lambda (node engine)
	     (let ((name (markup-option node :name))
		   (title (markup-option node :title))
		   (affiliation (markup-option node :affiliation))
		   (email (markup-option node :email))
		   (url (markup-option node :url))
		   (address (markup-option node :address))
		   (phone (markup-option node :phone))
		   (author-font (engine-custom engine 'author-font))
		   (align (markup-option node :align)))
	       (define (row node)
                 (output node engine)
                 (html-open 'br))
               (html-open 'div
                          `((style . ,(style-declaration
                                       `((text-align . ,align))))))
	       ;; name
               (when author-font
                 (html-open 'span
                            `((style . ,(style-declaration
                                         `((font-family . ,author-font)))))))
               (row name)
               (when author-font (html-close 'span))
	       ;; title
	       (when title (row title))
	       ;; affiliation
	       (when affiliation (row affiliation))
	       ;; Output address only if it is a list. address could
	       ;; be a markup object.
	       (when (list? address)
                 (for-each row address))
	       ;; telephone
	       (when phone (row phone))
	       ;; email
	       (when email (row email))
	       ;; url
	       (when url (row url))
               (html-close 'div))))

;*---------------------------------------------------------------------*/
;*    author ...                                                       */
;*---------------------------------------------------------------------*/
(markup-writer 'author
   :options '(:name :title :affiliation :email :url :address :phone :photo :align)
   :predicate (lambda (node engine)
                (markup-option node :photo))
   :action (lambda (node engine)
	      (let ((photo (markup-option node :photo)))
                (output photo engine)
		(markup-option-add! node :photo #f)
		(output node engine)
		(markup-option-add! node :photo photo))))

;*---------------------------------------------------------------------*/
;*    toc ...                                                          */
;*---------------------------------------------------------------------*/
(markup-writer 'toc
   :options 'all
   :action (lambda (node engine)
	     (define (toc-entries entries)
               ;; Do not produce an empty table.
               (unless (null? entries)
                 (html-open 'ol)
                 (for-each (match-lambda
                             ((parent children ...)
                              (let ((id (markup-ident parent))
		                    (file (html-file parent engine)))
		                (unless (string? id)
		                  (skribe-error 'toc
				                (format #f "invalid identifier '~a'" id)
				                parent))
		                ;; title
                                (html-open 'li
                                           `((class . "skribilo-toc-item")
                                             (skribilo-toc-item-marker
                                              . ,(string-append (html-container-number parent engine)
                                                                " "))))
                                (html-open 'a
                                           `((href . ,(string-append
                                                       (if (and (*destination-file*)
				                                (string=? file (*destination-file*)))
			                                   ""
			                                   (strip-ref-base (or file (*destination-file*) "")))
                                                       "#"
                                                       (string-canonicalize id)))))
		                (output (markup-option parent :title)
                                        engine)
                                (html-close 'a)
		                ;; the children
                                (toc-entries children)
                                (html-close 'li))))
                           entries)
                 (html-close 'ol)))

	      (let ((chapter (markup-option node :chapter))
		    (section (markup-option node :section))
		    (subsection (markup-option node :subsection))
		    (subsubsection (markup-option node :subsubsection))
		    (body (if (handle? (markup-body node))
                              (handle-ast (markup-body node))
                              (markup-body node))))
		 (if (not (container? body))
		     (error 'toc
			    "Invalid body (container expected)"
			    (if (markup? body)
				(markup-markup body)
				"???"))
                     (toc-entries
                      (find-down (lambda (x)
				   (and (markup? x)
					(markup-option x :toc)
					(or (and subsubsection
                                                 (is-markup? x 'subsubsection))
					    (and subsection
                                                 (is-markup? x 'subsection))
					    (and section
                                                 (is-markup? x 'section))
					    (and chapter
                                                 (is-markup? x 'chapter))
					    (markup-option node
                                                           (symbol->keyword
							    (markup-markup x))))))
				 (container-body body)))))))

(define (sections-in-same-file? node1 node2 engine)
  ;; Return #t when NODE1 and NODE2 are to be output in the same file
  ;; according to E's settings.
  (and (container? node1)
       (container? node2)
       (equal? (html-file node1 engine)
               (html-file node2 engine))))

;*---------------------------------------------------------------------*/
;*    &html-generic-document ...                                       */
;*---------------------------------------------------------------------*/
(define (&html-generic-document node title engine)
  (define (set-output-encoding)
    ;; Make sure the output is suitably encoded.
    (and=> (engine-custom engine 'charset)
           (lambda (charset)
             (set-port-encoding! (current-output-port) charset)
             (set-port-conversion-strategy! (current-output-port) 'error))))

   (let* ((id (markup-ident node))
	  (header (new markup
		     (markup '&html-chapter-header)
		     (ident (string-append id "-header"))
		     (class (markup-class node))
		     (parent node)
		     (body (html-browser-title node))))
          (meta (new markup
                   (markup '&html-meta)
                   (ident (string-append id "-meta"))
                   (class (markup-class node))
                   (parent node)
                   (body (markup-option (ast-document node)
                                        :keywords))))
	  (head (new markup
		   (markup '&html-head)
		   (ident (string-append id "-head"))
		   (class (markup-class node))
		   (parent node)
		   (body (list header meta))))
	  (ftnote (new markup
		     (markup '&html-footnotes)
		     (ident (string-append id "-footnote"))
		     (class (markup-class node))
		     (parent node)

		     (body
                      ;; Collect the footnotes of all the sub-containers that
                      ;; are to be output in the same file.
                      (match (find-down (lambda (s)
                                          (sections-in-same-file? s node engine))
                                        node)
                        ((containers ...)
                         (reverse
                          (let loop ((subsections containers)
                                     (footnotes   '()))
                            (match subsections
                              ((subsections ...)
                               (fold loop footnotes subsections))
                              (container
                               (append footnotes
                                       (or (container-env-get container
                                                              'footnote-env)
                                           '())))))))
                        (_ #f)))))
	  (page (new markup
		   (markup '&html-page)
		   (ident (string-append id "-page"))
		   (class (markup-class node))
		   (parent node)
		   (body (list (markup-body node)
                               ftnote))))
	  (ending (new markup
		     (markup '&html-ending)
		     (ident (string-append id "-ending"))
		     (class (markup-class node))
		     (parent node)
		     (body (or (markup-option node :ending)
			       (let ((p (ast-document node)))
				  (and p (markup-option p :ending)))))))
	  (body (new markup
		   (markup '&html-body)
		   (ident (string-append id "-body"))
		   (class (markup-class node))
		   (parent node)
		   (body (list title page ending))))
	  (html (new markup
		   (markup '&html-html)
		   (ident (string-append id "-html"))
		   (class (markup-class node))
		   (parent node)
		   (body (list head body)))))
      ;; No file must be opened for documents. These files are
      ;; directly opened by Skribe
      (if (document? node)
          (begin
            (set-output-encoding)
            (output html engine))
          (parameterize ((*destination-file* (html-file node engine)))
            (with-output-to-file (*destination-file*)
              (lambda ()
                (set-output-encoding)
		(output html engine)))))))

;*---------------------------------------------------------------------*/
;*    &html-generic-subdocument ...                                    */
;*---------------------------------------------------------------------*/
(define (&html-generic-subdocument node engine)
   (let* ((p (ast-document node))
	  (id (markup-ident node))
	  (ti (let* ((nb (html-container-number node engine))
		     (tc (markup-option node :title))
		     (ti (if (document? p)
			     (list (markup-option p :title)
				   (engine-custom engine 'file-title-separator)
				   tc)
			     tc))
		     (sep (engine-custom
			     engine
			     (symbol-append (markup-markup node)
					    '-title-number-separator)))
		     (nti (and tc
			       (if (and nb (not (equal? nb "")))
				   (list nb
					 (if (unspecified? sep) ". " sep)
					 ti)
				   ti))))
		 (new markup
		    (markup (symbol-append '&html- (markup-markup node) '-title))
		    (ident (string-append id "-title"))
		    (parent node)
		    (options '((author ())))
		    (body nti)))))
      (case (markup-markup node)
	 ((chapter)
	  (skribe-message "  [~s chapter: ~a]\n" (engine-ident engine) id))
	 ((section)
	  (skribe-message "    [~s section: ~a]\n" (engine-ident engine) id)))
      (&html-generic-document node ti engine)))

;*---------------------------------------------------------------------*/
;*    chapter ... @label chapter@                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'chapter
   :options '(:title :number :file :toc :html-title :env)
   :before (lambda (node engine)
	      (let ((title (markup-option node :title))
		    (ident (markup-ident node)))
                ;; chapter title in comments
		(display "<!-- ")
		(output title html-title-engine)
		(display " -->\n")
                ;; h1 wrapping chapter title
                (html-open 'h1
                           `((class . ,(markup-class node))
                             (id . ,(string-canonicalize ident))
                             (style . ,(style-declaration
                                        '((text-align . "center"))))))
                ;; chapter number
		(output (html-container-number node engine)
                        engine)
		(display " ")
                ;; chapter title
		(output (markup-option node :title)
                        engine)
                (html-close 'h1))))

;; This writer is invoked only for chapters rendered inside separate files!
(markup-writer 'chapter
   :options '(:title :number :file :toc :html-title :env)
   :predicate (lambda (node engine)
		 (or (markup-option node :file)
		     (engine-custom engine 'chapter-file)))
   :action &html-generic-subdocument)

;*---------------------------------------------------------------------*/
;*    html-section-title ...                                           */
;*---------------------------------------------------------------------*/
(define (html-section-title node engine)
   (let* ((title (markup-option node :title))
	  (number (markup-option node :number))
	  (class (markup-class node))
	  (ident (markup-ident node))
	  (kind (markup-markup node))
	  (title-background (engine-custom engine
                                           (symbol-append kind '-title-background)))
	  (title-foreground (engine-custom engine
                                           (symbol-append kind '-title-foreground)))
	  (title-start (engine-custom engine
                                      (symbol-append kind '-title-start)))
	  (title-stop (engine-custom engine
                                     (symbol-append kind '-title-stop)))
	  (number-separator (engine-custom engine
                                           (symbol-append kind '-title-number-separator))))
      ;; section title in comments
      (display "<!-- ")
      (output title html-title-engine)
      (display " -->\n")
      ;; div wrapping the title
      (html-open 'div
                 `((class . ,(if class
                                 (string-append class "-title")
                                 (string-append "skribilo-" kind "-title")))
                   (id . ,(string-canonicalize ident))
                   (style . ,(style-declaration
                              `((color . ,(and (html-color-spec? title-foreground)
                                               title-foreground))
                                (background-color . ,(and (html-color-spec? title-background)
                                                          title-background)))))))
      ;; title start string
      (display title-start)
      ;; section number if enabled
      (when number
        (output (html-container-number node engine)
                engine)
        (output number-separator engine))
      ;; the title itself
      (output title engine)
      ;; title stop string
      (display title-stop)
      (html-close 'div)
      ((html-markup-class "div") node engine)))

;*---------------------------------------------------------------------*/
;*    section ...  @label section@                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'section
   :options '(:title :html-title :number :toc :file :env)
   :before html-section-title
   :after "</div>\n")

;; on-file section writer
(markup-writer 'section
   :options '(:title :html-title :number :toc :file :env)
   :predicate (lambda (node engine)
		 (or (markup-option node :file)
		     (engine-custom engine 'section-file)))
   :action &html-generic-subdocument)

;*---------------------------------------------------------------------*/
;*    subsection ... @label subsection@                                */
;*---------------------------------------------------------------------*/
(markup-writer 'subsection
   :options '(:title :html-title :number :toc :env :file)
   :before html-section-title
   :after "</div>\n")

;; on-file subsection writer
(markup-writer 'section
   :options '(:title :html-title :number :toc :file :env)
   :predicate (lambda (node engine)
		 (or (markup-option node :file)
		     (engine-custom engine 'subsection-file)))
   :action &html-generic-subdocument)

;*---------------------------------------------------------------------*/
;*    subsubsection ... @label subsubsection@                          */
;*---------------------------------------------------------------------*/
(markup-writer 'subsubsection
   :options '(:title :html-title :number :toc :env :file)
   :before html-section-title
   :after "</div>\n")

;; on-file subsection writer
(markup-writer 'section
   :options '(:title :html-title :number :toc :file :env)
   :predicate (lambda (node engine)
		 (or (markup-option node :file)
		     (engine-custom engine 'subsubsection-file)))
   :action &html-generic-subdocument)

;*---------------------------------------------------------------------*/
;*    paragraph ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'paragraph
   :before (lambda (node engine)
	      (when (and (>= (*debug*) 2)
                         (location? (ast-loc node)))
                (html-open 'span
                           '((style . "display: block; position: relative; left: -2cm; font-size: x-small; font-style: italic; color: ff8e1e;")))
                (ast-loc node)
                (html-close 'span))
	      ((html-markup-class "p") node engine))
   :after "</p>")

;*---------------------------------------------------------------------*/
;*    ~ ...                                                            */
;*---------------------------------------------------------------------*/
(markup-writer '~
   :before "&nbsp;"
   :after #f
   :action #f)

;*---------------------------------------------------------------------*/
;*    footnote ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'footnote
   :options '(:label)
   :action (lambda (node engine)
             (html-open 'a
                        `((name . ,(string-append "footnote-site-"
                                                  (string-canonicalize
                                                   (container-ident node))))))
             (html-open 'a
                        `((href . ,(string-append "#footnote-"
                                                  (string-canonicalize
                                                   (container-ident node))))))
             (html-open 'sup)
             (html-open 'small)
             (display (markup-option node :label))
             (html-close 'small)
             (html-close 'sup)
             (html-close 'a)))

;*---------------------------------------------------------------------*/
;*    linebreak ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'linebreak
	       :before (lambda (node engine)
                         (html-open 'br
                                    `((class . ,(html-class node))))))

;*---------------------------------------------------------------------*/
;*    hrule ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'hrule
   :options '(:width :height)
   :before (lambda (node engine)
	      (let ((width (markup-option node :width))
		    (height (markup-option node :height)))
                (html-open 'hr
                           `((width . ,(and (< width 100)
                                            (html-width width)))
                             (size . ,(and (> height 1)
                                           height)))))))

;*---------------------------------------------------------------------*/
;*    color ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'color
    :options '(:bg :fg :width :margin)
    :before (lambda (node engine)
	      (let ((margin (markup-option node :margin))
		    (width (markup-option node :width))
		    (background-color (markup-option node :bg))
		    (foreground-color (markup-option node :fg)))
                (html-open 'span
                           `((class . ,(markup-class node))
                             (style . ,(style-declaration
                                        `((color . ,(and (html-color-spec? foreground-color)
                                                         (with-output-to-string
                                                           (cut output foreground-color engine))))
                                          (background-color . ,(and (html-color-spec? background-color)
                                                                    (with-output-to-string
                                                                      (cut output background-color engine))))
                                          (padding . ,(and margin
                                                           (string-append (number->string margin)
                                                                          "px")))
                                          (width . ,(and width
                                                         (string-append (html-width width)
                                                                        "px")))))))
                           #f)))
    :after "</span>")

;*---------------------------------------------------------------------*/
;*    frame ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'frame
   :options '(:width :margin :border)
   :before (lambda (node engine)
	      (let ((margin (markup-option node :margin))
		    (border (markup-option node :border))
		    (width (markup-option node :width)))
                (html-open 'div
                           `((class . ,(markup-class node))
                             (style . ,(style-declaration
                                        `((border-style . "solid")
                                          (border-width . ,(and border
                                                                (string-append (number->string border)
                                                                               "px")))
                                          (padding . ,(and margin
                                                           (string-append (number->string margin)
                                                                          "px")))
                                          (width . ,(and width
                                                         (string-append (html-width width)
                                                                        "px"))))))))))
   :after "</div>\n")

;*---------------------------------------------------------------------*/
;*    font ...                                                         */
;*---------------------------------------------------------------------*/
(markup-writer 'font
   :options '(:size :face)
   :before (lambda (node engine)
	     (let ((size (markup-option node :size))
		   (face (markup-option node :face)))
               ;; Set font face and size if absolute.
               (html-open 'span
                          `((class . ,(markup-class node))
                            (style . ,(style-declaration
                                       `((font-family . ,face)
                                         (font-size . ,(and size
                                                            (positive? size)
                                                            (exact? size)
                                                            (string-append (number->string size)
                                                                           "px"))))))))
               ;; If size is relative, add a number of <span> tags
               ;; with larger/smaller font-size styles.
               (when (and size
                          (or (and (positive? size)
                                   (inexact? size))
                              (negative? size)))
                 (for-each (lambda _
                             (html-open 'span
                                        `((style . ,(style-declaration
                                                     `((font-size . ,(if (positive? size)
                                                                         "larger"
                                                                         "smaller"))))))))
                           (iota (abs (inexact->exact size)))))))
   :after (lambda (node engine)
	    (let ((size (markup-option node :size)))
              (when (and size
                         (or (and (positive? size)
                                  (inexact? size))
                             (negative? size)))
                (for-each (lambda _
                            (html-close 'span))
                          (iota (abs (inexact->exact size)))))
              (html-close 'span))))

;*---------------------------------------------------------------------*/
;*    flush ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'flush
    :options '(:side)
    :before (lambda (node engine)
              (let ((text-align
                     (case (markup-option node :side)
                       ((left center right)
                        (symbol->string (markup-option node :side)))
                       (else
                        (skribe-error 'flush
				      "Invalid side"
				      (markup-option node :side))))))
                (html-open 'span
                           `((class . ,(markup-class node))
                             (style . ,(style-declaration
                                        `((text-align . ,text-align))))))))
    :after "</span>\n")

;*---------------------------------------------------------------------*/
;*    center ...                                                       */
;*---------------------------------------------------------------------*/
(markup-writer 'center
   :before (lambda (node engine)
             (html-open 'span
                        `((class . ,(markup-class node))
                          (style . "text-align: center"))))
   :after "</span>\n")

;*---------------------------------------------------------------------*/
;*    pre ...                                                          */
;*---------------------------------------------------------------------*/
(markup-writer 'pre :before (html-markup-class "pre") :after "</pre>\n")

;*---------------------------------------------------------------------*/
;*    prog ...                                                         */
;*---------------------------------------------------------------------*/
(markup-writer 'prog
   :options '(:line :mark)
   :before (html-markup-class "pre")
   :after "</pre>\n")

;*---------------------------------------------------------------------*/
;*    itemize ...                                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'itemize
   :options '(:symbol)
   :before (html-markup-class "ul")
   :action (lambda (node engine)
	      (for-each (lambda (item)
			  (let ((ident (and (markup? item)
					    (markup-ident item))))
                            (html-open 'li
                                       `((class . ,(markup-class item))))
			    (if ident  ;; produce an anchor
                                (html-open 'a
                                           `((name . ,(string-canonicalize ident))))
                                (html-close 'a))
			   (output item engine)
                           (html-close 'li)))
			(markup-body node)))
   :after "</ul>")

;*---------------------------------------------------------------------*/
;*    enumerate ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'enumerate
   :options '(:symbol)
   :before (html-markup-class "ol")
   :action (lambda (node engine)
	      (for-each (lambda (item)
			  (let ((ident (and (markup? item)
					    (markup-ident item))))
                            (html-open 'li
                                       `((class . ,(markup-class item))))
			    (if ident  ;; produce an anchor
                                (html-open 'a
                                           `((name . ,ident)))
                                (html-close 'a))
			    (output item engine)
                            (html-close 'li)))
			(markup-body node)))
   :after "</ol>")

;*---------------------------------------------------------------------*/
;*    description ...                                                  */
;*---------------------------------------------------------------------*/
(markup-writer 'description
   :options '(:symbol)
   :before (html-markup-class "dl")
   :action (lambda (node engine)
	      (for-each (lambda (item)
			   (let ((k (markup-option item :key)))
			      (for-each (lambda (i)
                                          (html-open 'dt
                                                     `((class . ,(and (markup? i)
                                                                      (markup-class i)))))
					  (output i engine)
                                          (html-close 'dt))
					(if (pair? k) k (list k)))
                              (html-open 'dd
                                         `((class . ,(markup-class item))))
			      (output (markup-body item)
                                      engine)
                              (html-close 'dd)))
			(markup-body node)))
   :after "</dl>")

;*---------------------------------------------------------------------*/
;*    item ...                                                         */
;*---------------------------------------------------------------------*/
(markup-writer 'item
   :options '(:key)
   :action (lambda (node engine)
	      (let ((k (markup-option node :key)))
		 (if k
		     (begin
                       (html-open 'b
                                  `((class . ,(markup-class node))))
		       (output k engine)
                       (html-close 'b))))
	      (output (markup-body node)
                      engine)))

;*---------------------------------------------------------------------*/
;*    blockquote ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer 'blockquote
   :options '()
   :before (lambda (node engine)
             (html-open 'blockquote
                        `((class . ,(markup-class node)))))
   :after "\n</blockquote>\n")

;*---------------------------------------------------------------------*/
;*    figure ... @label figure@                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'figure
   :options '(:legend :number :multicolumns :legend-width)
   :before (html-markup-class "br")
   :action (lambda (node engine)
	      (let ((ident (markup-ident node))
		    (number (markup-option node :number))
		    (legend (markup-option node :legend)))
                (html-open 'a
                           `((name . ,(string-canonicalize ident))))
                (html-close 'a)
		(output (markup-body node)
                        engine)
                (html-open 'br)
		(output (new markup
			     (markup '&html-figure-legend)
			     (parent node)
			     (ident (string-append ident "-legend"))
			     (class (markup-class node))
			     (options `((:number ,number)))
			     (body legend))
			engine)))
   :after "<br>")

;*---------------------------------------------------------------------*/
;*    &html-figure-legend ...                                          */
;*---------------------------------------------------------------------*/
(markup-writer '&html-figure-legend
   :options '(:number)
   :before (lambda (node engine)
              (html-open 'center)
	      (let ((number (markup-option node :number)))
		(if number
                    (begin
                      (html-open 'strong)
                      (display "Fig. ")
                      (display (number->string number))
                      (display ":")
                      (html-close 'strong))
                    (begin
                      (html-open 'strong)
                      (display "Fig. :")
                      (html-close 'strong)))))
   :after "</center>")

;*---------------------------------------------------------------------*/
;*    table ...                                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'table
   :options '(:border :width :frame :rules :cellstyle :cellpadding :cellspacing)
   :before (lambda (node engine)
	      (let ((width (markup-option node :width))
		    (border (markup-option node :border))
		    (frame (markup-option node :frame))
		    (rules (markup-option node :rules))
		    (cstyle (markup-option node :cellstyle))
		    (cp (markup-option node :cellpadding))
		    (cs (markup-option node :cellspacing)))
                (html-open 'table
                           `((class . ,(markup-class node))
                             (width . ,(and width
                                            (html-width width)))
                             (border . ,border)
                             (cellpadding . ,(and (number? cp)
                                                  (>= cp 0)
                                                  cp))
                             (cellspacing . ,(and (number? cs)
                                                  (>= cs 0)
                                                  cs))
                             (style . ,(cond
                                        ((symbol? cstyle)
                                         (string-append "border-collapse: "
                                                        (symbol->string cstyle)
                                                        ";"))
                                        ((string? cstyle)
                                         (string-append "border-collapse: separate; border-spacing="
                                                        cstyle))
                                        ((number? cstyle)
                                         (string-append "border-collapse: separate; border-spacing="
                                                        cstyle
                                                        "pt"))
                                        (else #f)))
                             (frame . ,(and frame
                                            (if (eq? frame 'none)
                                                "void"
                                                frame)))
                             (rules . ,(and (not (eq? rules 'header))
                                            rules))))
                (html-open 'tbody)))
   :after "</tbody></table>\n")

;*---------------------------------------------------------------------*/
;*    tr ...                                                           */
;*---------------------------------------------------------------------*/
(markup-writer 'tr
   :options '(:bg)
   :before (lambda (node engine)
	      (let ((bg (markup-option node :bg)))
                (html-open 'tr
                           `((class . ,(markup-class node))
                             (bgcolor . ,(and (html-color-spec? bg)
                                              bg))))))
   :after "</tr>\n")

;*---------------------------------------------------------------------*/
;*    tc ...                                                           */
;*---------------------------------------------------------------------*/
(markup-writer 'tc
   :options '(markup :width :align :valign :colspan :rowspan :bg)
   :before (lambda (node engine)
	      (let ((markup (or (markup-option node 'markup) 'td))
		    (width (markup-option node :width))
		    (align (markup-option node :align))
		    (valign (let ((v (markup-option node :valign)))
			       (cond
				  ((or (eq? v 'center)
				       (equal? v "center"))
				   "middle")
                                  (else
                                   v))))
		    (colspan (markup-option node :colspan))
		    (rowspan (markup-option node :rowspan))
		    (bg (markup-option node :bg)))
                (html-open markup
                           `((class . ,(markup-class node))
                             (width . ,(and width (html-width width)))
                             (align . ,align)
                             (valign . ,valign)
                             (colspan . ,colspan)
                             (rowspan . ,rowspan)
                             (bgcolor . ,(and (html-color-spec? bg)
                                              bg))))))
   :after (lambda (node engine)
	     (let ((markup (or (markup-option node 'markup)
                               'td)))
               (html-close markup))))

;*---------------------------------------------------------------------*/
;*    image ... @label image@                                          */
;*---------------------------------------------------------------------*/
(markup-writer 'image
   :options '(:file :url :width :height)
   :action (lambda (node engine)
	      (let* ((file (markup-option node :file))
		     (url (markup-option node :url))
		     (width (markup-option node :width))
		     (height (markup-option node :height))
		     (body (markup-body node))
		     (efmt (engine-custom engine 'image-format))
		     (img (or url (convert-image file
						 (if (list? efmt)
						     efmt
						     '("gif" "jpg" "png"))))))
		 (if (not (string? img))
		     (skribe-error 'html "Invalid image" file)
                     (html-open 'img
                                `((class . ,(markup-class node))
                                  (src . ,img)
                                  (border . "0")
                                  (alt . ,(if body
                                              (with-output-to-string
                                                (cut output body engine))
                                              file))
                                  (width . ,(and width
                                                 (html-width width)))
                                  (height . ,height)))))))

;*---------------------------------------------------------------------*/
;*    Ornaments ...                                                    */
;*---------------------------------------------------------------------*/
(markup-writer 'roman :before "")
(markup-writer 'bold
  :before (html-markup-class "strong" #f)
  :after "</strong>")
(markup-writer 'underline :before (html-markup-class "u") :after "</u>")
(markup-writer 'strike :before (html-markup-class "strike") :after "</strike>")
(markup-writer 'emph :before (html-markup-class "em") :after "</em>")
(markup-writer 'kbd :before (html-markup-class "kbd") :after "</kbd>")
(markup-writer 'it :before (html-markup-class "em") :after "</em>")
(markup-writer 'tt :before (html-markup-class "tt") :after "</tt>")
(markup-writer 'code :before (html-markup-class "code") :after "</code>")
(markup-writer 'var :before (html-markup-class "var") :after "</var>")
(markup-writer 'samp :before (html-markup-class "samp") :after "</samp>")
(markup-writer 'sc :before "<span class=\"sc\">" :after "</span>")
(markup-writer 'sf :before "<span class=\"sf\">" :after "</span>")
(markup-writer 'sub :before (html-markup-class "sub") :after "</sub>")
(markup-writer 'sup :before (html-markup-class "sup") :after "</sup>")

;*---------------------------------------------------------------------*/
;*    q ... @label q@                                                  */
;*---------------------------------------------------------------------*/
(markup-writer 'q
   :before "\""
   :after "\"")

;*---------------------------------------------------------------------*/
;*    mailto ... @label mailto@                                        */
;*---------------------------------------------------------------------*/
(markup-writer 'mailto
   :options '(:text)
   :action (lambda (node engine)
	      (let ((text (markup-option node :text)))
                (html-open 'a
                           `((class . ,(markup-class node))
                             (href . ,(string-append "mailto:"
                                                     (with-output-to-string
                                                       (cut output (markup-body node) engine))))))
		(if text
		    (output text engine)
		    (evaluate-document (tt (markup-body node))
                                       engine))
                (html-close 'a))))

;*---------------------------------------------------------------------*/
;*    mailto ... @label mailto@                                        */
;*---------------------------------------------------------------------*/
(define %non-at
  ;; Char-set not containing the `@' character.
  (char-set-complement (char-set #\@)))

(markup-writer 'mailto
   :options '(:text)
   :predicate (lambda (node engine)
		 (and (engine-custom engine 'javascript)
		      (or (string? (markup-body node))
			  (and (pair? (markup-body node))
			       (null? (cdr (markup-body node)))
			       (string? (car (markup-body node)))))))
   :action (lambda (node engine)
	      (let* ((body (markup-body node))
		     (email (if (string? body) body (car body)))
		     (split (string-tokenize email %non-at))
		     (na (car split))
		     (do (if (pair? (cdr split)) (cadr split) ""))
		     (nn (regexp-substitute/global #f "\\." na
                                                   'pre " " 'post))
		     (dd (regexp-substitute/global #f "\\." do
                                                   'pre " " 'post))
		     (text (markup-option node :text)))
                (html-open 'script
                           `((language . "JavaScript")
                             (type . "text/javascript")))
		(if (not text)
                    (format #t "skribenospam( ~s, ~s, true )" nn dd)
		    (begin
		      (format #t "skribenospam( ~s, ~s, false )" nn dd)
                      (html-close 'script)
		      (output text engine)
                      (html-open 'script
                                 `((language . "JavaScript")
                                   (type . "text/javascript")))
                      (display "document.write(\"</\" + \"a>\")")))
                (html-close 'script))))

;*---------------------------------------------------------------------*/
;*    mark ... @label mark@                                            */
;*---------------------------------------------------------------------*/
(markup-writer 'mark
   :before (lambda (node engine)
             (html-open 'a
                        `((class . ,(markup-class node))
                          (name . ,(string-canonicalize (markup-ident node))))))
   :after "</a>")

;*---------------------------------------------------------------------*/
;*    ref ... @label ref@                                              */
;*---------------------------------------------------------------------*/
(markup-writer 'ref
   :options '(:text :chapter :section :subsection :subsubsection :figure :mark :handle)
   :before (lambda (node engine)
	      (let* ((c (handle-ast (markup-body node)))
		     (id (markup-ident c))
		     (f (html-file c engine))
		     (class (if (markup-class node)
				(markup-class node)
				"skribilo-ref")))
                (html-open 'a
                           `((href . ,(string-append (if (and (*destination-file*) f
				                              (string=? f (*destination-file*)))
			                                 ""
			                                 (strip-ref-base (or f (*destination-file*) "")))
                                                     "#"
                                                     (string-canonicalize id)))
                             (class . ,class)))))
   :action (lambda (node engine)
	      (let ((t (markup-option node :text))
		    (m (markup-option node 'mark))
		    (f (markup-option node :figure))
		    (c (markup-option node :chapter))
		    (s (markup-option node :section))
		    (ss (markup-option node :subsection))
		    (sss (markup-option node :subsubsection)))
		 (cond
		    (t
		     (output t engine))
		    (f
		     (output (new markup
				(markup '&html-figure-ref)
				(body (markup-body node)))
			     engine))
		    ((or c s ss sss)
		     (output (new markup
				(markup '&html-section-ref)
				(body (markup-body node)))
			     engine))

		    ((not m)
		     (output (new markup
				(markup '&html-unmark-ref)
				(body (markup-body node)))
			     engine))
		    (else
		     (display m)))))
   :after "</a>")

;*---------------------------------------------------------------------*/
;*    &html-figure-ref ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&html-figure-ref
   :action (lambda (node engine)
	      (let ((c (handle-ast (markup-body node))))
		 (if (or (not (markup? c))
			 (not (is-markup? c 'figure)))
		     (display "???")
		     (output (markup-option c :number)
                             engine)))))

;*---------------------------------------------------------------------*/
;*    &html-section-ref ...                                            */
;*---------------------------------------------------------------------*/
(markup-writer '&html-section-ref
   :action (lambda (node engine)
	      (let ((c (handle-ast (markup-body node))))
		 (if (not (markup? c))
		     (display "???")
		     (output (markup-option c :title)
                             engine)))))

;*---------------------------------------------------------------------*/
;*    &html-unmark-ref ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&html-unmark-ref
   :action (lambda (node engine)
	      (let ((c (handle-ast (markup-body node))))
		 (if (not (markup? c))
		     (display "???")
		     (let ((t (markup-option c :title)))
			(if t
			    (output t engine)
			    (let ((l (markup-option c :legend)))
			       (if l
				   (output t engine)
				   (display
				    (string-canonicalize
				     (markup-ident c)))))))))))

;*---------------------------------------------------------------------*/
;*    bib-ref ...                                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'bib-ref
   :options '(:text :bib)
   :before "["
   :action (lambda (node engine)
             ;; Produce a hyperlink.
             (output node
                     engine
                     (markup-writer-get 'ref engine)))
   :after "]")

;*---------------------------------------------------------------------*/
;*    url-ref ...                                                      */
;*---------------------------------------------------------------------*/
(markup-writer 'url-ref
   :options '(:url :text)
   :before (lambda (node engine)
	      (let* ((url (markup-option node :url))
		     (class (cond
			       ((markup-class node)
				(markup-class node))
			       ((not (string? url))
				#f)
			       (else
				(let ((l (string-length url)))
				   (let loop ((i 0))
				      (cond
					 ((= i l)
					  #f)
					 ((char=? (string-ref url i) #\:)
					  (substring url 0 i))
					 (else
					  (loop (+ i 1))))))))))
                (html-open 'a
                           `((href . ,(with-output-to-string
                                        (cut output url html-title-engine)))
                             (class . ,class)))))
   :action (lambda (node engine)
	      (let ((v (markup-option node :text)))
		 (output (or v (markup-option node :url))
                         engine)))
   :after "</a>")


;*---------------------------------------------------------------------*/
;*    &prog-line ...                                                   */
;*---------------------------------------------------------------------*/
(markup-writer '&prog-line
   :before (lambda (node engine)
             (let ((before (writer-before
                            (markup-writer-get '&prog-line base-engine))))
               (html-open 'a
                          `((class . ,(markup-class node))
                            (name . ,(string-canonicalize (markup-ident node))))
                          #f)
               (before node engine)))
   :after "</a>\n")

;*---------------------------------------------------------------------*/
;*    line-ref ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'line-ref
   :options '(:offset)
   :before (html-markup-class "i")
   :action (lambda (node engine)
	      (let ((o (markup-option node :offset))
		    (v (markup-option (handle-ast (markup-body node)) :number)))
		 (cond ((and (number? o) (number? v))
                        (markup-option-set! node :text (+ o v)))
                       ((number? v)
                        (markup-option-set! node :text v)))
		 (output node
                         engine
                         (markup-writer-get 'ref engine))
		 (if (and (number? o) (number? v))
		     (markup-option-set! node :text v))))
   :after "</i>")

;*---------------------------------------------------------------------*/
;*    page-ref ...                                                     */
;*---------------------------------------------------------------------*/
(markup-writer 'page-ref
   :options '(:mark :handle)
   :action (lambda (node engine)
	      (error 'page-ref:html "Not implemented yet" node)))

;*---------------------------------------------------------------------*/
;*    &bib-entry-label ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-label
   :options '(:title)
   :before (lambda (node engine)
             (html-open 'a
                        `((class . ,(markup-class node))
                          (name . ,(string-canonicalize (markup-ident node))))))
   :action (lambda (node engine)
	      (output node
                      engine
                      (markup-writer-get '&bib-entry-label base-engine)))
   :after "</a>")

;*---------------------------------------------------------------------*/
;*    &bib-entry-title ...                                             */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-title
   :action (lambda (node engine)
	      (let* ((t (bold (markup-body node)))
		     (en (handle-ast (ast-parent node)))
		     (url (or (markup-option en 'url)
			      (markup-option en 'documenturl)))
		     (ht (if url (ref :url (markup-body url) :text t) t)))
		 (evaluate-document ht engine))))

;*---------------------------------------------------------------------*/
;*    &bib-entry-url ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&bib-entry-url
    :action (lambda (node engine)
	      (let* ((en (handle-ast (ast-parent node)))
		     (url (markup-option en 'url))
		     (t (bold (markup-body url))))
		(evaluate-document (ref :url (markup-body url) :text t)
                                   engine))))

;*---------------------------------------------------------------------*/
;*    &the-index-header ...                                            */
;*---------------------------------------------------------------------*/
(markup-writer '&the-index-header
    :action (lambda (node engine)
              (html-open 'center
                         `((class . ,(markup-class node))))
	      (for-each (lambda (h)
			  (let ((f (engine-custom engine 'index-header-font-size)))
			    (if f
			        (evaluate-document (font :size f (bold (it h)))
                                                   engine)
			        (output h engine))
			    (display " ")))
		        (markup-body node))
              (html-close 'center)
	      (evaluate-document (linebreak 2) engine)))

;*---------------------------------------------------------------------*/
;*    &source-comment ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer '&source-comment
    :action (lambda (node engine)
	      (let* ((cc (engine-custom engine 'source-comment-color))
		     (node1 (it (markup-body node)))
		     (node2 (if (and (engine-custom engine 'source-color)
                                     cc)
			        (color :fg cc node1)
			        node1)))
	        (evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    &source-line-comment ...                                         */
;*---------------------------------------------------------------------*/
(markup-writer '&source-line-comment
    :action (lambda (node engine)
	      (let* ((cc (engine-custom engine 'source-comment-color))
		     (node1 (bold (markup-body node)))
		     (node2 (if (and (engine-custom engine 'source-color)
                                     cc)
			        (color :fg cc node1)
			        node1)))
	        (evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    &source-keyword ...                                              */
;*---------------------------------------------------------------------*/
(markup-writer '&source-keyword
    :action (lambda (node engine)
	      (evaluate-document (bold (markup-body node))
                                 engine)))

;*---------------------------------------------------------------------*/
;*    &source-error ...                                                */
;*---------------------------------------------------------------------*/
(markup-writer '&source-error
    :action (lambda (node engine)
	      (let* ((cc (engine-custom engine 'source-error-color))
		     (node1 (bold (markup-body node)))
		     (node2 (if (and (engine-custom engine 'source-color)
                                     cc)
			        (color :fg cc node1)
			        node1)))
	        (evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    &source-define ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-define
    :action (lambda (node engine)
 	      (let* ((cc (engine-custom engine 'source-define-color))
		     (node1 (bold (markup-body node)))
		     (node2 (if (and (engine-custom engine 'source-color) cc)
			        (color :fg cc node1)
			        node1)))
	        (evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    &source-module ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-module
    :action (lambda (node engine)
	      (let* ((cc (engine-custom engine 'source-module-color))
		     (node1 (bold (markup-body node)))
		     (node2 (if (and (engine-custom engine 'source-color) cc)
			        (color :fg cc node1)
			        node1)))
	        (evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    &source-markup ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-markup
    :action (lambda (node engine)
	      (let* ((cc (engine-custom engine 'source-markup-color))
		     (node1 (bold (markup-body node)))
		     (node2 (if (and (engine-custom engine 'source-color)
                                     cc)
			        (color :fg cc node1)
			        node1)))
	        (evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    &source-thread ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-thread
    :action (lambda (node engine)
	      (let* ((cc (engine-custom engine 'source-thread-color))
		     (node1 (bold (markup-body node)))
		     (node2 (if (and (engine-custom engine 'source-color)
                                     cc)
			        (color :fg cc node1)
			        node1)))
	        (evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    &source-string ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-string
    :action (lambda (node engine)
	      (let* ((cc (engine-custom engine 'source-string-color))
		     (node1 (markup-body node))
		     (node2 (if (and (engine-custom engine 'source-color)
                                     cc)
			        (color :fg cc node1)
			        node1)))
		(evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    &source-bracket ...                                               */
;*---------------------------------------------------------------------*/
(markup-writer '&source-bracket
    :action (lambda (node engine)
	      (let* ((cc (engine-custom engine 'source-bracket-color))
		     (node1 (markup-body node))
		     (node2 (if (and (engine-custom engine 'source-color) cc)
			        (color :fg cc (bold node1))
			        (bold node1))))
		(evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    &source-type ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer '&source-type
    :action (lambda (node engine)
	      (let* ((cc (engine-custom engine 'source-type-color))
		     (node1 (markup-body node))
		     (node2 (if (and (engine-custom engine 'source-color) cc)
			        (color :fg cc node1)
			        (it node1))))
		(evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    &source-key ...                                                  */
;*---------------------------------------------------------------------*/
(markup-writer '&source-key
    :action (lambda (node engine)
	      (let* ((cc (engine-custom engine 'source-type-color))
		     (node1 (markup-body node))
		     (node2 (if (and (engine-custom engine 'source-color) cc)
			        (color :fg cc (bold node1))
			        (it node1))))
	        (evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    &source-type ...                                                 */
;*---------------------------------------------------------------------*/
(markup-writer '&source-type
    :action (lambda (node engine)
	      (let* ((cc (engine-custom engine 'source-type-color))
		     (node1 (markup-body node))
		     (node2 (if (and (engine-custom engine 'source-color)
                                     cc)
			        (color :fg "red" (bold node1))
			        (bold node1))))
	        (evaluate-document node2 engine))))

;*---------------------------------------------------------------------*/
;*    Restore the base engine                                          */
;*---------------------------------------------------------------------*/
(default-engine-set! (find-engine 'base))


;;; Local Variables:
;;; mode: scheme
;;; End:
