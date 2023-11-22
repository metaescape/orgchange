;;; -*- lexical-binding: t; -*-

(require 'ox-html)
(require 'org-id)
(require 'oc)

(add-to-list 'load-path "../../elpa/citeproc-20220623.1550/")
(add-to-list 'load-path "../../elpa/dash-20220608.1931")
(add-to-list 'load-path "../../elpa/queue-0.2")
(add-to-list 'load-path "../../elpa/s-20210616.619")
(add-to-list 'load-path "../../elpa/f-20220608.943")
(add-to-list 'load-path "../../elpa/parsebib-20220620.2207/")

(require 'oc-csl)

(setq org-cite-export-processors '((t csl)))

(prin1 (format "load package %s \n" (locate-library "ox-html")) )
;; disable files end with ~
(setq make-backup-files nil)
(setq user-settings-blog-title nil)
(setq multi-page-index nil)
;; disable default css, use html5 sematic tags
(setq org-html-style-default ""
      org-html-html5-fancy 't
      org-html-doctype "html5"
      org-export-use-babel nil
      org-export-with-sub-superscripts nil
      org-html-container-element "section"
      org-export-with-broken-links 'mark
      )

(setq org-html-checkbox-type 'unicode ;; ascii
      org-export-headline-levels 3
      org-export-with-toc t
	    org-export-with-section-numbers nil)

(setq org-html-preamble t)
;; (setq org-html-postamble t)
  
(setq org-html-preamble-format
      `(("en" "<span id=\"created-timestamp\">%d</span>
	  		   <span id=\"last-modify-timestamp\">%C</span>
			   <span id=\"emacs-org-version\">%c</span>"
			   )))
         

(setq org-html-divs
      '((preamble "header" "preamble")
        (content "main" "content")
        (postamble "footer" "postamble")))


(setq org-html-text-markup-alist
      '((bold . "<strong>%s</strong>")
      (code . "<code>%s</code>")
      (italic . "<i>%s</i>")
      (strike-through . "<del>%s</del>")
      (underline . "<span class=\"underline\">%s</span>")
      (verbatim . "<code>%s</code>")))
;; end of html5 sematic tags


(defun read-file-content (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq org-html-scripts "") ;; disable default js for code highlght
;; add an inner <code> element for code block
(defun org-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
	       (code (org-html-format-code src-block info))
	       (label (let ((lbl (org-html--reference src-block info t)))
		            (if lbl (format " id=\"%s\"" lbl) "")))
	       (klipsify  (and  (plist-get info :html-klipsify-src)
                            (member lang '("javascript" "js"
					                       "ruby" "scheme" "clojure" "php" "html")))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
	    (format "<div class=\"org-src-container\">\n%s%s\n</div>"
		        ;; Build caption.
		        (let ((caption (org-export-get-caption src-block)))
		          (if (not caption) ""
		            (let ((listing-number
			               (format
			                "<span class=\"listing-number\">%s </span>"
			                (format
			                 (org-html--translate "Listing %d:" info)
			                 (org-export-get-ordinal
			                  src-block info nil #'org-html--has-caption-p)))))
		              (format "<label class=\"org-src-name\">%s%s</label>"
			                  listing-number
			                  (org-trim (org-export-data caption info))))))
		        ;; Contents.
		        (if klipsify
		            (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
			                lang
			                label
			                (if (string= lang "html")
				                " data-editor-type=\"html\""
			                  "")
			                code)
                  ;; code add
                  (format "<pre><code class=\"%s\"%s>%s</code></pre>"
                          lang label code)))))))


(defun org-export-each-headline-to-html (&optional dir scope)
  "Export each headline to a html file with the title as filename.
If SCOPE is nil headlines in the current buffer are exported.
For other valid values for SCOPE see `org-map-entries'.
Already existing files are overwritten."
  ;; Widen buffer temporarily as narrowing would affect the exporting.
  (setq multi-page-index 0)
  (org-with-wide-buffer
   (save-mark-and-excursion
     ;; Loop through each heading.
     (org-map-entries
      (lambda ()
        ;; Get the plain heading text without statistics and make filename.
        (let* ((title (car (last (org-get-outline-path t))))
               (dir (if dir dir	default-directory))
               (dirname (file-name-nondirectory
                         (directory-file-name dir)))
               (filename (if (= multi-page-index 0)
                             (concat dir "/index.html")
                             (concat dir "/" (format "%s_%s.html" dirname multi-page-index))))
                             )
          ;;Set the active region.
		  
		  ;; 不显示一级标题
          ;; (set-mark (line-end-position)) 
          ;; (forward-line 1)

		  ;; 显示一级标题，会与 title 重复
		    (set-mark (point))
		    (outline-next-visible-heading 1)
          ;;   (outline-next-preface) ;; any level
		  (if (search-forward-regexp "^* " nil t)
          (forward-char -2)
			    (goto-char (point-max)))
          
          (activate-mark)
          ;;Export the region to a html file.
		  (setq user-settings-blog-title title)
      (with-current-buffer (org-html-export-as-html)
        ;; Save the buffer to file and kill it.
        (write-file filename)
        (kill-current-buffer))
      (setq multi-page-index (1+ multi-page-index))
        ))
      "+LEVEL=1-noexport" scope))))

;; https://orgmode.org/manual/Matching-tags-and-properties.html#Matching-tags-and-properties

(defun change/org-modify-cite-links (backend)
  "Modify cite links in the current buffer for export."
  (goto-char (point-min))
  (while (re-search-forward "\\[\\[cite:&\\(.*?\\)\\]\\]" nil t)
    (unless (org-between-regexps-p "^#\\+begin_" "^#\\+end_")
      (replace-match "[cite:@\\1]" nil nil))))

(add-hook 'org-export-before-parsing-hook 'change/org-modify-cite-links)

(defun change/org-make-add-bibliography (bib-file csl-file-path csl-style-option)
  (lambda (backend)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^#\\+title:" nil t)
      (forward-line)
      (let ((current-point (point)))
        (goto-char (point-min))
        (unless (re-search-forward "^#\\+bibliography:" nil t)
          (goto-char current-point)
          (insert (concat "#+bibliography: " bib-file "\n"))
          (insert (concat "#+cite_export: csl " csl-file-path " " csl-style-option "\n")))
        (unless (re-search-forward "^#\\+print_bibliography:" nil t)
          (goto-char (point-max))
          (insert "\n* 参考文献\n#+print_bibliography:"))))
          ))

;; just set a placeholder for mathjax, the real <script> will add dynamically in front-end js loading
(defun org-html--build-mathjax-config (info)
  "Insert the user setup into the mathjax template.
INFO is a plist used as a communication channel."
  (when (and (memq (plist-get info :with-latex) '(mathjax t))
	     (org-element-map (plist-get info :parse-tree)
		 '(latex-fragment latex-environment) #'identity info t nil t))
    "<script id=\"need-mathjax\"></script>"))


;; https://emacs.stackexchange.com/questions/28301/export-javascript-source-block-to-script-tag-in-html-when-exporting-org-file-to
(add-to-list 'org-src-lang-modes '("inline-js" . javascript)) ;; js2 if you're fancy
(defvar org-babel-default-header-args:inline-js
  '((:results . "html")
    (:exports . "results")))
(defun org-babel-execute:inline-js (body _params)
  (format "<script type=\"text/javascript\">\n%s\n</script>" body))

(add-to-list 'org-src-lang-modes '("module-js" . javascript)) ;; js2 if you're fancy
(defvar org-babel-default-header-args:module-js
  '((:results . "html")
    (:exports . "results")))
(defun org-babel-execute:module-js (body _params)
  (format "<script type=\"module\">\n%s\n</script>" body))


;; (setq orgchange-theme-dir default-directory
;; 	  github-issue-link "#")

;; add placehoder for custom header
;; add an inner div with class "container" in <body>
;; move preamble below title
;; add a horizontal line after preamble
(defun org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	        (decl (or (and (stringp xml-declaration) xml-declaration)
		              (cdr (assoc (plist-get info :html-extension)
				                  xml-declaration))
		              (cdr (assoc "html" xml-declaration))
		              "")))
       (when (not (or (not decl) (string= "" decl)))
	     (format "%s\n"
		         (format decl
			             (or (and org-html-coding-system
				                  (fboundp 'coding-system-get)
				                  (coding-system-get org-html-coding-system 'mime-charset))
			                 "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	       (cond ((org-html-xhtml-p info)
		          (format
		           " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
		           (plist-get info :language) (plist-get info :language)))
		         ((org-html-html5-p info)
		          (format " lang=\"%s\"" (plist-get info :language))))
	       ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   ;; add head

;;    (read-file-content (expand-file-name "head.html" orgchange-theme-dir ))
   "</head>\n"
   "<body>\n"
   "<script>\n"
;;    (read-file-content 
;;    	(expand-file-name "afterBodyBegin.js" orgchange-theme-dir )) 
   "</script>\n"
   "<div class=\"container\">\n"
  ;;  (read-file-content (expand-file-name "header.html" orgchange-theme-dir))
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	     (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
	           (or link-up link-home)
	           (or link-home link-up))))
   ;;    ;; Preamble.
   ;;    (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
   ;; Document title.
   (when (plist-get info :with-title)
	 (let* ((origin-title (org-export-data (plist-get info :title) info))
         (title (and (plist-get info :with-title)
	 				;; (plist-get info :title) ;;original
                       (or (and user-settings-blog-title (format "%s:%s" origin-title multi-page-index))
                           (plist-get info :title))
					   ))
		   (subtitle (plist-get info :subtitle)) ;;original
		  ;;  (subtitle
			;; 	(if (null multi-page-index)
			;; 		(plist-get info :subtitle)
			;; 	(format "%s" multi-page-index)))

		   (html5-fancy (org-html--html5-fancy-p info)))
	   (when title
		 (format
		  (if html5-fancy
			  "<header class=\"header-titles\">\n<h1 class=\"title\">%s</h1>\n%s</header>"
		    "<h1 class=\"title\">%s%s</h1>\n")
		  (org-export-data title info)
		  (if subtitle
			  (format
			   (if html5-fancy
				   "<p class=\"subtitle\">%s</p>\n"
				 (concat "\n" (org-html-close-tag "br" nil info) "\n"
						 "<span class=\"subtitle\">%s</span>\n"))
			   (org-export-data subtitle info))
		    "")))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;;    "<hr>" ;; add a horizontal line
   "<div id=\"org-main\">\n"
   contents
   "</div>\n"

   ;;categories, prev post, next post and comment link
;;    (let* 
;;    	   ((categories-list (split-string categories ","))
;;         (categories-ele 
;; 	     (if (not (equal categories ""))
;; 	         (mapconcat 
;; 	          'identity
;; 	  	      (mapcar 
;; 	   	       (lambda (x) 
;; 	   		     (format "<a href=\"%s\" target=\"_blank\">%s</a>" 
;; 		   		         (format "/categories/%s.html" x)
;; 			             (string-trim x)))
;; 		 	   categories-list) "") "")
;; 		 ))
     
;; 	 (format
;; 	  (read-file-content 
;; 	   (expand-file-name "article_footer.html" orgchange-theme-dir )) 
;; 	  categories-ele github-issue-link)
;; 	 )
   
   ;; close main
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   "\n</div>"
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
	         "</.script><script src=\""
	         org-html-klipse-js
	         "\"></><link rel=\"stylesheet\" type=\"text/css\" href=\""
	         org-html-klipse-css "\"/>"))
   ;; Closing document.
   "<script>\n"
;;    (read-file-content 
	;; (expand-file-name "beforeBodyEnd.js" orgchange-theme-dir )) 
   "</script>\n"
   "\n</body>\n</html>"))



;; (setq org-html-preamble-format
;;       `(("en" ,(read-file-content "preamble.html"))))
;; (setq org-html-postamble-format
;;       `(("en"
;;          ,(concat (format-time-string "<span> © %G</span>")
;;                   (read-file-content "postamble.html")))))

;; (prin1 "Ok , this publish system don't need htmlize.el\n")

;; use ID as org-id, use fixed id instead of random id
(defun org-html--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets."
  (let* ((type (org-element-type datum))
	 (user-label
	  (org-element-property
	   (pcase type
	     ((or `headline `inlinetask) :ID)
	     ((or `radio-target `target) :value)
	     (_ :name))
	   datum)))
    (cond
     ((and user-label
	   (or (plist-get info :html-prefer-user-labels)
	       ;; Used CUSTOM_ID property unconditionally.
	       (memq type '(headline inlinetask))))
      user-label)
     ((and named-only
	   (not (memq type '(headline inlinetask radio-target target)))
	   (not user-label))
      nil)
     (t
      (org-export-get-reference datum info)))))
