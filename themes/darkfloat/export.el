(load-file "../general.el")
(setq currrent-theme-dir default-directory)

;; add an innter <code> element for code block
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
                  (format "<pre><code class=\"%s\"%s>%s</code></pre>"
                          lang label code)))))))

;; add placehoder for custom header
;; add an innter div with class "container" in <body>
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
   (let ((relative-dir (file-relative-name currrent-theme-dir default-directory)))
   	 (format (read-file-content (expand-file-name "head.html" currrent-theme-dir )) relative-dir relative-dir)
     )
   "</head>\n"
   "<body>\n<div class=\"container\">\n"
   (read-file-content (expand-file-name "header.html" currrent-theme-dir))
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
     (let ((title (and (plist-get info :with-title)
		               (plist-get info :title)))
	       (subtitle (plist-get info :subtitle))
	       (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	     (format
	      (if html5-fancy
	          "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
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
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
	         "</script><script src=\""
	         org-html-klipse-js
	         "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
	         org-html-klipse-css "\"/>"))
   ;; Closing document.
   "\n</div>\n</body>\n</html>"))


(setq org-html-checkbox-type 'unicode ;; ascii
      org-export-headline-levels 3
      org-export-with-toc t)


(setq org-html-scripts "") ;; disable default js for code highlght

(setq org-html-preamble t)
(setq org-html-postamble t)
(setq org-html-preamble-format `(("en" ,(read-file-content "preamble.html"))))
(setq org-html-postamble-format `(("en"
                                   ,(concat (format-time-string "<span> © %G</span>")
                                            (read-file-content "postamble.html")))))

;; (prin1 "Ok , this publish system don't need htmlize.el\n")
