(setq orgchange-theme-dir default-directory
	  prev-link "#"
	  prev-title ""
	  next-link "#"
	  next-title ""
	  github-issue-link "#"
)

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

   (read-file-content (expand-file-name "head.html" orgchange-theme-dir ))
   "</head>\n"
   "<body>\n"
   "<script>\n"
   (read-file-content 
   	(expand-file-name "afterBodyBegin.js" orgchange-theme-dir )) 
   "</script>\n"
   "<div class=\"container\">\n"
   (read-file-content (expand-file-name "header.html" orgchange-theme-dir))
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
					(or (plist-get info :title) user-settings-blog-title)
					))
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

   ;;categories, prev post, next post and comment link
   (let* 
   	((categories-list (split-string categories ","))
     (categories-ele 
	  (if (not (equal categories ""))
	   (mapconcat 
	     'identity
	  	  (mapcar 
	   	    (lambda (x) 
	   		  (format "<a href=\"%s\" target=\"_blank\">%s</a>" 
		   		(format "/categories/%s.html" x)
			    (string-trim x)))
		 	categories-list) "") "")
		  ))
    
	(format
		(read-file-content 
		(expand-file-name "article_footer.html" orgchange-theme-dir )) 
			categories-ele prev-link prev-title next-link next-title github-issue-link)
		  )
			
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
   (read-file-content 
		(expand-file-name "beforeBodyEnd.js" orgchange-theme-dir )) 
	"</script>\n"
   "\n</body>\n</html>"))


(setq org-html-checkbox-type 'unicode ;; ascii
      org-export-headline-levels 3
      org-export-with-toc t
	  org-export-with-section-numbers nil)


(setq org-html-preamble t)
(setq org-html-postamble t)
(setq org-html-preamble-format `(("en" ,(read-file-content "preamble.html"))))
(setq org-html-postamble-format `(("en"
                                   ,(concat (format-time-string "<span> © %G</span>")
                                            (read-file-content "postamble.html")))))

;; (prin1 "Ok , this publish system don't need htmlize.el\n")
