(require 'ox-html)
(require 'org-id)
(require 'oc)

(add-to-list 'load-path "~/.emacs.d/elpa/citeproc-20220623.1550/")
(add-to-list 'load-path "~/.emacs.d/elpa/dash-20220608.1931")
(add-to-list 'load-path "~/.emacs.d/elpa/queue-0.2")
(add-to-list 'load-path "~/.emacs.d/elpa/s-20210616.619")
(add-to-list 'load-path "~/.emacs.d/elpa/f-20220608.943")
(add-to-list 'load-path "~/.emacs.d/elpa/parsebib-20220620.2207/")

(require 'oc-csl)

;; (setq org-cite-export-processors '((t csl)))

(prin1 (format "load package %s \n" (locate-library "ox-html")) )
;; disable files end with ~
(setq make-backup-files nil)
(setq user-settings-blog-title nil)
;; disable default css, use html5 sematic tags
(setq org-html-style-default ""
      org-html-html5-fancy 't
      org-html-doctype "html5"
      org-export-use-babel nil
      org-export-with-sub-superscripts nil
      org-html-container-element "section"
      org-export-with-broken-links t
      )

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
  (interactive)
  ;; Widen buffer temporarily as narrowing would affect the exporting.
  (org-with-wide-buffer
   (save-mark-and-excursion
     ;; Loop through each headline.
     (org-map-entries
      (lambda ()
        ;; Get the plain headline text without statistics and make filename.
        (let* ((title (car (last (org-get-outline-path t))))
               (dir (if dir dir	(file-name-directory buffer-file-name)
			   	))
               (filename (concat dir "/" title ".html")))
          ;;Set the active region.
		  
		  ;; 不显示一级标题
        (set-mark (line-end-position)) 
        (forward-line 1)

		;; 显示一级标题，会与 title 重复
		;;   (set-mark (point))
		;;   (outline-next-visible-heading 1)
        ;;   (outline-next-preface) ;; any level
		  (if (search-forward-regexp "^* " nil t)
        (forward-char -2)
				  (goto-char (point-max))
				)
        
        (activate-mark)
          ;;Export the region to a html file.
		  (setq user-settings-blog-title title)
      (with-current-buffer (org-html-export-as-html)
        ;; Save the buffer to file and kill it.
        (write-file filename)
        (kill-current-buffer))
          ))
      "+LEVEL=1-noexport" scope))))
;; https://orgmode.org/manual/Matching-tags-and-properties.html#Matching-tags-and-properties