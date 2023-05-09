(require 'ox-html)

(prin1 (format "load package %s \n" (locate-library "ox-html")) )

;; disable files end with ~
(setq make-backup-files nil)

;; disable default css, use html5 sematic tags
(setq org-html-style-default ""
      org-html-html5-fancy 't
      org-html-doctype "html5"
      org-export-use-babel nil
      org-export-with-sub-superscripts nil
      org-html-container-element "section"
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
                  (format "<pre><code class=\"%s\"%s>%s</code></pre>"
                          lang label code)))))))
