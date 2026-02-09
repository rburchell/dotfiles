;;; rb-org.el -*- lexical-binding: t; -*-

;; Org stuff ...
;; Some inspiration from: https://karelvo.com/blog/orgmode/

;; Org capture
(global-set-key (kbd "C-c c") #'org-capture)

(after! org
  (defvar rb/yt-iframe-format
    (concat "<iframe width=\"427\""
            " height=\"240\""
            " src=\"https://www.youtube.com/embed/%s\""
            " frameborder=\"0\""
            " allowfullscreen>%s</iframe>"))

  (org-add-link-type
   "yt"
   (lambda (handle)
     (browse-url (concat "https://www.youtube.com/watch?v=" handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format rb/yt-iframe-format path (or desc "")))
       (latex (format "\href{%s}{%s}" path (or desc "video"))))))

  (org-add-link-type
   "qt"
   (lambda (handle)
     (browse-url (concat "https://qt-project.atlassian.net/browse/" handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format "<a href=\"%s\">%s</a>" path (or desc "")))
       (latex (format "\href{%s}{%s}" path (or desc "bugreport"))))))

  (org-add-link-type
   "rm"
   (lambda (handle)
     (browse-url (concat "https://getremarkable.atlassian.net/browse/" handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format "<a href=\"%s\">%s</a>" path (or desc "")))
       (latex (format "\href{%s}{%s}" path (or desc "bugreport"))))))

  (defun rb/org-youtube-transform-on-yank (orig-fun &rest args)
    "Replace YouTube URLs with Org yt links after yank in org-mode."
    (let ((start (point)))
      (apply orig-fun args)
      (when (derived-mode-p 'org-mode)
        (let ((end (point)))
          (save-excursion
            (goto-char start)
            (while (re-search-forward
                    "\\(?:https?://\\)?\\(?:www\\.\\)?\\(?:youtube\\.com/watch\\?v=\\|youtube\\.com/\\?v=\\|youtu\\.be/\\)\\([a-zA-Z0-9_-]+\\)"
                    end t)
              (replace-match "[[yt:\\1]]" nil nil)))))))

  (advice-add 'yank :around #'rb/org-youtube-transform-on-yank)

  ;; START BLOG

  (defun rb/org-blog-html-headline (headline contents info)
    (let* ((raw-value (org-element-property :raw-value headline))
           (anchor (secure-hash 'md5 raw-value))
           (ts (or (org-element-property :POST_TIME headline) ""))
           (pretty-ts (replace-regexp-in-string "[<>]" "" ts)))
      (format "<h2 id=\"%s\" title=\"%s\"><a href=\"#%s\">%s</a></h2>\n%s"
              anchor pretty-ts anchor raw-value contents)))

  (defvar rb/org-blog-publish-dir "~/")

  (defun rb/org-blog-new-post ()
    (interactive)
    (find-file "~/src/workspace/.org/blog.org")
    (goto-char (point-max))
    (let* ((ts (format-time-string "%Y-%m-%d %a %H:%M"))
           ;; read-string will show an empty string but pick a default. read-from-minibuffer will show the default.
           ;; not sure which behavior I like better..
           ;; (title (read-string "Post title: " nil nil ts)))
           (title (read-from-minibuffer "Post title: " ts)))
      (org-insert-heading)
      (insert title)
      (org-set-tags '("draft"))
      (org-set-property "POST_TIME" (format "<%s>" ts))
      (forward-line -1)
      ;; (org-fold-show-entry)
      (goto-char (point-max))
      ))

  (defun rb/org-blog-publish ()
    (interactive)
    ;; Kind of a dirty hack, but otherwise, org-export-with-* aren't defined for us to redefine below.
    (require 'ox-html)
    (let ((accum (make-hash-table :test 'equal)))
      (dolist (file (org-agenda-files))
        (message "Publishing %S" file)
        (with-current-buffer (find-file-noselect file)
          (org-element-map (org-element-parse-buffer) 'headline
            (lambda (h)
              ;; (message "Headline %S" h)
              (let ((timestamp (org-element-property :POST_TIME h)))
                (when timestamp
                  (let ((tags (org-element-property :tags h))
                        (begin (org-element-property :begin h)))
                    (unless (member "draft" tags)
                      (save-excursion
                        (goto-char begin)
                        (org-narrow-to-subtree)
                        (unwind-protect
                            (let ((org-export-with-toc nil)
                                  (org-export-with-section-numbers nil))
                              (let* ((year (format-time-string "%Y" (org-time-string-to-time timestamp)))
                                     (html (org-export-string-as (buffer-string) 'rb/blog-html t))
                                     (existing (gethash year accum '())))
                                (puthash year
                                         (cons (list timestamp html) existing)
                                         accum))
                              )
                          (widen)
                          ))))))))))

      (let ((years (sort (hash-table-keys accum) #'string<)))
        (dolist (year years)
          (with-temp-file (expand-file-name (concat year ".html") rb/org-blog-publish-dir)
            (message "Writing %S" year)
            (insert "<html>\n")
            (insert org-html-head-extra)
            (insert "<body>\n")
            (insert "<h1>Rob's Blog</h1>\n")
            (dolist (link-year years)
              (insert (format "<a href=\"%s.html\">%s</a> " link-year link-year)))
            (insert "<hr>\n")


            (let* ((post-tuples (gethash year accum))
                   (sorted (sort post-tuples
                                 (lambda (a b) (string> (car a) (car b))))))
              (dolist (tuple sorted)
                (let ((timestamp (car tuple))
                      (html (cadr tuple)))
                  (insert (format "%s<hr>" html)))))

            (dolist (link-year years)
              (insert (format "<a href=\"%s.html\">%s</a> " link-year link-year)))

            (insert "</body></html>\n"))))))

  ;; END BLOG


  (defun rb/org-gptel-archive ()
    "Archives the current gptel thread, and starts a new one."
    (interactive)
    (let ((new-name (read-string "Rename current heading to: ")))
      (org-edit-headline new-name))
    (org-delete-property "UNFOLD")
    (org-insert-heading)
    (insert "Untitled")
    (org-set-property "UNFOLD" "t")
    (while (save-excursion
             (org-backward-heading-same-level 1))
      (org-move-subtree-up)))

  (setq org-capture-templates
        (quote (
                ("t" "todo" entry (file "~/src/workspace/.org/index.org") "* TODO ")
                ("m" "Meeting" entry (file "~/src/workspace/.org/index.org")
                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("p" "Phone call" entry (file "~/src/workspace/.org/index.org")
                 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                ("v" "clip to inbox" entry (file "~/src/workspace/.org/index.org") "* %(gui-get-selection 'CLIPBOARD)%?")

                )))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "KILL(k)")))


  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "green" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAIT" :foreground "orange" :weight bold)
                ("KILL" :foreground "magenta" :weight bold))))

  (defun rb/org-insert-now-time ()
    "Insert org-mode timestamp with current date and time."
    (interactive)
    (org-insert-time-stamp (current-time) t))

  (defun rb/org-insert-now-date ()
    "Insert org-mode timestamp with current date."
    (interactive)
    (org-insert-time-stamp (current-time)))

  )

;; Maximise the window when we start.. FIXME: This does not belong in this file
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(after! ox-html
  (org-export-define-derived-backend 'rb/blog-html 'html
    :translate-alist
    '((headline . rb/org-blog-html-headline)))

  ;; Applies special formatting to @tag and #tag from doom.
  (defun rb/org-html-keyword-highlight (text backend _info)
    (when (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string
       "\\(^\\|\\s-\\)@\\(\\w+\\)"
       "\\1<span class=\"at-tag\">@\\2</span>"
       (replace-regexp-in-string
        "\\(^\\|\\s-\\)#\\(\\w+\\)"
        "\\1<span class=\"hash-tag\">#\\2</span>"
        text))
      ))


  ;; Add CSS used by the above formatting.
  (setq org-html-head-extra
        "<style>
blockquote {
        margin:0.5em;
        border-left:1px solid #bbb;
        padding-left:0.5em;
}
blockquote > p {
        margin: 0em; padding: 0em;
}
p {
margin-block-start: 1em;
margin-block-end: 1em;
}
ul {
margin-block-start: 0em;
margin-block-end: 0em;
}
.at-tag { color: #0055AA; font-weight: bold; }
.hash-tag { color: #228800; font-style: italic; }
.TODO { color: #22ee22; font-style: bold; }
.WAIT { color: #ffbf00; font-style: bold; }
.DONE { color: #a9a9a9; font-style: bold; }
.KILL { color: #ee2222; font-style: bold; }
</style>")

  (add-to-list 'org-export-filter-plain-text-functions
               'rb/org-html-keyword-highlight))
