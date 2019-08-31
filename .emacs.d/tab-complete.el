
;; Small primer in case I forget things...
;; dabbrev is "Dynamic Abbreviations".
;; dabbrev-expand takes the word to the left of 'point', and tries to
;; expand it based on the text of the current buffer (or other buffers).
;;
;; It's not a great autocompletion framework, but it is at least minimal,
;; and always available.
;;
;; TODO:
;; * Investigate other frameworks (company, ivy, ...) for other languages?
;; * Figure out a way to display candidates in an overlay?

(require 'dabbrev)

(defun dabbrev-completion-at-point ()
  "Provides completion at point via dabbrev.  This won't provide
high quality results, but it requires no dependencies or additional setup."
  (dabbrev--reset-global-variables)
  (let* ((abbrev (dabbrev--abbrev-at-point))
	 (candidates (dabbrev--find-all-expansions abbrev t))
	 (bnd (bounds-of-thing-at-point 'symbol)))
    (list (car bnd) (cdr bnd) candidates)))
(add-to-list 'completion-at-point-functions 'dabbrev-completion-at-point)

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibufferp)
        (dabbrev-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (progn
            (completion-at-point)
            (switch-to-completions)
            )
        (dabbrev-expand nil)
        (indent-for-tab-command)))))
(global-set-key (kbd "TAB") 'smart-tab)

(setq completion-cycle-threshold 100)
