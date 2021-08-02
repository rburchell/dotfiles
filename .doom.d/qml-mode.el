;; QML Mode

(defvar qml-mode-hook nil)

(let ((qml-highlight-blue "MediumBlue")      ;#0000cd
      (qml-highlight-orchid "DarkOrchid")    ;#9932cc
      (qml-highlight-olive "OliveDrab")      ;#6b8e23
      (qml-highlight-red "red4")             ;#8b0000
      (qml-highlight-violet "MediumPurple3") ;#8968cd
      (qml-highlight-green "SeaGreen4")      ;#698b69
      (qml-highlight-royal-blue "RoyalBlue2");#436eee
      )

  (defface qml-declaration-face
    `((t :foreground ,qml-highlight-blue))
    "Face for element declaration.")

  (defvar qml-declaration-face 'qml-declaration-face)
  (setq qml-declaration-face font-lock-variable-name-face)

  (defface qml-preprocessor-face
    `((t :foreground ,qml-highlight-orchid))
    "Face for preprocessor.")
  (defvar qml-preprocessor-face 'qml-preprocessor-face)
  (setq qml-preprocessor-face font-lock-keyword-face)

  (defface qml-package-face
    `((t :foreground ,qml-highlight-olive))
    "Face for package name.")
  (defvar qml-package-face 'qml-package-face)
  (setq qml-package-face font-lock-function-name-face)

  (defface qml-package-version-face
    `((t :foreground ,qml-highlight-red))
    "Face for package version.")
  (defvar qml-package-version-face 'qml-package-version-face)
  (setq qml-package-version-face font-lock-constant-face)

  (defface qml-property-def-keyword-face
    `((t :foreground ,qml-highlight-violet))
    "Face for \"property\" keyword of peroperty definition.")
  (defvar qml-property-def-keyword-face 'qml-property-def-keyword-face)
  (setq qml-property-def-keyword-face font-lock-keyword-face)

  (defface qml-basic-type-face
    `((t :foreground ,qml-highlight-green))
    "Face for qml baic type.")
  (defvar qml-basic-type-face 'qml-basic-type-face)
  (setq qml-basic-type-face font-lock-type-face)

  (defface qml-function-keyword-face
    `((t :foreground ,qml-highlight-royal-blue))
    "Face for qml function and signal keyword.")
  (defvar qml-function-keyword-face 'qml-function-keyword-face)
  )
  (setq qml-function-keyword-face font-lock-keyword-face)

(defvar qml-syntax-table
  (let ((qml-st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" qml-st)
    (modify-syntax-entry ?* ". 23" qml-st)
    (modify-syntax-entry ?\n "> b" qml-st)
    (modify-syntax-entry ?' "\"" qml-st)
    qml-st)
  "Syntax table for qml-mode.")

(defvar qml-font-lock-keywords
  (let* ((separator "\\|")
         (qml-directive-kwd
          (mapconcat 'identity '("import" "using") separator))
         (qml-declaration "[A-Z][a-zA-Z0-9_]*")
         (qml-package "[a-zA-Z0-9_]*")
         (qml-property "[a-z][a-zA-Z0-9_]*")
         (qml-basic-type-kwd
          (mapconcat 'identity
                     (list "int" "bool" "real" "double" "string" "url" "var"
                           "alias" "list" "enumeration"
                           "color" "font" "matrix4x4" "quaternion" "vector2d"
                           "vector3d" "vector4d" "date" "point" "size" "rect"
                           qml-declaration (concat qml-declaration "\\(?:." qml-declaration "\\)?"))
                     separator))
         )
    (list
     ;; preprocessor
     (list (concat "^[ \t]*\\(" qml-directive-kwd "\\)[ \t]+"
                   "\\(?:\\(?:" "\\(\\(?:\\(?:" qml-package "\\)\\.?\\)*\\(?:" qml-package "\\)\\)" ;; packages
                   "[ \t]+"
                   "\\(\\(?:\\(?:[0-9]+\\)\\.?\\)*\\(?:[0-9]+\\)\\)" ;; version
                   "\\|"
                   "\\(?:\"[^ \t].*\"\\)" ;; directory
                   "\\(?:[ \t]+as[ \t]+\\([a-zA-Z0-9_]+\\)\\)?"
                   "\\)"
                   "[ \t]*\\(?:as[ \t]+\\(" qml-declaration "\\)\\)?"
                   "\\)"
                   "[ \t]*;?$")
           '(1 qml-preprocessor-face nil t)
           '(2 qml-package-face nil t)
           '(3 qml-package-version-face nil t)
           '(4 qml-package-face nil t) ;; directory qualifier
           '(5 qml-package-face nil t) ;; alias
           )

     ;; declarations
     (list (concat "\\(?:^\\|\\*/\\|:[ \t]*\\)"
                   "[ \t]*"
                   "\\(" qml-declaration "\\.\\)?\\(" qml-declaration "\\)[ \t]*\\({\\|$\\)")
           '(1 qml-declaration-face nil t)
           '(2 qml-declaration-face nil t))

     ;; properties
     (list (concat "\\(?:^[ \t]*\\|;[ \t]*\\|{[ \t]*\\)"
                   "\\(?:\\([a-zA-Z][a-zA-Z0-9]*\\)\\.\\)?\\(" qml-property "\\)"
                   "[ \t]*[:{]")
           '(1 font-lock-variable-name-face nil t)
           '(2 font-lock-variable-name-face nil t))

     ;; property definition
     (list (concat "\\(?:^[ \t]*\\|;[ \t]*\\|{[ \t]*\\)"
                   "\\(?:\\(readonly\\|default\\)[ \t]+\\)?\\(property\\)[ \t]+" ;; keyword
                   "\\(?:\\(" qml-basic-type-kwd "\\)" ;; type
                   "\\|"
                   "\\(?:\\(" qml-declaration "\\)\\.\\(" qml-declaration "\\)\\)" ;; with qualifier
                   "\\|"
                   "\\(list\\)<\\(?:" ;; list<type>
                   "\\(" qml-basic-type-kwd "\\)"
                   "\\|"
                   "\\(?:\\(" qml-declaration "\\)\\.\\(" qml-declaration "\\)\\)"
                   "\\)>\\)[ \t]+" ;; end - list<type>
                   "\\([a-zA-Z0-9_]+\\)[ \t]*\\(?::[ \t]*[^ \t\n\\.=;:&|$#^/*+-].*\\)?$" ;; property name
                   )
           '(1 qml-property-def-keyword-face nil t) ;; keyword
           '(2 qml-property-def-keyword-face nil t) ;; keyword
           '(3 qml-basic-type-face nil t) ;; type
           '(4 qml-basic-type-face nil t) ;; with qualifier
           '(5 qml-basic-type-face nil t) ;; with qualifier
           '(6 qml-basic-type-face nil t) ;; list
           '(7 qml-basic-type-face nil t) ;; list
           '(8 qml-basic-type-face nil t) ;; list
           '(9 qml-basic-type-face nil t) ;; list
           '(10 font-lock-variable-name-face nil t) ;; property name
           )

     ;; function definition
     (list (concat "\\(^[ \t]*\\|:[ \t]*\\)"
                   "\\(function\\)"
                   "\\([ \t]+\\([a-zA-Z0-9_]+\\)\\)?[ \t]*?("
                   )
           '(2 qml-function-keyword-face nil t) ;; keyword
           ;;'(3 qml-basic-type-face nil t) ;; function name
           )

     ;; signal definition
     (list (concat "\\(^[ \t]*\\|:[ \t]*\\)"
                   "\\(signal\\)[ \t]+"
                   "\\([a-zA-Z0-9_]+\\)"
                   )
           '(2 qml-function-keyword-face nil t) ;; keyword
           ;;'(3 qml-basic-type-face nil t) ;; signal name
           )
     )))

(defun qml-line-commentp (line)
  (save-excursion
    (goto-line line)
    (string-match "^[ \t]*//" (buffer-substring (point) (point-at-eol)))))

(defun qml-beginning-of-block-internal (prev cur)
  "This function used in `qml-beginning-of-block' function.
If want the position of beginning of block, use it."
  (if (not prev) nil
    (let* ((prev1
            (save-excursion
              (goto-char prev)
              (if (search-backward-regexp "\\({\\|\\[\\)[ \t]*$" nil t)
                  (match-beginning 1)
                nil)))
           (end-of-prev1 (if prev1
                             (condition-case nil
                                 (save-excursion (goto-char prev1)
                                                 (forward-sexp)
                                                 (point))
                               (error nil))
                           nil)))
      (if (not prev1) nil
        (if (or (not end-of-prev1)
                (and (not (qml-line-commentp (line-number-at-pos prev1)))
                     (> cur prev1)
                     (< (point-at-eol) end-of-prev1)))
            prev1
          (qml-beginning-of-block-internal prev1 cur))))))

(defun qml-beginning-of-block (&optional cur)
  "Get position of beginning of block.
Return the position of beginning parenthesis \"{\" if found it, otherwise return nil.
If CUR is nil, try find beginning of block from current position.
In the line CUR is in, an item is declared single line (like that
\"Item { id:item1 ... }\" is in a line.), return the position
of beginning of \"item1\"'s parent block."
  (let* ((cur-point (if (numberp cur) cur (point)))
         (qbob (qml-beginning-of-block-internal
                (save-excursion (goto-char cur-point) (point-at-bol))
                cur-point)))
    (if (not qbob) (message "Not found beginning of block"))
    qbob))

(defvar qml-indent-offset 4)

(defun line-emptyp (&optional n)
  (string-match "^[ \t]+$"
                (buffer-substring (point-at-bol n) (point-at-eol n))))

(defun qml-indent-line ()
  "Indent current line according to QML indentation rule."
  (interactive)
  (let* ((qbob (qml-beginning-of-block (point)))
         (pi (if (not qbob) nil
               (save-excursion
                 (goto-char qbob)
                 (current-indentation)))))
    (if (not pi) (indent-line-to 0)
      (let ((ni (+ pi qml-indent-offset))
            (ci (current-indentation)))
        (if (not (eq ni ci)) (save-excursion (indent-line-to ni)))
        (if (line-emptyp 0) (save-excursion
                              (beginning-of-line 0)
                              (indent-line-to 0)))
        (let ((lbp (line-beginning-position)))
          (if (< (- (point) lbp) ni) (goto-char (+ lbp ni))))))))

(defvar qml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<tab>")
      (lambda () (interactive) (indent-region (point-min) (point-max))))
    map))

(define-derived-mode qml-mode fundamental-mode "QML"
  "Mejor mode for Qt declarative UI (simple mode)"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table qml-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(qml-font-lock-keywords))
  (set (make-local-variable 'tab-width) qml-indent-offset)
  (set (make-local-variable 'indent-line-function) 'qml-indent-line)
  (set (make-local-variable 'comment-start) "//")
  (use-local-map qml-mode-map)
  (setq major-mode 'qml-mode)
  (setq mode-name "QML")
  (run-hooks 'qml-mode-hook)
  )

(dolist (associattions (list (cons "\\.qml\\'"       'qml-mode)
                             (cons "\\.qmltypes\\'"  'qml-mode)))
  (add-to-list 'auto-mode-alist associattions))

(provide 'qml-mode)
