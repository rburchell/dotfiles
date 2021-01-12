;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Check syntax automatically, not on save.
;; On save is obviously faster, but less nice.
(after! flycheck
  (setq flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch new-line mode-enabled)))

;; these are the defaults (before I changed them)
(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 3))

(doom-load-envvars-file "~/.doom.d/custom-env")

(after! projectile
  ;; A list of files considered to mark the root of a project. The topmost match has precedence.
  (setq projectile-project-root-files
    '(
      "Cargo.toml"         ; Cargo project file
      "go.mod"             ; golang default package root as of 1.13
      "meson.build"        ; Meson
      ))

  ;; A list of files considered to mark the root of a project. The bottommost (parentmost) match has precedence.
  (setq projectile-project-root-files-bottom-up
        '(
          ".projectile" ; projectile project marker
          ".git"        ; Git VCS root dir
          ))

  ;; A list of files considered to mark the root of a project.
  ;; The search starts at the top and descends down till a directory
  ;; that contains a match file but its parent does not.  Thus, it's a
  ;; bottommost match in the topmost sequence of directories
  ;; containing a root file.
  (setq projectile-project-root-files-top-down-recurring
        '(
          ".svn" ; Svn VCS root dir
          "CVS"  ; Csv VCS root dir
          "Makefile"
          ))

  (defun rb/projectile-qmake-project-p ()
    (projectile-verify-file-wildcard "*.pro"))

  (defcustom rb/projectile-qmake-project #'rb/projectile-qmake-project-p
    "Function to determine if project's type is qmake."
    :group 'projectile
    :type 'function
    :package-version '(projectile . "1.0.0"))

  (projectile-register-project-type 'qmake rb/projectile-qmake-project
                                    :compile "qmake-qt5"
                                    :compile "bear -a make -j4"
                                    :test "make check")

  ;; The default ordering is to prefer bottom-up before top-down, but that is very annoying.
  ;; It breaks nesting of several subprojects inside a git repository (for example).
  ;; So we override that here with our own logic.
  (setq projectile-project-root-files-functions '(projectile-root-local
                                               projectile-root-top-down
                                               projectile-root-top-down-recurring
                                               projectile-root-bottom-up))

  ;; ;; Disable caching: this gets huge really easily.
  (setq projectile-enable-caching nil))

;;;; rust
(after! rustic
  (set-formatter! 'rustic-mode #'rustic-cargo-fmt))
(setq rustic-lsp-server 'rust-analyzer
      ;; Disable for now. See https://github.com/rust-lang/rustfmt/issues/4454
      ;; Having to add a special toml just to format seems suboptimal.
      ;; rustic-format-on-save t
      lsp-rust-server 'rust-analyzer)
(after! lsp
  (setq lsp-rust-analyzer-lru-capacity 10
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-cargo-watch-enable t
        lsp-rust-analyzer-cargo-watch-command "clippy"))
      

(after! org
  ;; org settings: point things to the right place...
  (setq org-directory "~/code/workspace/.org")
  (setq org-default-notes-file "~/code/workspace/.org/refile.org"))

;; Don't autoformat everything...
;; (setq +format-on-save-enabled-modes
;;       '(go-mode rustic-mode))
;; (setq +format-on-save-enabled-modes
;;       '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
;;             sql-mode         ; sqlformat is currently broken
;;             tex-mode         ; latexindent is broken
;;             latex-mode))

(after! hl-todo
  (setq hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ("\\todo" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ("\\note" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ("\\bug" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))
