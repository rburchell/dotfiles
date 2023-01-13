;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;; LSP and format-on-save seem to not be working well together.
;; See https://github.com/hlissner/doom-emacs/issues/5128
;; (add-hook 'c-mode-hook #'format-all-mode)               ;; enable code formatting on save
;; (setq-hook! 'c-mode-hook +format-with-lsp nil)

(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)

(setq window-divider-default-right-width 10)
(setq window-divider-default-bottom-width 1)

;; Keep window title up-to-date; should fail gracefully in non-xterm terminals.
;; Only works in Emacs 27+.
;; xterm-set-window-title doesn't seem to correctly handle non-terminal frames.
;; See https://github.com/hlissner/doom-emacs/issues/4977.
(setq xterm-set-window-title t)
(defadvice! fix-xterm-set-window-title (&optional terminal)
  :before-while #'xterm-set-window-title
  (not (display-graphic-p terminal)))

;; Stop comments being completely and utterly broken... https://github.com/hlissner/doom-emacs/issues/5759
(after! cc-mode
  (sp-local-pair '(c-mode c++-mode) "/*!" "*/" :actions :rem))

;; Also don't continue the *s when hitting enter, because stylistically, I don't like that...
(setq +default-want-RET-continue-comments nil)

;; ;; Some terminals offer two different cursors: a "visible" static cursor and a
;; ;; "very visible" blinking one. By default, Emacs uses the very visible cursor
;; ;; and will switch back to it when Emacs is started or resumed. A nil
;; ;; `visible-cursor' prevents this.
;; (setq visible-cursor nil)

;; ;; Enable the mouse in terminal Emacs
;; (add-hook 'tty-setup-hook #'xterm-mouse-mode)

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

;; (setq doom-font (font-spec :family "Comic Mono" :size 12))


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

(load! "qml-mode.el")
