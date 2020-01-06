
(require 'package)
(package-initialize)

;; Automatically reload files that change on disk
(global-auto-revert-mode t)

;; Disable menu bar on console
(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))
(add-hook 'after-make-frame-functions 'contextual-menubar)
(add-hook 'after-init-hook 'contextual-menubar)

;; Move save files out of line
(setq backup-directory-alist `(("." . "~/.cache/emacs-backups")))
(setq auto-save-file-name-transforms `((".*" "~/.cache/emacs-saves/" t)))

;; Set backup to save files for a while
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Focus follows mouse
(setq mouse-autoselect-window t)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Window movement hotkeys
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;; Tabs vs spaces
(setq-default tab-width 4)

;; make return key also do indent, globally
(electric-indent-mode 1)

(progn
  ;; make indentation commands use space only (never tab character)
  (setq-default indent-tabs-mode nil)
  ;; emacs 23.1 to 26, default to t
  ;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
  )

;; For the love of god, turn the audible bell off!
(setq visible-bell nil ring-bell-function 'flash-mode-line)

(defun flash-mode-line ()
  "Called when the bell should sound. Should bring attention, but not too noisily."
  (flash-mode-line-color "#666666"))

(defun flash-mode-line-color (color)
  "Flash the active mode line a set color a few times to bring attention to something."
  (let ((old-color (face-background 'mode-line))
        (flash-sec (/ 5.0 20)))
    (set-face-background 'mode-line color)
    (run-with-timer flash-sec nil #'set-face-background 'mode-line old-color)
    (run-with-timer (* 2 flash-sec) nil #'set-face-background 'mode-line color)
    (run-with-timer (* 3 flash-sec) nil #'set-face-background 'mode-line old-color)))

;; Start an emacs server.
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html#Emacs-Server
(require 'server)
(or (server-running-p)
    (server-start))

(global-set-key (kbd "C-x =") 'delete-other-windows)

;; Display line numbers
; (global-display-line-numbers-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(c-auto-align-backslashes nil)
 '(c-basic-offset 4)
 '(c-cleanup-list
   (quote
    (brace-else-brace brace-elseif-brace brace-catch-brace defun-close-semi list-close-comma scope-operator comment-close-slash)))
 '(c-electric-pound-behavior (quote (alignleft)))
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (rust-mode ag aggressive-fill-paragraph go-mode magit org evil-magit ivy evil-collection evil)))
 '(term-char-mode-point-at-process-mark nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/filesystem.el")
(load "~/.emacs.d/mb.el")
(load "~/.emacs.d/projects.el")
(load "~/.emacs.d/tab-complete.el")
(load "~/.emacs.d/qml.el")
(load "~/.emacs.d/mode-customisation.el")
(load "~/.emacs.d/cancel-minibuffer.el")
(load "~/.emacs.d/notify.el")

;; Keep track of recently opened items.
(recentf-mode 1)
(setq recentf-max-menu-items 255)
(setq recentf-max-saved-items 255)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Automatically pair braces etc.
(electric-pair-mode 1)

;; Force window types to display over the top of the current window,
;; so they can be closed without having to switch focus.
(add-to-list 'display-buffer-alist '("*Apropos*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Help*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("magit:.*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("magit-diff:.*" display-buffer-same-window))

