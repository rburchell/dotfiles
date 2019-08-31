(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; This is needed to work around an emacs bug with package installation.
;; See: https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
;; Or: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;; This should be fixed in later emacs versions.
(setq gnutls-log-level 1)

(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; enable evil
;; evil-collection/issues/60 says so
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode t)
(require 'evil-collection)
(evil-collection-init)

;; TODO: do we need to fallback to ag if there is no project?
(require 'ag)
;; Can't use global-set-key because evil hooks this by default...
(evil-global-set-key 'normal (kbd "C-f") 'ag-project)

;; enable ivy
(ivy-mode 1)
(setq ivy-wrap t)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
;; Use for both C-x b and C-x C-b.
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)


(load "~/.emacs.d/snippet.el")
