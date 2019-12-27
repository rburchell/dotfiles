
;; Don't display the startup screen
(setq inhibit-startup-screen t)

(defun project-list ()
  "Open buffers for persistent things we want around always."
  (interactive)
  (find-file-noselect "~/code/bluectrl/X-Connect/gui/products/ias-gui")
  (find-file-noselect "~/code/bluectrl/X-Connect/gui/products/eas-gui")
  (find-file-noselect "~/code/serenity")
  (find-file-noselect "~/code/workspace")
  (find-file-noselect "~/code/crimson")
  (find-file-noselect "~/code/go/src/github.com/rburchell")
  (find-file-noselect "~/.emacs.d"))

;; Hook that's run on startup.
(defun project-list-startup-hook ()
  "Kill the scratch buffer (if it exists), and make something more useful."
  (when (and (string= "*scratch*" (buffer-name)) (not (buffer-file-name)))
    (kill-buffer "*scratch*")
    (project-list)))
(add-hook 'emacs-startup-hook 'project-list-startup-hook)
