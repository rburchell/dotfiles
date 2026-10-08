;;; rb-autosync.el -*- lexical-binding: t; -*-

(defun rb/autosync-notify (frame status cache-dir)
  "Report autosync STATUS from CACHE-DIR after client FRAME is ready."
  (when (frame-live-p frame)
    (let ((warning
           (cond
            ((not (memq status '(0 10)))
             (format "e: warning: dotfiles/Doom autosync failed; see %s"
                     (expand-file-name "sync.log" cache-dir)))
            ;; The timestamp also detects syncs completed in the background.
            ((let ((attrs (file-attributes (expand-file-name "state" cache-dir))))
               (and attrs
                    (time-less-p before-init-time
                                 (file-attribute-modification-time attrs))))
             "e: warning: Doom was synchronized; restart the running Emacs daemon to apply it"))))
      (when warning
        (with-selected-frame frame
          (message "%s" warning)
          ;; Paint the echo area while this client's frame is selected.
          (redisplay t))))))

(defun rb/autosync-after-frame ()
  "Schedule any autosync warning carried by this client frame."
  (let ((result (frame-parameter nil 'rb-autosync)))
    (when result
      ;; Consume it once, even if the server later reuses the frame.
      (set-frame-parameter nil 'rb-autosync nil)
      ;; Wait for file loading and server messages to finish, without blocking
      ;; attachment. Allow terminal negotiation to settle before displaying it.
      ;; `message' also preserves the warning in *Messages*.
      (run-at-time 0.1 nil #'rb/autosync-notify
                           (selected-frame) (car result) (cadr result)))))

(add-hook 'server-after-make-frame-hook #'rb/autosync-after-frame t)
