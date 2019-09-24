
;; mb.el, v3
;; This stuff is a project manager of sorts.
;; It finds settings (like compile commands) for whatever you are working on.
;; Or if it doesn't, it can be taught about it; by modifying mb/identify-project.

;; Change history:
;; * Version 1: project based, but badly
;; * Version 2: compile-command-based, no projects
;; * Version 3: project based, but now I know a little more lisp than last time
;;
;; TODO:
;; * Rework project discovery so that projects can live outside this file.
;;   Consider a mb/project-discovery-function-list var?
;; * Rather than using 'source' in compiles, use the compilation-environment variable?
;; * Allow a few different variants of project killing: always kill (current), ask-to-kill,
;;   and perhaps never-kill?
;; * Relatedly, somehow, we should probably allow multiple instances of a process...
;; * Figure out a better way to integrate with tooling (gdb, valgrind) --
;;   though emacs may have some stuff for this to look into too.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings and stuff                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq compilation-scroll-output t) ;; or 'first-error to find the first error?

;; Ensure that compilation-error-regexp-alist is set up.
(require 'compile)

;; Match things like: "[W] .... file:///path/to/qml:line:column: Warning here"
;; Also look into grep-regexp-alist to match debug/etc output similarly.
;; TODO: Need to also match: [W] default: unknown:0 - Warning message here
;; (which is a C++ error).
(setq compilation-error-regexp-alist-alist
      (cons '(mb/qt-custom-w00t
	      "^\\[W\\].* file://\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):"
                         1 ;; file
                         2 ;; line
                         3 ;; column
                         )
            compilation-error-regexp-alist-alist))
(setq compilation-error-regexp-alist
      (cons 'mb/qt-custom-w00t compilation-error-regexp-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mb/for-each-directory-part (directory callback)
  "Given a DIRECTORY string and a CALLBACK function accepting two strings,
the first being an individual part of a pathname, and the last being the full
path traversed so far, invoke CALLBACK for each part (separated by /) of
a filesystem path."
  (let ((mb/built-path "") (mb/done-parts))
    (dolist (mb/dirpart (split-string directory "/"))
      (if (not mb/done-parts)
          ;; Skip the initial empty part, to avoid //s in paths
          (if (not (string= mb/dirpart ""))
              (progn
                (setq mb/built-path (format "%s/%s" mb/built-path mb/dirpart))
                (if (funcall callback mb/dirpart mb/built-path)
                    (setq mb/done-parts t))))))))

;; small testing bullshit
;; (mb/for-each-directory-part "/usr/local/foo/bar/moo/cow"
;;                             (lambda (last-part full-path)
;;                               (message (format "Got part %s and full path %s" last-part full-path))
;;                               nil))

(defun mb/project-name (projdata)
  "Get the project name (e.g. MySuperCoolThing) for the given PROJDATA."
  (cdr (assoc 'project projdata)))
(defun mb/project-sub-project-name (projdata)
  "Get the subproject name (e.g. MySuperCoolBinary) for the given PROJDATA."
  (cdr (assoc 'sub-project projdata)))
(defun mb/project-compile-command (projdata)
  "Get the compile command for the subproject identified by PROJDATA."
  (cdr (assoc 'compile-command projdata)))
(defun mb/project-requires-interactive-compile-buffer (projdata)
  "Get whether an interactive compile buffer is required for the subproject identified by PROJDATA."
  (cdr (assoc 'interactive-compile-buffer-required projdata)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finding project settings                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mb/identify-serenity-project (file-name)
  "Identify compile command for Serenity (if in serenity), or nil."
  (let ((mb/projdata ()))
    (setq mb/projdata (cl-acons 'project "Serenity" mb/projdata))
    (setq mb/projdata (cl-acons 'sub-project "" mb/projdata))
    ;; Serenity uses sudo to build, so we need to set up an interactive terminal.
    (setq mb/projdata (cl-acons 'interactive-compile-buffer-required t mb/projdata))
    (mb/for-each-directory-part file-name
                                (lambda (mb/dirpart mb/built-path)
                                  (if (string= mb/dirpart "serenity")
                                      (setq mb/projdata (cl-acons 'compile-command
                                                               (format "cd %s/Toolchain && source UseIt.sh && cd ../Kernel && ./makeall.sh && ./run" mb/built-path) mb/projdata)))))
    (if (mb/project-compile-command mb/projdata)
        (throw 'mb-done mb/projdata))))

(defun mb/identify-go-project (file-name)
  "Identify compile command for Go source, or nil."
  (if (string-suffix-p ".go" file-name)
      (let ((mb/projdata ()))
        (setq mb/projdata (cl-acons 'project "Generic Go" mb/projdata))
        (setq mb/projdata (cl-acons 'sub-project "" mb/projdata))
        (setq mb/projdata (cl-acons 'compile-command "go build && go test -v ./..." mb/projdata))
        (throw 'mb-done mb/projdata))))

(defun mb/identify-tinyscheme-project (file-name)
  "Identify compile command for tinyscheme source, or nil."
  (let ((mb/projdata ()))
    (setq mb/projdata (cl-acons 'project "tinyscheme" mb/projdata))
    (setq mb/projdata (cl-acons 'sub-project "" mb/projdata))
    ;; Needs to be interactive. It's a command line binary, after all.
    (setq mb/projdata (cl-acons 'interactive-compile-buffer-required t mb/projdata))
    (mb/for-each-directory-part file-name
                                (lambda (mb/dirpart mb/built-path)
                                  (if (string= mb/dirpart "tinyscheme")
                                      (setq mb/projdata (cl-acons 'compile-command
                                                               "make && ./scheme" mb/projdata)))))
    (if (mb/project-compile-command mb/projdata)
        (throw 'mb-done mb/projdata))))

(defun mb/identify-generic-makefile-project (file-name)
  "Identify project for a Makefile, or nil."
  (let ((mb/projdata ()))
    (setq mb/projdata (cl-acons 'project "Makefile" mb/projdata)) ;; TODO: use file-name directory?
    (setq mb/projdata (cl-acons 'sub-project "" mb/projdata))
    (mb/for-each-directory-part file-name
                                (lambda (mb/dirpart mb/built-path)
                                  (if (file-exists-p (format "%s/Makefile" mb/built-path))
                                      (setq mb/projdata (cl-acons 'compile-command
                                                               "make" mb/projdata)))))
    (if (mb/project-compile-command mb/projdata)
        (throw 'mb-done mb/projdata))))

(defun mb/get-xconnect-compile-command (project-path binary-path run-path arguments)
  "Get compile command for an X-Connect binary.  This is just a simple helper."
  (message (format "get-xconnect-compile-command %s" project-path))
  (let*
      ((mb/build-root (concat project-path "/gui_build"))
       (mb/cd-to-build-dir (format "cd %s" mb/build-root)))

    (progn
      (setq binary-path (concat mb/build-root "/" binary-path))
      (setq run-path (concat mb/build-root "/" run-path))

      (let* (
             (mb/cd-to-run-dir (format "cd %s" run-path))
             (mb/run-binary-with-args (format "%s %s" binary-path (mapconcat 'identity arguments " ")))
             (mb/ensure-args-sourced "source ~/.ssh/hosts/adele.home.viroteck.net.sh")
             (mb/final-command)
             )
        (progn
          ;; TODO: rather than running the binary as a part of compile, look into using gdb mode?
          ;; Or I suppose we could have a separate command for that.
          ;;(setq mb/run-binary-with-args (format "valgrind --track-origins=yes %s" mb/run-binary-with-args))
          (setq mb/run-binary-with-args (format "gdb --args %s" mb/run-binary-with-args))
          (setq mb/final-command (format "%s && %s && make -k -j30 -w && %s && %s" mb/cd-to-build-dir mb/ensure-args-sourced mb/cd-to-run-dir mb/run-binary-with-args))
          mb/final-command)))))

(defvar mb/xconnect-uls (expand-file-name "~/code/ulstein/IasConfig/201127-UVE_315/201127_UVE315_Windea3.uls"))

(defun mb/ias-project (root)
  "Get project data for IasGui."
  (let ((mb/projdata ())
        (mb/startup-page "SignalLabView.qml")
        ;;(mb/startup-page (expand-file-name "~/battery.qml"))
        )
    (setq mb/projdata (cl-acons 'project "X-Connect" mb/projdata))
    (setq mb/projdata (cl-acons 'sub-project "IasGui" mb/projdata))
    (setq mb/projdata (cl-acons 'compile-command
                             (mb/get-xconnect-compile-command
                              root
                              "products/ias-gui/ias-gui-app/IasGui"
                              "products/ias-gui/ias-gui-app/"
                              (list
                               "--configFile"
                               mb/xconnect-uls
                               "--startupPage"
                               mb/startup-page
                               "--fullScreen"))
                             mb/projdata))
    (if (string-match-p (regexp-quote " gdb ") (mb/project-compile-command mb/projdata))
        (setq mb/projdata (cl-acons 'interactive-compile-buffer-required t mb/projdata)))
    (throw 'mb-done mb/projdata)))

(defun mb/eas-project (root)
  "Get project data for EasGui."
  (let ((mb/projdata ()))
    (setq mb/projdata (cl-acons 'project "X-Connect" mb/projdata))
    (setq mb/projdata (cl-acons 'sub-project "EasGui" mb/projdata))
    (setq mb/projdata (cl-acons 'compile-command
                             (mb/get-xconnect-compile-command
                              root
                              "products/eas-gui/eas-gui-app/EasGui"
                              "products/eas-gui/eas-gui-app/"
                              (list 
                               "--configFile"
                               mb/xconnect-uls))
                             mb/projdata))
    (if (string-match-p (regexp-quote " gdb ") (mb/project-compile-command mb/projdata))
        (setq mb/projdata (cl-acons 'interactive-compile-buffer-required t mb/projdata)))
    (throw 'mb-done mb/projdata)))

(defun mb/simulator-project (root)
  "Get project data for GuiSimulator."
  (let ((mb/projdata ()))
    (setq mb/projdata (cl-acons 'project "X-Connect" mb/projdata))
    (setq mb/projdata (cl-acons 'sub-project "GuiSimulator" mb/projdata))
    ;; (setq mb/projdata (cl-acons 'interactive-compile-buffer-required t mb/projdata))
    (setq mb/projdata (cl-acons 'compile-command
                             (mb/get-xconnect-compile-command
                              root
                              "products/gui-simulator/gui-simulator-app/GuiSimulator"
                              "products/ias-gui/ias-gui-app/"
                              (list 
                              "--enableSimulation"
                               "--configFile"
                               mb/xconnect-uls))
                             mb/projdata))
    (if (string-match-p (regexp-quote " gdb ") (mb/project-compile-command mb/projdata))
        (setq mb/projdata (cl-acons 'interactive-compile-buffer-required t mb/projdata)))
    (throw 'mb-done mb/projdata)))

(defun mb/identify-xconnect-project (file-name)
  (let ((mb/xconnect-root-path) (mb/built-path))
    (mb/for-each-directory-part file-name (lambda (mb/dirpart mb/built-path)
      (if (string= mb/dirpart "X-Connect")
          (setq mb/xconnect-root-path mb/built-path))
      (if mb/xconnect-root-path
          ;; TODO: control system, eventually...
          (progn
            (if (string= mb/dirpart "ias-gui")
                (throw 'mb-done (mb/ias-project mb/xconnect-root-path)))
            (if (string= mb/dirpart "eas-gui")
                (throw 'mb-done (mb/eas-project mb/xconnect-root-path)))
            (if (string= mb/dirpart "gui-simulator")
                (throw 'mb-done (mb/simulator-project mb/xconnect-root-path)))))))))

(defun mb/identify-project ()
  "Identify project information associated with the current file."
  (interactive)
  (let ((mb/file-name buffer-file-name))
    (message (format "Initial before anything %s" mb/file-name))
    (if (not mb/file-name)
        ;; this might be the case for e.g. dired buffers
        (setq mb/file-name (expand-file-name ".")))
    (catch 'mb-done
      (mb/identify-xconnect-project mb/file-name)
      (mb/identify-serenity-project mb/file-name)
      (mb/identify-go-project mb/file-name)
      (mb/identify-tinyscheme-project mb/file-name)
      (mb/identify-generic-makefile-project mb/file-name)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; actual compilation/notification stuff                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mb/strip-dots-prefix-repeatedly (string-with-dots-prefix)
  "Return STRING with a prefix of '../' removed from it (repeatedly, until it is not there)"
  (while (string-prefix-p "../" string-with-dots-prefix)
    (setq string-with-dots-prefix (replace-regexp-in-string "^\\.\\.\\/" "" string-with-dots-prefix)))
  string-with-dots-prefix)

(defun mb/process-error-filename (filename)
  "Process a filename for X-Connect builds."
  ;; Builds for X-Connect look like:
  ;; source-tree/
  ;; build-tree/
  ;; and we get plenty of relative filename paths during builds from build-tree
  ;; that really point into source-tree. Since the two trees mirror each other,
  ;; we can just remove the relative prefix, and make the path absolute.
  (setq processed-filename (mb/strip-dots-prefix-repeatedly filename))
  ;; if processed-filename ends up with a prefix of gui_build/, strip it and replace it with
  ;; gui/. We always want the canonical location to be the source, not the build tree.
  (while (string-prefix-p "/home/burchr/code/ulstein/X-Connect/gui_build/" processed-filename)
    (setq processed-filename (replace-regexp-in-string "^\\/home\\/burchr\\/code\\/ulstein\\/X\\-Connect\\/gui_build\\/" "" processed-filename)))

  (setq processed-filename (concat "/home/burchr/code/ulstein/X-Connect/gui/" processed-filename))

  (cond ((file-exists-p processed-filename)
         processed-filename)
        (t
         filename)))

;; XXX: this should be only used for X-Connect project, rather than forced globally.
;; XXX: it should probably also be made local to the compilation buffer/project.
(setq compilation-parse-errors-filename-function 'mb/process-error-filename)

(defun mb/notify-compilation-result(buffer msg)
  "Hook for compilation end.  Notify what happened."
  (if (string-match "^finished" msg)
      (flash-mode-line-color "#00aa00") ; success
    (flash-mode-line-color "#aa0000") ; failure
    ))
(add-to-list 'compilation-finish-functions 'mb/notify-compilation-result)

(defun mb/compilation-buffer-name-for-project (projdata)
  "Get a buffer name for compilation of a project.  This will not be shared by anything else, so multiple projects can run at once."
  (format "*compilation*<%s:%s>" (mb/project-name projdata) (mb/project-sub-project-name projdata)))

(defun mb/mb ()
  "Mega Builder.  Builds (and run) the current buffer as appropriate."
  (interactive)
  (let* ((mb/projdata (mb/identify-project))
        (mb/projbuffer-name (mb/compilation-buffer-name-for-project mb/projdata)))
    (if (not mb/projdata)
        (message "No project to build")
      (progn
        (message (format "Preparing to build project into buffer %s" mb/projbuffer-name))
        ;; Kill the old compilation process for the project.
        ;; Ignore errors, because the buffers/processes may not exist.
        ;; TODO: only do this if the project is not set to run in the background.
        (ignore-errors (delete-process mb/projbuffer-name))
        (ignore-errors (set-buffer mb/projbuffer-name) (kill-buffer-and-window))

        ;; Also kill the generic *compilation* process.
        ;; If something got run through some other mechanism than mb,
        ;; then we still want to build our thing now.
        (ignore-errors (delete-process "*compilation*"))
        (ignore-errors (set-buffer "*compilation*") (kill-buffer-and-window))

        ;; Run the compilation
        (setq compile-command (mb/project-compile-command mb/projdata))
        (if (mb/project-requires-interactive-compile-buffer mb/projdata)
            (progn
              ;; In interactive mode, call with a prefix so comint gets enabled.
              ;; This allows interaction with the buffer, so that e.g. passwords can be typed in
              ;; if sudo is required, for example.
              (setq current-prefix-arg '(4)) ; C-u
              (call-interactively 'compile))
          (compile compile-command))

        ;; Rename the compilation to our unique name
        (with-current-buffer
            (progn
              (set-buffer "*compilation*")
              (ignore-errors (rename-buffer mb/projbuffer-name))
              ))
        (message "Done megabuilding")))))

(global-set-key (kbd "<f5>") 'mb/mb)
