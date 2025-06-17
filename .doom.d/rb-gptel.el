;;; rb-gptel.el -*- lexical-binding: t; -*-

(defun rb/describe-symbol (name)
  "Return the source code for function or variable NAME as a string.
If source isn't found, falls back to the Emacs Lisp object sexp."
  (let* ((sym (if (symbolp name) name (intern name)))
         (callable (or (functionp sym) (macrop sym)))
         (find-fn (if callable #'find-function-noselect #'find-variable-noselect)))
    (condition-case nil
        (let* ((res (funcall find-fn sym))
               (buf (car res))
               (pos (cdr res)))
          (with-current-buffer buf
            (save-excursion
              (goto-char pos)
              (buffer-substring-no-properties
               (point)
               (progn (end-of-defun) (point))))))
      (error
       (let ((obj (if callable
                      (symbol-function sym)
                    (symbol-value sym))))
         (pp-to-string obj))))))

(defun rb/sqlite-get-schema (dbfile)
  "Return the full SQL schema of the SQLite DBFILE as a string."
  (interactive "fSQLite DB file: ")
  (let ((db (sqlite-open dbfile)))
    (unwind-protect
        (string-join
         (mapcar #'car
                 (sqlite-select db "SELECT sql FROM sqlite_master WHERE sql NOT NULL;"))
         "\n\n")
      (when db (sqlite-close db)))))

(defun rb/sqlite-exec (dbfile query)
  "Run arbitrary SQL QUERY on SQLite database DBFILE and return results as a tab-separated string."
  (let* ((db (sqlite-open dbfile))
         (results (unwind-protect
                      (sqlite-select db query)
                    (when db (sqlite-close db)))))
    (mapconcat (lambda (row)
                 (mapconcat (lambda (cell) (format "%s" cell)) row "\t"))
               results "\n")))

(setq gptel-model 'gpt-4o
      gptel-backend (gptel-make-gh-copilot "Copilot"))
(setq gptel-model 'gpt-4.1
      gptel-backend (gptel-make-gh-copilot "Copilot"))

(setq gptel-available t
      gptel-track-media t
      gptel-org-branching-context t
      gptel-default-mode 'org-mode)
(setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
(setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

(defvar gptel-tool-library-debug-buffer "*gptel-tool-debug*"
  "Buffer for debug output, if debug logging is enabled via `gptel-tool-library-debug'")

(defun gptel-tool-library--debug-log (log)
  "Print LOG to `gptel-tool-library-debug-buffer' if debug logging is enabled."
  (let ((buffer (get-buffer-create gptel-tool-library-debug-buffer)))
    (with-current-buffer gptel-tool-library-debug-buffer
      (goto-char (point-max)) ; Ensure logs are appended at the end
      (insert (format "%s\n" log)))))

(setq gptel-tools
      (list
       (gptel-make-tool
        :name "list_buffers"
        :function (lambda ()
                    (gptel-tool-library--debug-log "list-buffers")
                    (mapcar #'buffer-name (buffer-list)))
        :description "Return the names of all currently open Emacs buffers."
        :args nil
        :category "emacs")

       (gptel-make-tool
        :name "read_buffer"                    ; javascript-style snake_case name
        :function (lambda (buffer)                  ; the function that will run
                    (gptel-tool-library--debug-log (format "read-buffer %s" buffer))
                    (unless (buffer-live-p (get-buffer buffer))
                      (error "error: buffer %s is not live." buffer))
                    (with-current-buffer  buffer
                      (buffer-substring-no-properties (point-min) (point-max))))
        :description "return the contents of an emacs buffer"
        :args (list '(:name "buffer"
                      :type string            ; :type value must be a symbol
                      :description "the name of the buffer whose contents are to be retrieved"))
        :category "emacs")

       ;; (gptel-make-tool
       ;;  :function (lambda (query limit)
       ;;              (gptel-tool-library--debug-log (format "kagi_search %s %s" query limit))
       ;;              (rb/kagi-search-query query limit))
       ;;  :name "kagi_search"
       ;;  :description "Perform a web search using the Kagi Search API"
       ;;  :args (list '(:name "query"
       ;;                :type "string"
       ;;                :description "The search query string")
       ;;              '(:name "limit"
       ;;                :type "integer"
       ;;                :description "The maximum number of results to retrieve (optional)"))
       ;;  :category "web")

       (gptel-make-tool
        :function (lambda (directory)
                    (gptel-tool-library--debug-log (format "list_directory_contents: %s" directory))
                    (if (file-directory-p directory)
                        (let ((contents (directory-files directory t nil t))
                              (file-modes-string
                               (lambda (modes)
                                 (let ((perm-chars '(("-" . 0) ("r" . 256) ("w" . 128) ("x" . 64)
                                                     ("r" . 32)  ("w" . 16)  ("x" . 8)
                                                     ("r" . 4)   ("w" . 2)   ("x" . 1))))
                                   (concat
                                    (if (>= modes #o40000) "d" "-")
                                    (mapconcat (lambda (char)
                                                 (if (/= 0 (logand (cdr char) modes))
                                                     (car char) "-"))
                                               perm-chars ""))))))
                          (if contents
                              (string-join
                               (mapcar (lambda (file)
                                         (let* ((attributes (file-attributes file))
                                                (size (file-attribute-size attributes))
                                                (is-dir (file-attribute-type attributes))
                                                (owner (or (file-attribute-user-id attributes) "unknown"))
                                                (permissions (funcall file-modes-string (file-modes file))))
                                           (format "%s\t%s\t%s\t%s\t%s"
                                                   (if is-dir "DIR " "FILE")
                                                   permissions
                                                   owner
                                                   size
                                                   (file-name-nondirectory file))))
                                       contents)
                               "\n")
                            "The directory is empty."))
                      (error "Invalid directory: %s" directory)))
        :name "list_directory"
        :category "fs"
        :description "List the contents of a directory with metadata (type, permissions, owner, size)"
        :args (list '(:name "directory"
                      :type "string"
                      :description "The path of the directory to list")))

       (gptel-make-tool
        :function (lambda (file &optional offset size)
                    (gptel-tool-library--debug-log (format "read_file: %s %s %s" file offset size))
                    (if (not (file-exists-p file))
                        (error "File does not exist: %s" file)
                      (with-temp-buffer
                        (insert-file-contents file nil (or offset 0) size)
                        (buffer-string))))
        :name "read_file"
        :category "fs"
        :description "Read the contents of a file, optionally specifying the start offset and size to read."
        :args (list '(:name "file"
                      :type "string"
                      :description "The path of the file to read")
                    '(:name "offset"
                      :type "integer"
                      :description "The byte offset to start reading from (optional)")
                    '(:name "size"
                      :type "integer"
                      :description "The number of bytes to read (optional)")))

       (gptel-make-tool
        :name "delete_file"
        :category "fs"
        :description "Delete a file from the filesystem. Requires confirmation."
        :args (list '(:name "path"
                      :type "string"
                      :description "The full path of the file to delete."))
        :confirm t
        :function (lambda (path)
                    (gptel-tool-library--debug-log (format "delete_file: %S" path))
                    (if (not (file-exists-p path))
                        (format "File does not exist: %s" path)
                      (delete-file path)
                      (format "File %s has been deleted." path))))


       (gptel-make-tool
        :name "delete_recursively"
        :category "fs"
        :description "Recursively delete a file or directory (with all its contents). Requires confirmation."
        :args (list '(:name "path"
                      :type "string"
                      :description "The full path of the file or directory to delete."))
        :confirm t ; Mark the tool as sensitive
        :function (lambda (path)
                    (gptel-tool-library--debug-log (format "delete_recursively: %s" path))
                    (if (not (file-exists-p path))
                        (error "Path does not exist: %s" path)
                      (if (file-directory-p path)
                          (progn
                            (delete-directory path t) ; 't' ensures recursive deletion
                            (format "Directory %s has been deleted." path))
                        (progn
                          (delete-file path)
                          (format "File %s has been deleted." path))))))


       (gptel-make-tool
        :name "copy_file"
        :category "fs"
        :description "Copy a file from SRC to DEST. Only supports a single file, not directories."
        :args (list '(:name "src"
                      :type "string"
                      :description "The full path of the source file to copy.")
                    '(:name "dest"
                      :type "string"
                      :description "The full path where the file will be copied to."))
        :confirm t
        :function
        (lambda (src dest)
          (gptel-tool-library--debug-log (format "copy_file: %s -> %s" src dest))
          (cond
           ((not (file-exists-p src))
            (error "Source file does not exist: %s" src))
           ((file-directory-p src)
            (error "Source is a directory: %s (not supported)" src))
           (t
            (copy-file src dest 1)
            (format "Copied %s to %s." src dest)))))

       (gptel-make-tool
        :name "copy_recursive"
        :category "fs"
        :description "Recursively copy a file or directory from SRC to DEST (like cp -r). Will overwrite DEST if it exists."
        :args (list '(:name "src"
                      :type "string"
                      :description "The full path of the source file or directory to copy.")
                    '(:name "dest"
                      :type "string"
                      :description "The destination path (file name or directory location)."))
        :confirm t
        :function
        (lambda (src dest)
          (gptel-tool-library--debug-log (format "copy_recursive: %s -> %s" src dest))
          (unless (file-exists-p src)
            (error "Source does not exist: %s" src))
          (copy-directory src dest t t t) ;; copy-directory: preserve-time, copy-contents, keep-time
          (format "Recursively copied %s to %s." src dest)))

       (gptel-make-tool
        :name "rename_file"
        :category "fs"
        :description "Rename a file from OLD-PATH to NEW-PATH. Requires confirmation."
        :args (list '(:name "old"
                      :type "string"
                      :description "The path to rename from.")
                    '(:name "new"
                      :type "string"
                      :description "The path to rename to."))
        :confirm t
        :function (lambda (old_path new_path)
                    (gptel-tool-library--debug-log (format "rename_file: %s -> %s" old_path new_path))
                    (if (not (file-exists-p old_path))
                        (error "File does not exist: %s" old_path)
                      (rename-file old_path new_path 1)
                      (format "File %s has been renamed to %s." old_path new_path))))

       (gptel-make-tool
        :name "transfer_file"
        :category "file"
        :description "Asynchronously transfer a file to or from a remote host using parameters similar to scp. Requires confirmation."
        :args (list '(:name "from"
                      :type "string"
                      :description "The path to the source file or remote location (e.g., user@host:/path/to/file).")
                    '(:name "to"
                      :type "string"
                      :description "The path to the destination or remote location (e.g., user@host:/path/to/destination)."))
        :confirm t ; Mark tool as sensitive
        :function (lambda (from to)
                    (gptel-tool-library--debug-log
                     (format "transfer_file: from=%s to=%s" from to))
                    ;; Build the scp process arguments
                    (let ((args (list from to)))
                      ;; Execute `scp` asynchronously in a safe manner
                      (make-process
                       :name "transfer-file-process"
                       :filter (lambda (_process output)
                                 ;; Log process output as it appears
                                 (gptel-tool-library--debug-log
                                  (format "Transfer process output: %s"
                                          (string-trim output))))
                       :sentinel (lambda (process event)
                                   ;; Log process state changes
                                   (gptel-tool-library--debug-log
                                    (format "Transfer process %s: %s"
                                            (process-name process)
                                            (string-trim event))))
                       :command (append '("scp") args))
                      (format "File transfer from %s to %s initiated." from to))))

       ;; NOTE: This might be replaceable/simplifiable with builtin elisp (man), but
       ;; this is presently broken on non-GNU systems.
       ;; Fixed by https://github.com/emacs-mirror/emacs/commit/fc5e905dc90e21b1a381bde42e22c06f45c17e16
       (gptel-make-tool
        :name "read_manpage"
        :category "system"
        :description "Read a manpage and return its contents as plain text."
        :args (list '(:name "page"
                      :type "string"
                      :description "The name of the manpage to read (e.g., 'ls' for the 'ls' command).")
                    '(:name "section"
                      :type "integer"
                      :optional t
                      :description "The section of the manpage to read (optional, e.g., 1 for user commands)."))
        :function (lambda (page section)
                    (gptel-tool-library--debug-log (format "read_manpage: %s, section: %s" page section))
                    (let* ((command (if section
                                        (list "man" (number-to-string section) page)
                                      (list "man" page)))
                           (output (with-temp-buffer
                                     (apply #'call-process (car command) nil t nil (cdr command))
                                     (buffer-string))))
                      output)))

       (gptel-make-tool
        :name "elisp_eval"
        :category "elisp"
        :description "Use eval to evaluate elisp code."
        :confirm t
        :args (list '(:name "command"
                      :type "string"
                      :description "The elisp code to evaluate"))
        :function (lambda (command)
                    (gptel-tool-library--debug-log (format "elisp_eval: %s" command))
                    (eval (if (stringp command)
                              (car (read-from-string (format "(progn %s)" command)))
                            command))))

       (gptel-make-tool
        :name "elisp_describe"
        :category "elisp"
        :description "Return the source of a function (defun) or variable (defvar) by name, as defined in the running environment."
        :args (list '(:name "symbol"
                      :type "string"
                      :description "The name of the function or variable to describe, as a string."))
        :function (lambda (symbol)
                    (gptel-tool-library--debug-log (format "elisp_describe_symbol: %s" symbol))
                    (rb/describe-symbol symbol)))

       (gptel-make-tool
        :name "elisp_function_doc"
        :category "elisp"
        :description "Retrieve the documentation for an Elisp function."
        :args (list '(:name "fname"
                      :type "string"
                      :description "The name of the function to retrieve documentation for."))
        :function (lambda (fname)
                    (gptel-tool-library--debug-log (format "elisp_function_doc: %s" fname))
                    (let ((func (intern-soft fname)))
                      (if (and func (fboundp func))
                          (documentation func)
                        (format "No documentation found for function: %s" fname)))))

       (gptel-make-tool
        :name "elisp_variable_doc"
        :category "elisp"
        :description "Retrieve the documentation for an Elisp variable."
        :args (list '(:name "vname"
                      :type "string"
                      :description "The name of the variable to retrieve documentation for."))
        :function (lambda (vname)
                    (gptel-tool-library--debug-log (format "elisp_variable_doc: %s" vname))
                    (let ((var (intern-soft vname)))
                      (if (and var (boundp var))
                          (documentation-property var 'variable-documentation)
                        (format "No documentation found for variable: %s" vname)))))

       (gptel-make-tool
        :name "elisp_fuzzy_match"
        :category "elisp"
        :description "List all functions and variables that fuzzily match the given name, along with a one-sentence summary, with an optional limit on the number of results."
        :args (list '(:name "name"
                      :type "string"
                      :description "The name to match against available functions and variables.")
                    '(:name "limit"
                      :type "integer"
                      :optional t
                      :description "The maximum number of results to return. If nil, returns all matches."))
        :function (lambda (name &optional limit)
                    (gptel-tool-library--debug-log (format "elisp_fuzzy_match: %s, limit: %s" name limit))
                    (let ((matches '()))
                      ;; Search functions
                      (mapatoms (lambda (sym)
                                  (when (and (fboundp sym) (string-match-p name (symbol-name sym)))
                                    (push (format "Function: %s - %s"
                                                  (symbol-name sym)
                                                  (or (ignore-errors
                                                        (documentation sym))
                                                      "No documentation available."))
                                          matches))))
                      ;; Search variables
                      (mapatoms (lambda (sym)
                                  (when (and (boundp sym) (string-match-p name (symbol-name sym)))
                                    (push (format "Variable: %s - %s"
                                                  (symbol-name sym)
                                                  (or (ignore-errors
                                                        (documentation-property sym 'variable-documentation))
                                                      "No documentation available."))
                                          matches))))
                      ;; Apply limit if specified
                      (when (and limit (> (length matches) limit))
                        (setq matches (seq-take matches limit)))
                      (if matches
                          (string-join matches "\n")
                        (format "No matches found for name: %s" name)))))

       (gptel-make-tool
        :name "sqlite_schema"
        :category "sqlite"
        :description "Show the full SQL schema of the SQLite PATH in a new buffer."
        :confirm t
        :args (list '(:name "path"
                      :type "string"
                      :description "The path to the SQLite database file."))
        :function (lambda (path)
                    (gptel-tool-library--debug-log (format "sqlite_schema: %s" path))
                    (rb/sqlite-get-schema path)))
       (gptel-make-tool
        :name "sqlite_exec"
        :category "sqlite"
        :description "Run an arbitrary SQL query on the given SQLite database file and return the results."
        :confirm t
        :args (list '(:name "path"
                      :type "string"
                      :description "The path to the SQLite database file.")
                    '(:name "query"
                      :type "string"
                      :description "The SQL query to run."))
        :function (lambda (path query)
                    (gptel-tool-library--debug-log (format "sqlite_query: %s: %s" path query))
                    (rb/sqlite-exec path query)))

       ))

(setq gptel--system-message "You are a large language model living in Emacs and a helpful assistant. Respond tersely and concisely and don't summarize unless specifically requested to. You have a number of specialised tools available, you should use them when they would help your answers.")
