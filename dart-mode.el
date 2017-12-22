;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'json)
(require 'cl-lib)

;; define keywords
;; (setq dart-keywords-zero '("assert" "break" "case" "catch" "class" "const" "continue" "default" "do" "else" "enum" "extends" "false" "final" "finally" "for" "if" "in" "is" "new" "null" "rethrow" "return" "super" "switch" "this" "throw" "true" "try" "var" "void" "while" "with"))
;; (setq dart-keywords-one '("abstract" "as" "covariant" "deferred" "dynamic" "export" "external" "factory" "get" "implements" "import" "library" "operator" "part" "set" "static" "typedef"))
;; (setq dart-keywords-two '("async" "await" "sync" "yield"))
;; (setq dart-types '("int" "double" "num" "bool"))


;; define several category of keywords
(setq dart-keywords '("assert" "break" "case" "catch" "class" "const" "continue" "default" "do" "else" "enum" "extends" "false" "final" "finally" "for" "if" "in" "is" "new" "null" "rethrow" "return" "super" "switch" "this" "throw" "true" "try" "var" "void" "while" "with" "abstract" "as" "covariant" "deferred" "dynamic" "export" "external" "factory" "get" "implements" "import" "library" "operator" "part" "set" "static" "typedef" "async" "await" "sync" "yield") )
(setq dart-types '("double" "int" "num" "bool"))
(setq dart-constants '("false" "true" "null"))
;;(setq dart-events '("at_rot_target" "at_target" "attach"))
;;(setq dart-functions '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList"))

;; generate regex string for each category of keywords
(setq dart-keywords-regexp (regexp-opt dart-keywords 'words))
(setq dart-type-regexp (regexp-opt dart-types 'words))
(setq dart-constant-regexp (regexp-opt dart-constants 'words))
;; (setq dart-event-regexp (regexp-opt dart-events 'words))
;; (setq dart-functions-regexp (regexp-opt dart-functions 'words))

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq dart-font-lock-keywords
      `(
        (,dart-type-regexp . font-lock-type-face)
        (,dart-constant-regexp . font-lock-constant-face)
;;        (,dart-event-regexp . font-lock-builtin-face)
;;        (,dart-functions-regexp . font-lock-function-name-face)
        (,dart-keywords-regexp . font-lock-keyword-face)
        ;; note: order above matters, because once colored, that part won't change.
        ;; in general, longer words first
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dart-beginning-of-statement ()
  "Moves to the beginning of a Dart statement.

Unlike `c-beginning-of-statement', this handles maps correctly
and will move to the top level of a bracketed statement."
  (while
      (progn
        (back-to-indentation)
        (while (eq (char-after) ?})
          (forward-char)
          (forward-sexp -1)
          (back-to-indentation))
        (when (not (dart--beginning-of-statement-p)) (forward-line -1)))))

(defun dart--beginning-of-statement-p ()
  "Returns whether the point is at the beginning of a statement.

Statements are assumed to begin on their own lines. This returns
true for positions before the start of the statement, but on its line."
  (and
   (save-excursion
     (skip-syntax-forward " ")
     (not (or (bolp) (eq (char-after) ?}))))
   (save-excursion
     (skip-syntax-backward " ")
     (when (bolp)
       (cl-loop do (forward-char -1)
             while (looking-at "^ *$"))
       (skip-syntax-backward " ")
       (cl-case (char-before)
         ((?} ?\;) t)
         ((?{) (dart-in-block-p (c-guess-basic-syntax))))))))

(defconst dart--identifier-re
  "[a-zA-Z_$][a-zA-Z0-9_$]*"
  "A regular expression that matches keywords.")

(defun dart--forward-identifier ()
  "Moves the point forward through a Dart identifier."
  (when (looking-at dart--identifier-re)
    (goto-char (match-end 0))))

(defun dart--kill-buffer-and-window (buffer)
  "Kills BUFFER, and its window if it has one.

This is different than `kill-buffer' because, if the buffer has a
window, it respects the `quit-restore' window parameter. See
`quit-window' for details."
  (-if-let (window (get-buffer-window buffer))
      (quit-window t window)
    (kill-buffer buffer)))

(defun dart--get (alist &rest keys)
  "Recursively calls `cdr' and `assoc' on ALIST with KEYS.
Returns the value rather than the full alist cell."
  (--reduce-from (cdr (assoc it acc)) alist keys))

(defmacro dart--json-let (json fields &rest body)
  "Assigns variables named FIELDS to the corresponding fields in JSON.
FIELDS may be either identifiers or (ELISP-IDENTIFIER JSON-IDENTIFIER) pairs."
  (declare (indent 2))
  (let ((json-value (make-symbol "json")))
    `(let ((,json-value ,json))
       (let ,(--map (if (symbolp it)
                        `(,it (dart--get ,json-value ',it))
                      (-let [(variable key) it]
                        `(,variable (dart--get ,json-value ',key))))
                    fields)
         ,@body))))

(defun dart--property-string (text prop value)
  "Returns a copy of TEXT with PROP set to VALUE.

Converts TEXT to a string if it's not already."
  (let ((copy (substring (format "%s" text) 0)))
    (put-text-property 0 (length copy) prop value copy)
    copy))

(defun dart--face-string (text face)
  "Returns a copy of TEXT with its font face set to FACE.

Converts TEXT to a string if it's not already."
  (dart--property-string text 'face face))

(defmacro dart--fontify-excursion (face &rest body)
  "Applies FACE to the region moved over by BODY."
  (declare (indent 1))
  (-let [start (make-symbol "start")]
    `(-let [,start (point)]
       ,@body
       (put-text-property ,start (point) 'face ,face))))

(defun dart--flash-highlight (offset length)
  "Briefly highlights the text defined by OFFSET and LENGTH.
OFFSET and LENGTH are expected to come from the analysis server,
rather than Elisp."
  (-let [overlay (make-overlay (+ 1 offset) (+ 1 offset length))]
    (overlay-put overlay 'face 'highlight)
    (run-at-time "1 sec" nil (lambda () (delete-overlay overlay)))))

(defun dart--read-file (filename)
  "Returns the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defmacro dart--with-temp-file (name-variable &rest body)
  "Creates a temporary file for the duration of BODY.
Assigns the filename to NAME-VARIABLE. Doesn't change the current buffer.
Returns the value of the last form in BODY."
  (declare (indent 1))
  `(-let [,name-variable (make-temp-file "dart-mode.")]
     (unwind-protect
         (progn ,@body)
       (delete-file ,name-variable))))

(defun dart--run-process (executable &rest args)
  "Runs EXECUTABLE with ARGS synchronously.
Returns (STDOUT STDERR EXIT-CODE)."
  (dart--with-temp-file stderr-file
    (with-temp-buffer
      (-let [exit-code
             (apply #'call-process
                    executable nil (list t stderr-file) nil args)]
        (list
         (buffer-string)
         (dart--read-file stderr-file)
         exit-code)))))

(defun dart--try-process (executable &rest args)
  "Like `dart--run-process', but only returns stdout.
Any stderr is logged using dart-log. Returns nil if the exit code is non-0."
  (-let [result (apply #'dart--run-process executable args)]
    (unless (string-empty-p (nth 1 result))
      (dart-log (format "Error running %S:\n%s" (cons executable args) (nth 1 result))))
    (if (eq (nth 2 result) 0) (nth 0 result))))

(defvar dart--do-it-again-callback nil
  "A callback to call when `dart-do-it-again' is invoked.

Only set in `dart-popup-mode'.")
(make-variable-buffer-local 'dart--do-it-again-callback)


(defcustom dart-sdk-path
  ;; Use Platform.resolvedExecutable so that this logic works through symlinks
  ;; and wrapper scripts.
  (-when-let (dart (executable-find "dart"))
    (dart--with-temp-file input
      (with-temp-file input (insert "
        import 'dart:io';

        void main() {
          print(Platform.resolvedExecutable);
        }
        "))
      (-when-let (result (dart--try-process dart input))
        (file-name-directory
         (directory-file-name
          (file-name-directory (string-trim result)))))))
  "The absolute path to the root of the Dart SDK."
  :group 'dart-mode
  :type 'directory
  :package-version '(dart-mode . "1.0.0"))

(defun dart-executable-path ()
  "The absolute path to the 'dart' executable.

Returns nil if `dart-sdk-path' is nil."
  (when dart-sdk-path
    (concat dart-sdk-path
            (file-name-as-directory "bin")
            (if (memq system-type '(ms-dos windows-nt))
                "dart.exe"
              "dart"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom dart-formatter-command-override nil
  "The command for running the Dart formatter.

Don't read this variable; call `dart-formatter-command' instead."
  :type 'string
  :group 'dart-mode
  :package-version '(dart-mode . "1.0.0"))

(defcustom dart-formatter-line-length 80
  "The line length to use when running the Dart formatter."
  :type 'integer
  :group 'dart-mode
  :package-version '(dart-mode . "1.0.0"))

(defcustom dart-format-on-save nil
  "Whether to run the Dart formatter before saving."
  :type 'boolean
  :group 'dart-mode
  :package-version '(dart-mode . "1.0.0"))

(defcustom dart-formatter-show-errors 'buffer
  "Where to display Dart formatter error output.
It can either be displayed in its own buffer, in the echo area, or not at all.

Please note that Emacs outputs to the echo area when writing
files and will overwrite the formatter's echo output if used from
inside a `before-save-hook'."
  :type '(choice
          (const :tag "Own buffer" buffer)
          (const :tag "Echo area" echo)
          (const :tag "None" nil))
  :group 'dart-mode)

(defun dart-formatter-command ()
  "The command for running the Dart formatter.

This can be customized by setting `dart-formatter-command-override'."
  (or dart-formatter-command-override
      (when dart-sdk-path
        (file-name-as-directory "bin")
            (if (memq system-type '(ms-dos windows-nt))
                "dartfmt.exe"
              "dartfmt"))))

(defvar dart--formatter-compilation-regexp
  '("^line \\([0-9]+\\), column \\([0-9]+\\) of \\([^ \n]+\\):" 3 1 2)
  "Regular expresion to match errors in the formatter's output.
See `compilation-error-regexp-alist' for help on their format.")

(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'dart-formatter dart--formatter-compilation-regexp))
(add-to-list 'compilation-error-regexp-alist 'dart-formatter)

(defun* dart-format ()
  "Format the current buffer using the Dart formatter.

By default, this uses the formatter in `dart-sdk-path'. However,
this can be overridden by customizing
`dart-formatter-command-override'."
  (interactive)
  (let* ((file (make-temp-file "format" nil ".dart"))
         (patch-buffer (get-buffer-create "*Dart formatter patch*"))
         (error-buffer (when dart-formatter-show-errors
                         (get-buffer-create "*Dart formatter errors*")))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (args `("--line-length" ,(number-to-string dart-formatter-line-length)
                 "--overwrite" ,file)))
    (unwind-protect
        (save-restriction
          (widen)

          (when error-buffer
            (with-current-buffer error-buffer
              (setq buffer-read-only nil)
              (erase-buffer)))

          (write-region nil nil file nil 'no-message)
          (dart-info (format "%s %s" (dart-formatter-command) args))

          (unless (zerop (apply #'call-process (dart-formatter-command) nil error-buffer nil args))
            (message "Formatting failed")
            (when error-buffer
              (dart--formatter-show-errors error-buffer file (buffer-file-name)))
            (return-from dart-format))

          ;; Apply the format as a diff so that only portions of the buffer that
          ;; actually change are marked as modified.
          (if (zerop (call-process-region (point-min) (point-max)
                                          "diff" nil patch-buffer nil "--rcs" "-" file))
              (message "Buffer is already formatted")
            (dart--apply-rcs-patch patch-buffer)
            (message "Formatted buffer"))
          (when error-buffer (dart--kill-buffer-and-window error-buffer)))
      (kill-buffer patch-buffer)
      (delete-file file))))

(defun dart--apply-rcs-patch (patch-buffer)
  "Apply an RCS diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; The relative offset between line numbers in the buffer and in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so we have to
        ;; keep an offset when making changes to the buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it negative),
        ;; deleting lines increments it. This order simplifies the forward-line
        ;; invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid RCS patch or internal error in dart--apply-rcs-patch"))

          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (-let [start (point)]
                (forward-line len)
                (-let [text (buffer-substring start (point))]
                  (with-current-buffer target-buffer
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))

             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (incf line-offset len)
                (let (kill-ring) (kill-whole-line len))))

             (t
              (error "Invalid RCS patch or internal error in dart--apply-rcs-patch")))))))))

(defun dart--formatter-show-errors (error-buffer temp-file real-file)
  "Display formatter errors in `error-buffer'.
This replaces references to TEMP-FILE with REAL-FILE."
  (with-current-buffer error-buffer
    (-let [echo (eq dart-formatter-show-errors 'echo)]
      (goto-char (point-min))
      (-let [regexp (concat "\\(" (regexp-quote temp-file) "\\):")]
        (while (search-forward-regexp regexp nil t)
          (replace-match (file-name-nondirectory real-file) t t nil 1)))

      (if echo
          (progn
            (message "%s" (buffer-string))
            (dart--kill-buffer-and-window error-buffer))
        (compilation-mode)
        (temp-buffer-window-show error-buffer)
        (select-window (get-buffer-window error-buffer))))))


;;;;;;;;;;;;;;;;;;;;; Analysis Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar dart-debug nil
  "If non-nil, enables writing debug messages for dart-mode.")


(defcustom dart-sdk-path
  ;; Use Platform.resolvedExecutable so that this logic works through symlinks
  ;; and wrapper scripts.
  (-when-let (dart (executable-find "dart"))
    (dart--with-temp-file input
      (with-temp-file input (insert "
        import 'dart:io';

        void main() {
          print(Platform.resolvedExecutable);
        }
        "))
      (-when-let (result (dart--try-process dart input))
        (file-name-directory
         (directory-file-name
          (file-name-directory (string-trim result)))))))
  "The absolute path to the root of the Dart SDK."
  :group 'dart-mode
  :type 'directory
  :package-version '(dart-mode . "1.0.0"))

(defcustom dart-enable-analysis-server nil
  "If non-nil, enables support for Dart analysis server.

The Dart analysis server adds support for error checking, code completion,
navigation, and more."
  :group 'dart-mode
  :type 'boolean
  :package-version '(dart-mode . "0.12"))

(defvar dart--analysis-server nil
  "The instance of the Dart analysis server we are communicating with.")

(defun dart--analysis-server-snapshot-path ()
  "The absolute path to the snapshot file that runs the Dart analysis server."
  (when dart-sdk-path
    (concat dart-sdk-path
            (file-name-as-directory "bin")
            (file-name-as-directory "snapshots")
            "analysis_server.dart.snapshot")))

(defvar dart-analysis-roots nil
  "The list of analysis roots that are known to the analysis server.

All Dart files underneath the analysis roots are analyzed by the analysis
server.")

(defvar dart--analysis-server-next-id 0
  "The ID to use for the next request to the Dart analysis server.")

(defvar dart--analysis-server-callbacks nil
  "An alist of ID to callback to be called when the analysis server responds.

Each request to the analysis server has an associated ID.  When the analysis
server sends a response to a request, it tags the response with the ID of the
request.  We look up the callback for the request in this alist and run it with
the JSON decoded server response.")

(defvar dart--analysis-server-subscriptions nil
  "An alist of event names to lists of callbacks to be called for those events.

These callbacks take the event object and an opaque subcription
object which can be passed to `dart--analysis-server-unsubscribe'.")

(defun dart-info (msg)
  "Logs MSG to the dart log if `dart-debug' is non-nil."
  (when dart-debug (dart-log msg)))

(defun dart-log (msg)
  "Logs MSG to the dart log."
  (let* ((log-buffer (get-buffer-create "*dart-debug*"))
         (iso-format-string "%Y-%m-%dT%T%z")
         (timestamp-and-log-string
          (format-time-string iso-format-string (current-time))))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert "\n\n\n")
      (insert (concat timestamp-and-log-string
                      "\n"
                      msg))
      (insert "\n"))))

(defun dart--start-analysis-server-for-current-buffer ()
  "Initialize Dart analysis server for current buffer.

This starts Dart analysis server and adds either the pub root
directory or the current file directory to the analysis roots."
  (unless dart--analysis-server (dart-start-analysis-server))
  ;; TODO(hterkelsen): Add this file to the priority files.
  (dart-add-analysis-root-for-file)
  (add-hook 'first-change-hook 'dart-add-analysis-overlay t t)
  (add-hook 'after-change-functions 'dart-change-analysis-overlay t t)
  (add-hook 'after-save-hook 'dart-remove-analysis-overlay t t)
  (add-to-list 'flycheck-checkers 'dart-analysis-server))

(defun dart-start-analysis-server ()
  "Start the Dart analysis server.

Initializes analysis server support for all `dart-mode' buffers."
  (when dart--analysis-server
    (-let [process (dart--analysis-server-process dart--analysis-server)]
      (when (process-live-p process) (kill-process process)))
    (kill-buffer (dart--analysis-server-buffer dart--analysis-server)))

  (let* ((process-connection-type nil)
         (dart-process
          (start-process "dart-analysis-server"
                         "*dart-analysis-server*"
                         (dart-executable-path)
                         (dart--analysis-server-snapshot-path)
                         "--no-error-notification")))
    (set-process-query-on-exit-flag dart-process nil)
    (setq dart--analysis-server
          (dart--analysis-server-create dart-process)))

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'dart-mode)
        (dart--start-analysis-server-for-current-buffer)
        (when (buffer-modified-p buffer) (dart-add-analysis-overlay))))))

(defun dart--analysis-server-create (process)
  "Create a Dart analysis server from PROCESS."
  (-let [instance (dart--make-analysis-server
                   :process process
                   :buffer (generate-new-buffer (process-name process)))]
    (buffer-disable-undo (dart--analysis-server-buffer instance))
    (set-process-filter
     process
     (lambda (_ string)
       (dart--analysis-server-process-filter instance string)))
    instance))

(defun dart-add-analysis-overlay ()
  "Report to the Dart analysis server that it should overlay this buffer.

The Dart analysis server allows clients to 'overlay' file contents with
a client-supplied string.  This is needed because we want Emacs to report
errors for the current contents of the buffer, not whatever is saved to disk."
  (dart--analysis-server-send
   "analysis.updateContent"
   `((files .
            ((,buffer-file-name . ((type . "add")
                                   (content . ,(buffer-string)))))))))

(defun dart-change-analysis-overlay
    (change-begin change-end change-before-length)
  "Report to analysis server that it should change the overlay for this buffer.

The region that changed ranges from CHANGE-BEGIN to CHANGE-END, and the
length of the text before the change is CHANGE-BEFORE-LENGTH. See also
`dart-add-analysis-overlay'."
  (dart--analysis-server-send
   "analysis.updateContent"
   `((files
      . ((,buffer-file-name
          . ((type . "change")
             (edits
              . (((offset . ,(- change-begin 1))
                  (length . ,change-before-length)
                  (replacement
                   . ,(buffer-substring change-begin change-end))))))))))))

(defun dart-remove-analysis-overlay ()
  "Remove the overlay for the current buffer since it has been saved.

See also `dart-add-analysis-overlay'."
  (dart--analysis-server-send
   "analysis.updateContent"
   `((files . ((,buffer-file-name . ((type . "remove"))))))))

(defun dart-add-analysis-root-for-file (&optional file)
  "Add the given FILE's root to the analysis server's analysis roots.

A file's root is the pub root if it is in a pub package, or the file's directory
otherwise.  If no FILE is given, then this will default to the variable
`buffer-file-name'."
  (let* ((file-to-add (or file buffer-file-name))
         (pub-root (locate-dominating-file file-to-add "pubspec.yaml"))
         (current-dir (file-name-directory file-to-add)))
    (if pub-root
        (dart-add-to-analysis-roots (expand-file-name pub-root))
      (dart-add-to-analysis-roots (expand-file-name current-dir)))))

(defun dart-add-to-analysis-roots (dir)
  "Add DIR to the analysis server's analysis roots.

The analysis roots are directories that contain Dart files. The analysis server
analyzes all Dart files under the analysis roots and provides information about
them when requested."
  (add-to-list 'dart-analysis-roots dir)
  (dart--send-analysis-roots))

(defun dart--send-analysis-roots ()
  "Send the current list of analysis roots to the analysis server."
  (dart--analysis-server-send
   "analysis.setAnalysisRoots"
   `(("included" . ,dart-analysis-roots)
     ("excluded" . nil))))

(defun dart--analysis-server-send (method &optional params callback)
  "Send the METHOD request to the server with optional PARAMS.

PARAMS should be JSON-encodable.  If you provide a CALLBACK, it will be called
with the JSON decoded response.  Otherwise, the output will just be checked."
  (-let [req-without-id (dart--analysis-server-make-request method params)]
    (dart--analysis-server-enqueue req-without-id callback)))

(defun dart--analysis-server-make-request (method &optional params)
  "Construct a request for the analysis server.

The constructed request will call METHOD with optional PARAMS."
  `((method . ,method) (params . ,params)))

(defun dart--analysis-server-on-error-callback (response)
  "If RESPONSE has an error, report it."
  (-when-let (resp-err (assoc-default 'error response))
    (error "Analysis server error: %s" (assoc-default 'message resp-err))))

(defun dart--analysis-server-enqueue (req-without-id callback)
  "Send REQ-WITHOUT-ID to the analysis server, call CALLBACK with the result."
  (setq dart--analysis-server-next-id (1+ dart--analysis-server-next-id))
  (-let [request
         (json-encode (cons (cons 'id (format "%s" dart--analysis-server-next-id))
                            req-without-id))]

    ;; Enqueue the request so that we can be sure all requests are processed in
    ;; order.
    (push (cons dart--analysis-server-next-id
                (or callback #'dart--analysis-server-on-error-callback))
          dart--analysis-server-callbacks)

    (cond
     ((not dart--analysis-server)
      (message "Starting Dart analysis server.")
      (dart-start-analysis-server))
     ((not (process-live-p (dart--analysis-server-process dart--analysis-server)))
      (message "Dart analysis server crashed, restarting.")
      (dart-start-analysis-server)))

    (dart-info (concat "Sent: " request))
    (process-send-string (dart--analysis-server-process dart--analysis-server)
                         (concat request "\n"))))

(defun* dart--analysis-server-process-filter (das string)
  "Handle the event or method response from the dart analysis server.

The server DAS has STRING added to the buffer associated with it.
Method responses are paired according to their pending request and
the callback for that request is given the json decoded response."
  (-let [buf (dart--analysis-server-buffer das)]
    ;; The buffer may have been killed if the server was restarted
    (unless (buffer-live-p buf)
      (return-from dart--analysis-server-process-filter))

    ;; We use a buffer here because emacs might call the filter before the
    ;; entire line has been written out. In this case we store the
    ;; unterminated line in a buffer to be read when the rest of the line is
    ;; output.
    (with-current-buffer buf
      (goto-char (point-max))
      (insert string)
      (-let [buf-lines (s-lines (buffer-string))]
        (delete-region (point-min) (point-max))
        (insert (-last-item buf-lines))

        (-let [messages
               (--filter (and it (not (string-empty-p it)))
                         (-butlast buf-lines))]
          (dolist (message messages)
            (dart-info (concat "Received: " message))
            (dart--analysis-server-handle-msg
             (-let [json-array-type 'list]
               (json-read-from-string message)))))))))

(defun dart--analysis-server-handle-msg (msg)
  "Handle the parsed MSG from the analysis server."
  (-if-let* ((raw-id (dart--get msg 'id))
             (id (string-to-number raw-id)))
      ;; This is a response to a request, so we should invoke a callback in
      ;; dart--analysis-server-callbacks.
      (-if-let (resp-closure (dart--get dart--analysis-server-callbacks id))
          (progn
            (setq dart--analysis-server-callbacks
                  (assq-delete-all id dart--analysis-server-callbacks))
            (funcall resp-closure msg))
        (-if-let (err (dart--get msg 'error))
            (dart--analysis-server-on-error-callback msg)
          (dart-info (format "No callback was associated with id %s" raw-id))))

    ;; This is a notification, so we should invoke callbacks in
    ;; dart--analysis-server-subscriptions.
    (-when-let* ((event (dart--get msg 'event))
                 (params (dart--get msg 'params))
                 (callbacks (dart--get dart--analysis-server-subscriptions event)))
      (dolist (callback callbacks)
        (-let [subscription (cons event callback)]
          (funcall callback params subscription))))))

(defun dart--analysis-server-subscribe (event callback)
  "Registers CALLBACK to be called for each EVENT of the given type.

CALLBACK should take two parameters: the event object and an
opaque subscription object that can be passed to
`dart--analysis-server-unsubscribe'. Returns the same opaque
subscription object."
  (-if-let (cell (assoc event dart--analysis-server-subscriptions))
      (nconc cell (list callback))
    (push (cons event (list callback)) dart--analysis-server-subscriptions))
  (cons event callback))

(defun dart--analysis-server-unsubscribe (subscription)
  "Unregisters the analysis server SUBSCRIPTION.

SUBSCRIPTION is an opaque object provided by
`dart--analysis-server-subscribe'."
  (-let [(event . callback) subscription]
    (delq callback (assoc event dart--analysis-server-subscriptions))))






;;;###autoload
(define-derived-mode idart-mode prog-mode "iDart"
  "Major mode for editing Dart"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((dart-font-lock-keywords))))

;; clear memory. no longer needed
(setq dart-keywords nil)
(setq dart-types nil)
(setq dart-constants nil)
;;(setq dart-events nil)
;;(setq dart-functions nil)

;; clear memory. no longer needed
(setq dart-keywords-regexp nil)
(setq dart-types-regexp nil)
(setq dart-constants-regexp nil)
;; (setq dart-events-regexp nil)
;; (setq dart-functions-regexp nil)

;; (defvar dart-analysis-request-id 1
;;   "ID used to for requests to the analysis server. Simply increments for each call.")
;; (defconst dart-buffer-name "*dart-analysis*"
;;   "Name of the dart process buffer.")
;; (defvar dart-analysis-server-callbacks (make-hash-table :test 'equal)
;;   "Hash Table that temporarily holds callbacks.")

;; (setq dart-executable-path (executable-find "dart"))
;; (setq dart-analysis-server-path "/opt/dart-sdk/bin/snapshots/analysis_server.dart.snapshot")

;; start processes using pipe
;; (let ((process-connection-type nil))  ; Use a pipe.
;;   (start-process ...))

;; (defun dart-start-analyzer-process ()
;;   "Some docs."
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "dart-process" dart-buffer-name dart-executable-path dart-analysis-server-path)))
;;       (setq dart-process proc))))

;; (defun dart-call-call-back (id)
;;   "Call the callback at given ID and then delete it from the hash-table."
;;   '(gethash dart-analysis-server-callbacks id)
;;   (remhash dart-analysis-server-callbacks id))

;; (defun dart-set-call-back (callback)
;;   "Save the given CALLBACK."
;;   (puthash dart-analysis-request-id callback dart-analysis-server-callbacks)
;;   (setq dart-analysis-request-id (+ dart-analysis-request-id 1)))

;; (defvar dart-process nil
;;   "Handle to the process running the dart analysis server.")

;; (defun dart-terminate-process ()
;;   "Terminate the dart analysis server process."
;;   (delete-process dart-buffer-name)
;;   (kill-buffer dart-buffer-name))


(provide 'idart-mode)
;;; dart-mode.el ends here
