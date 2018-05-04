;;; package --- Sum
;;; dart-mode.el --- Major mode for editing Dart files -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'compile)
(require 'rx)

;;; Utility functions and macros

(defun dart--read-file (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defmacro dart--with-temp-file (name-variable &rest body)
  "Create a temporary file for the duration of BODY.
Assigns the filename to NAME-VARIABLE. Doesn't change the current buffer.
Returns the value of the last form in BODY."
  (declare (indent 1))
  `(-let [,name-variable (make-temp-file "dart-mode.")]
     (unwind-protect
         (progn ,@body)
       (delete-file ,name-variable))))

(defun dart--run-process (executable &rest args)
  "Run EXECUTABLE with ARGS synchronously.
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
  "Like `dart--run-process', but only return stdout.
Any stderr is logged using dart-log. Returns nil if the exit code is non-0."
  (-let [result (apply #'dart--run-process executable args)]
    (unless (string-empty-p (nth 1 result))
      (dart-log (format "Error running %S:\n%s" (cons executable args) (nth 1 result))))
    (if (eq (nth 2 result) 0) (nth 0 result))))

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

(defun dart--kill-buffer-and-window (buffer)
  "Kill BUFFER, and its window if it has one.

This is different than `kill-buffer' because, if the buffer has a
window, it respects the `quit-restore' window parameter. See
`quit-window' for details."
  (if-let (window (get-buffer-window buffer))
      (quit-window t window)
    (kill-buffer buffer)))

(defvar dart-debug nil
  "If non-nil, enables writing debug messages for dart-mode.")

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


;; General configuration

;; (defcustom dart-sdk-path
;;   ;; Use Platform.resolvedExecutable so that this logic works through symlinks
;;   ;; and wrapper scripts.
;; 	(f-dirname (executable-find "dart"))
;;   ;; (when-let (dart (executable-find "dart"))
;; 	;; 	(setq input "hello")
;;   ;;   (dart--with-temp-file input
;;   ;;     (with-temp-file input (insert "
;;   ;;       import 'dart:io';

;;   ;;       void main() {
;;   ;;         print(Platform.resolvedExecutable);
;;   ;;       }
;;   ;;       "))
;;   ;;     (when-let (result (dart--try-process dart input))
;;   ;;       (file-name-directory
;;   ;;        (directory-file-name
;;   ;;         (file-name-directory (string-trim result)))))
;; 	;; 		))
;;   "The absolute path to the root of the Dart SDK."
;;   :group 'dart-mode
;;   :type 'directory
;;   :package-version '(dart-mode . "1.0.0"))

;; (defun dart-executable-path ()
;;   "The absolute path to the 'dart' executable.

;; Returns nil if `dart-sdk-path' is nil."
;;   (when dart-sdk-path
;;     (concat dart-sdk-path
;;             (file-name-as-directory "bin")
;;             (if (memq system-type '(ms-dos windows-nt))
;;                 "dart.exe"
;;               "dart"))))

;;; Dart analysis server

;;;; Flycheck Error Reporting


;;;; Hover


;;;; Navigation


;;;; Auto-complete



;;; Popup Mode



;;; Formatting

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

(defcustom dart-format-on-save t
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

(defun dart-format ()
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
										(setq line-offset (- line-offset len))
;;                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))

             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
								(setq line-offset (+ line-offset len))
;;                (incf line-offset len)
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




(defconst dart-dangling-operators-regexp "[^-]-\\|[^+]\\+\\|[/*&><.=|^]")
(defconst dart--max-dangling-operator-length 2
  "The maximum length of dangling operators.
This must be at least the length of the longest string matched by
‘dart-dangling-operators-regexp.’, and must be updated whenever
that constant is changed.")

(defconst dart-identifier-regexp "_?[[:word:][:multibyte:]]+")
(defconst dart-type-name-no-prefix-regexp "\\(?:[[:word:][:multibyte:]]+\\.\\)?[[:word:][:multibyte:]]+")
(defconst dart-qualified-identifier-regexp (concat dart-identifier-regexp "\\." dart-identifier-regexp))
(defconst dart-label-regexp dart-identifier-regexp)
(defconst dart-type-regexp "[[:word:][:multibyte:]*]+")
(defconst dart-func-regexp (concat "\\_<func\\_>\\s *\\(" dart-identifier-regexp "\\)"))
(defconst dart-variable-regexp "\\(\\w+\\)\\s-+=")
(defconst dart-func-meth-regexp (concat
																 "\\_<func\\_>\\s *\\(?:(\\s *"
																 "\\(" dart-identifier-regexp "\\s +\\)?" dart-type-regexp
																 "\\s *)\\s *\\)?\\("
																 dart-identifier-regexp
																 "\\)("))

;; (defconst dart-builtins
;;   '("append" "cap"   "close"   "complex" "copy"
;;     "delete" "imag"  "len"     "make"    "new"
;;     "panic"  "print" "println" "real"    "recover")
;;   "All built-in functions in the Go language.  Used for font locking.")

(defconst dart-mode-keywords
  '(
		"assert" "break" "case" "catch" "class"
		"const" "continue" "default" "do" "else"
		"enum" "extends" "false" "final" "finally"
		"for" "if" "in" "is" "new" "null" "rethrow"
		"return" "super" "switch" "this" "throw"
		"true" "try" "var" "void" "while" "with"
		"abstract" "as" "covariant" "deferred"
		"dynamic" "export" "external" "factory"
		"get" "implements" "import" "library"
		"operator" "part" "set" "static" "typedef"
		"async" "await" "sync" "yield" "this")
  "All keywords in the Go language.  Used for font locking.")

(defconst dart-constants '("null" "true" "false"))
;(defconst dart-type-name-regexp (concat "\\(?:[*(]\\)*\\(\\(?:" go-identifier-regexp "\\.\\)?" go-identifier-regexp "\\)"))


(defun dart--build-font-lock-keywords ()
	"Do some stuff."
	(append
	 `((,"\\_<[0-9]+\\_>" . font-lock-constant-face)
		 (,(concat "\\_<" (regexp-opt dart-mode-keywords t) "\\_>") . font-lock-keyword-face)
		 (,(concat "\\(\\_<" (regexp-opt go-builtins t) "\\_>\\)[[:space:]]*(") 1 font-lock-builtin-face)
		 (,(concat "\\_<" (regexp-opt dart-constants t) "\\_>") . font-lock-constant-face)
		 (,(concat "\\(" dart-identifier-regexp "\\)[[:space:]]*(") 1 font-lock-function-name-face)
		 (,dart-identifier-regexp . font-lock-variable-name-face)
		 )))
																				; font-lock-builtin-face (no need to highlight builtins differently)
																				; font-lock-constant-face (null )
																				; font-lock-doc-face
																				; font-lock-function-name-face
																				; font-lock-keyword-face
																				; font-lock-type-face
																				; font-lock-variable-name-face



(defvar dart-mode-syntax-table
	(let ((st (make-syntax-table)))
		(modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?%  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?|  "." st)
    (modify-syntax-entry ?^  "." st)
    (modify-syntax-entry ?!  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?`  "\"" st)
		(modify-syntax-entry ?\\ "\\" st)
		(modify-syntax-entry ?_ "w" st)
		
		st)
	"Syntax table for Dart mode.")

(defun dart-syntax-is-triple-quote (start)
	"Check if START is beginning of triple quote."
	(save-excursion
		(goto-char start)
		(when (looking-at-p "\\('''\\)\\|\\(\"\"\"\\)")
			t)))

(defun dart-syntax-propertize (start end)
	"A 'syntax-propertize-function for `dart-mode'.
Propertize text from START to END."
	(save-excursion
		(goto-char start)
		;; if we are inside a string already
		;;  check the type of string ('' "" '''''' """""" r'' r"")
		;;   if single quotes don't do anything
		;;   else search for matching quotes in within START and END
		;;     if found
		;;       mark last quote and call dart-syntax-properties again from new Start to END
		;;     else do nothing
		(when (re-search-forward "\\('''\\)\\|\\(\"\"\"\\)" end t)
			(if-let ((matchingquote (nth 8 (syntax-ppss (match-beginning 0)))))
					(if (and (equal (char-after matchingquote) (char-after (match-beginning 0))) (dart-syntax-is-triple-quote matchingquote))
							(put-text-property (+ (match-beginning 0) 2) (+ (match-beginning 0) 3) 'syntax-table (string-to-syntax "|")))
				(put-text-property (match-beginning 0) (+ (match-beginning 0) 1) 'syntax-table (string-to-syntax "|")))
			(when (< (match-end 0) end)
				(dart-syntax-propertize (1+ (match-end 0)) end)))))


;;; initialization

;;;###autoload (add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

(define-derived-mode dart-mode prog-mode "Dart"
	"Major mode for editing Dart"
	:syntax-table dart-mode-syntax-table
	(set (make-local-variable 'font-lock-defaults)
			 '(dart--build-font-lock-keywords))
	;; (if dart-enable-auto-pos-tip
	;; 		(setq dart-pos-tip-timer (dart--turn-on-pos-tip-with-timer)))
	(setq-local syntax-propertize-function #'dart-syntax-propertize)
	(add-hook (make-local-variable 'before-save-hook)
						(lambda () (when dart-format-on-save (dart-format))))
	(setq-local indent-tabs-mode nil))



(provide 'dart-mode)
;;; idart.el ends here
