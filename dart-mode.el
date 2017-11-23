;;; package --- Summary
;;; Commentary:
;;; Code:
;; define keywords
;; (setq dart-keywords-zero '("assert" "break" "case" "catch" "class" "const" "continue" "default" "do" "else" "enum" "extends" "false" "final" "finally" "for" "if" "in" "is" "new" "null" "rethrow" "return" "super" "switch" "this" "throw" "true" "try" "var" "void" "while" "with"))
;; (setq dart-keywords-one '("abstract" "as" "covariant" "deferred" "dynamic" "export" "external" "factory" "get" "implements" "import" "library" "operator" "part" "set" "static" "typedef"))
;; (setq dart-keywords-two '("async" "await" "sync" "yield"))


;; "assert" "break" "case" "catch" "class" "const" "continue" "default" "do" "else" "enum" "extends" "false" "final" "finally" "for" "if" "in" "is" "new" "null" "rethrow" "return" "super" "switch" "this" "throw" "true" "try" "var" "void" "while" "with" "abstract" "as" "covariant" "deferred" "dynamic" "export" "external" "factory" "get" "implements" "import" "library" "operator" "part" "set" "static" "typedef" "async" "await" "sync" "yield"

(defvar dart-mode-hook nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               MAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar dart-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Dart major mode.")
;; OR
(defvar dart-mode-map
  (let ((map (make-sparse-keymap)))
    (unless (boundp 'electric-indent-chars)                   ;;
      (define-key map "}" #'dart-mode-insert-and-indent)      ;; delete this maybe
      (define-key map ")" #'dart-mode-insert-and-indent))     ;;
    (define-key map (kbd "C-c C-a") #'dart-import-add)
    map)
  "Keymap used for Dart major mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              FONT LOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst dart-dangling-operators-regexp "[^-]-\\|[^+]\\|+\\|[/*&><.=|^|]") ;; from golang
(defconst dart--max-dangling-operator-length 2
  "The maximum length of dangling operators.")

(defconst dart-identifier-regexp "_?[[:word:][:multibyte:]]+")
(defconst dart-type-name-no-prefix-regexp "\\(?:[[:word:][:multibyte:]]+\\.\\)?[[:word:][:multibyte:]]+")
(defconst dart-qualified-identifier-regexp (concat dart-identifier-regexp "\\." dart-type-name-no-prefix-regexp))
(defconst dart-label-regexp dart-identifier-regexp)
(defconst dart-type-regexp "[[:word:][:multibyte:]*]+")
(defconst dart-func-regexp dart-identifier-regexp)
(defconst dart-func-meth-regexp "")

(defconst dart-builtins
  '("")
  "All built-in functions in the Dart language. Used for font locking.")

(defconst dart-keywords
  '("assert" "break" "case" "catch" "class" "const" "continue" "default" "do" "else" "enum" "extends" "false" "final" "finally" "for" "if" "in" "is" "new" "null" "rethrow" "return" "super" "switch" "this" "throw" "true" "try" "var" "void" "while" "with" "abstract" "as" "covariant" "deferred" "dynamic" "export" "external" "factory" "get" "implements" "import" "library" "operator" "part" "set" "static" "typedef" "async" "await" "sync" "yield")
  "All keywords in dart.")
(defconst dart-constants
  '("true" "false" "null")
  "All constants.")

(setq dart-keywords-regexp (regexp-opt dart-keywords 'words))
(setq dart-constants-regexp (regexp-opt dart-constants 'words))

(setq dart-font-lock-keywords
      `(
	(,dart-constants-regexp . font-lock-constant-face)
	(,dart-keywords-regexp . font-lock-kewords)
	("'\\{3\\}\\(.*?\n*?\\)*?'\\{3\\}" . font-lock-multiline)
	("\"\\{3\\}\\(.*\n*\\)*?\"\\{3\\}" . font-lock-multiline)))

;;(defconst dart-type-name-regexp (concat ""))
;; (defconst dart-font-lock-keywords-1
;;   (list
;;    '("'\\{3\\}\\(.*?\n*?\\)*?'\\{3\\}" . font-lock-multiline) ;; '''
;;    '("\"\\{3\\}\\(.*\n*\\)*?\"\\{3\\}" . font-lock-multiline)) ;; """
;;   "Minimal highlighting expressions for Dart mode.")

;; (defun dart--build-font-lock-keywords ()
;;   (append
;;    '((,(concat "\\_<" (regexp-opt dart-constants t) "\\_>") . font-lock-constant-face))
;;    '((,(concat "\\_<" (regexp-opt dart-keywords t) "\\_>") . font-lock-keyword-face))
;;    '(,("'\\{3\\}\\(.*?\n*?\\)*?'\\{3\\}" . font-lock-multiline))
;;    '(,("\"\\{3\\}\\(.*\n*\\)*?\"\\{3\\}" . font-lock-multiline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              SYNTAX TABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar dart-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)      ;;
    (modify-syntax-entry ?/ ". 124b" st) ;;
    (modify-syntax-entry ?* ". 23" st)   ;;  see https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html#Syntax-Flags
    (modify-syntax-entry ?\n "> b" st)   ;; comment ender
    (modify-syntax-entry ?' "\"" st)     ;; ' string delimiter
    (modify-syntax-entry ?\" "\"" st)    ;; " string delimiter
    st)
  "Syntax table for dart-mode.")


;; (defun dart-mode ()
;;   "Major mode for editing dart files"
;;   (interactive)
;;   (kill-all-local-variables)
;;   (set-syntax-table dart-mode-syntax-table)
;;   (use-local-map dart-mode-map)
;;   (set (make-local-variable 'font-lock-defaults) '(dart-font-lock-keywords))
;;   (set (make-local-variable 'indent-line-function) 'dart-indent-line)
;;   (setq major-mode 'dart-mode)
;;   (setq mode-name "Dart")
;;   (run-hooks 'dart-mode-hook))
;; OR
(define-derived-mode dart-mode prog-mode "iDart"
  "Major mode for editing dart files."
  (setq font-lock-defaults '((dart-font-lock-keywords)))
;;  (set (make-local-variable 'font-lock-defaults) '(dart-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'dart-indent-line))

(provide 'dart-mode)
;;; dart-mode.el ends here
