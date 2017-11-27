;;; package --- Summary
;;; Commentary:
;;; Code:

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

;;;###autoload
(define-derived-mode dart-mode prog-mode "iDart"
  "Major mode for editing LSL (Linden Scripting Language)…"

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


(setq dart-executable-path (executable-find "dart"))
(setq dart-analysis-server-path "/opt/dart-sdk/bin/snapshots/analysis_server.dart.snapshot")
;; start processes using pipe
;; (let ((process-connection-type nil))  ; Use a pipe.
;;   (start-process ...))
(defun create-dart-process ()
  "Some docs."
  (let ((process-connection-type nil))
    (let ((proc (start-process "dart-process" "*dart*" dart-executable-path dart-analysis-server-path)))
      proc)))

(setq dart-process (create-dart-process))
;(setq dart-process (start-process "dart-process" "*dart*" dart-executable-path dart-analysis-server-path))


(provide 'dart-mode)
;;; dart-mode.el ends here
