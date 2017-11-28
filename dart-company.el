;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defvar company-dart-keywords
  '(("foo" "overload 1") ("foo" "overload 2")))

(defun company-dart--make-candidate (candidate)
  "CANDIDATE."
  (let ((text (car candidate))
	(meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun company-dart--candidates (prefix)
  "PREFIX."
  (let (res)
    (dolist (item company-dart-keywords)
      (when (string-prefix-p prefix (car item))
	(push (company-dart--make-candidate item) res)))
    res))

(defun company-dart--meta (candidate)
  "CANDIDATE."
  (format "This will use %s of %s"
	  (get-text-property 0 'meta candidate)
	  (substring-no-properties candidate)))

(defun company-dart--annotation (candidate)
  "CANDIDATE."
  (format " (%s)" (get-text-property 0 'meta candidate)))

(defun company-dart (command &optional arg &rest ignored)
  "COMMAND ARG IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-dart))
    (prefix (company-grab-symbol-cons "\\.\\|->" 2))
    (candidates (company-dart--candidates arg))
    (annotation (company-dart--annotation arg))
    (meta (company-dart--meta arg))))

;; TODO see https://github.com/proofit404/company-tern/blob/master/company-tern.el as an example of async backend

(provide 'dart-company)
;;; dart-company.el ends here


