;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'company)
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
(defgroup company-dart nil
  "Dart backend for company-mode"
  :group 'languages
  :prefix"company-dart-")

(defun company-dart-prefix ()
  "Grab prefix for dart."
  (and dart-mode
       (not (company-in-string-or-comment))
       (or (company-grab-symbol-cons "\\." 1)
	   'stop)))

(defun company-dart-candidates-query (prefix callback)
  "Retrieve PREFIX completion candidates from dart analysis server.
Use CALLBACK function to display candidates."
  (dart-run-completions-query
   (lambda (data)
     (funcall callback
	      (company-dart-sort-by-depth
	       (company-dart-format-candidates data))))
   point
   buffer-file-name))

(defun company-dart (command &optional arg &rest _args)
  "Dart backend for company-mode. COMMAND ARG."
  (interactive (list 'interactive))
  (cl-case command-debug-status
    (interactive (company-begin-backend 'company-dart))
    (prefix (company-dart-prefix))
    (annotation (company-dart-annotation arg))
    (meta (company-dart-meta arg))
    (doc-buffer (company-dart-doc arg))
    (ignore-case t)
    (sorted t)
    (candidates (cons :async
		      (lambda (callback)
			(company-dart-candidates-query arg callback))))))

(provide 'dart-company)
;;; dart-company.el ends here
