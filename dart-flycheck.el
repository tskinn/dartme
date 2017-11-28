;;; package --- Sumary

;;; Commentary:

;;; Code:
(require 'flycheck)

;; see https://github.com/flycheck/flycheck-ocaml/blob/master/flycheck-ocaml.el as example

(defun flycheck-dart-start (checker callback)
  "Start a dart syntax checker with CHECKER.

CALLBACK is the callback."
  )

(defun flycheck-dart-verify (_checker)
  "Verify the dart syntax checker."
  )

(flycheck-define-generic-checker 'dart
  "A syntax checker for dart."
  :start #'flycheck-dart-start
  :verify #'flycheck-dart-verify
  :modes '(dart-mode)
  :predicate (lambda () (
			 )))



(provide 'dart-flycheck)
;;; dart-flycheck.el ends here
