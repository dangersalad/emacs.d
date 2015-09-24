;;; company-go-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for company-go package

;;; Code:

(require 'company-go)

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)))


;;; company-go-config.el ends here
