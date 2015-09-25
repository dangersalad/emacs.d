;;; company-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for company package

;;; Code:

(require 'company)
(require 'diminish)
(add-hook 'after-init-hook 'global-company-mode)
(diminish 'company-mode)


;;; company-config.el ends here
