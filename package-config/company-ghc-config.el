;;; company-ghc-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for company-ghc package

;;; Code:

(require 'company-ghc)

(add-hook 'haskell-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-ghc))
  (company-mode)))


;;; company-ghc-config.el ends here
