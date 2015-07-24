;;; helm-go-package-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for helm-go-package package

;;; Code:

(require 'helm-go-package)
(eval-after-load 'go-mode
  '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map))

;;; helm-go-package-config.el ends here
