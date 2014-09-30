;;; go-mode-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for go-mode package

;;; Code:

(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

;;; go-mode-config.el ends here
