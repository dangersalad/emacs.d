;;; flycheck-color-mode-line-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for flycheck-color-mode-line package

;;; Code:

(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;;; flycheck-color-mode-line-config.el ends here
