;;; aggressive-indent-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for aggressive-indent package

;;; Code:
(require 'aggressive-indent)
(add-hook 'prog-mode-hook #'aggressive-indent-mode)

;;; aggressive-indent-config.el ends here
