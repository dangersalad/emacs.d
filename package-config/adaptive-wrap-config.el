;;; adaptive-wrap-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for adaptive-wrap package

;;; Code:

(defvar adaptive-wrap-extra-indent 2)
(require 'adaptive-wrap)
(add-hook 'visual-line-mode-hook
          '(lambda ()
             (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1))))

;;; adaptive-wrap-config.el ends here
