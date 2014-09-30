;;; dockerfile-mode-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for dockerfile-mode package

;;; Code:

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;; dockerfile-mode-config.el ends here
