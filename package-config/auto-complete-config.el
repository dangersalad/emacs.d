;;; auto-complete-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for auto-complete package

;;; Code:

(require 'auto-complete)
(add-to-list 'ac-modes 'javascript-mode)
(global-auto-complete-mode t)
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "M-/") 'auto-complete)

;;; auto-complete-config.el ends here
