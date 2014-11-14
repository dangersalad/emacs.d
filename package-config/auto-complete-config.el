;;; auto-complete-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for auto-complete package

;;; Code:

(require 'auto-complete)
(ac-config-default)
(setq-default ac-sources (append ac-sources '(ac-source-yasnippet)))
(add-to-list 'ac-modes 'javascript-mode)
(global-auto-complete-mode t)
(setq ac-auto-start 3)

;;; auto-complete-config.el ends here
