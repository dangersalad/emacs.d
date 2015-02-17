;;; smartparens-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for smartparens package

;;; Code:

(require 'smartparens-config)
(show-smartparens-global-mode t)
(add-hook 'emacs-lisp-mode #'smartparens-mode)

;;; smartparens-config.el ends here
