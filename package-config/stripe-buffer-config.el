;;; stripe-buffer-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for stripe-buffer package

;;; Code:

(require 'stripe-buffer)
(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
(add-hook 'dired-mode-hook 'stripe-listify-buffer)

(add-hook 'prog-mode-hook 'turn-on-stripe-buffer-mode)
(set-face-attribute 'stripe-highlight nil :background "gray13")


;;; stripe-buffer-config.el ends here
