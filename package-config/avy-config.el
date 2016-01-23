;;; avy-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for avy package

;;; Code:

(require 'avy)

(avy-setup-default)
(global-set-key (kbd "C-;") 'avy-goto-char)

(global-set-key (kbd "C-:") 'avy-goto-word-or-subword-1)

(global-set-key (kbd "C-'") 'avy-goto-char-2)

(global-set-key (kbd "C-\"") 'avy-goto-subword-1)

;;; avy-config.el ends here
