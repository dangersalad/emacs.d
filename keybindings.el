;;; keybindings --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for package

;;; Code:

(require 'custom-functions)

;; custom indent function
(global-set-key (kbd "C-c \\") 'indent-buffer)

(global-set-key (kbd "C-x C-w") 'whitespace-mode)

(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))


(define-key Info-mode-map (kbd "<mouse-8>") 'Info-history-back)
(define-key Info-mode-map (kbd "<mouse-9>") 'Info-history-forward)


;;; keybindings.el ends here
