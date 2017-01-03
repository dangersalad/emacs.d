;;; keybindings --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for package

;;; Code:

(require 'custom-functions)

;; custom indent function
(global-set-key (kbd "C-c \\") 'indent-buffer)

;; enable whitespace mode
(global-set-key (kbd "C-x C-w") 'whitespace-mode)

;; use a prefix arg to kill multiple lines from anywhere in the line
(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))

;; mouse back and forward buttons for info mode
(define-key Info-mode-map (kbd "<mouse-8>") 'Info-history-back)
(define-key Info-mode-map (kbd "<mouse-9>") 'Info-history-forward)

(global-set-key (kbd "M-S-SPC") 'mark-word-at-point)

;;; keybindings.el ends here
