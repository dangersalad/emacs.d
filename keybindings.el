;;; keybindings --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for package

;;; Code:

(global-set-key (kbd "C-x g") 'magit-status)

(define-key global-map (kbd "C-c r") 'vr/replace)

(global-set-key (kbd "<f9>") 'mu4e)

(global-set-key (kbd "C-c C-/ C-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-c C-/ C-l") 'evilnc-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-c C-/ C-c") 'evilnc-copy-and-comment-lines)
(global-set-key (kbd "C-c C-/ C-p") 'evilnc-comment-or-uncomment-paragraphs)

;;; keybindings.el ends here
