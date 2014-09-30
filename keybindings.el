;;; keybindings --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for package

;;; Code:

(global-set-key (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-c r") 'vr/replace)
(global-set-key (kbd "<f9>") 'mu4e)


;;; keybindings.el ends here
