;;; keybindings --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for package

;;; Code:

(global-set-key (kbd "C-x g") 'magit-status)

(define-key global-map (kbd "C-c r") 'vr/replace)

(global-set-key (kbd "<f9>") 'mu4e)
;; toggle vim bindings with C-f1
(global-set-key (kbd "<f12>") 'evil-local-mode)

(global-set-key (kbd "C-c C-/ C-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-c C-/ C-l") 'evilnc-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-c C-/ C-c") 'evilnc-copy-and-comment-lines)
(global-set-key (kbd "C-c C-/ C-p") 'evilnc-comment-or-uncomment-paragraphs)

(eval-after-load 'evil-leader
  (progn
    (evil-leader/set-key "\\" 'evilnc-comment-operator)
    (evil-leader/set-key "cl" 'evilnc-comment-or-uncomment-to-the-line)
    (evil-leader/set-key "cc" 'evilnc-copy-and-comment-lines)
    (evil-leader/set-key "cp" 'evilnc-comment-or-uncomment-paragraphs)))

(global-set-key (kbd "C-x C-b") 'ibuffer)


;;; keybindings.el ends here
