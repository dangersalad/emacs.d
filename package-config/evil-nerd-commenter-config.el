;;; evil-nerd-commenter-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for evil-nerd-commenter package

;;; Code:

(require 'evil-nerd-commenter)

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



;;; evil-nerd-commenter-config.el ends here
