;;; keybindings --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for package

;;; Code:

(defvar my-git-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'magit-status)
    (define-key map (kbd "C-g") 'magit-status)
    (define-key map "l" 'magit-log)
    (define-key map "f" 'magit-fetch-current)
    (define-key map "h" 'helm-git-files)
    (define-key map "!" 'magit-blame-mode)
    (define-key map "c" 'magit-checkout)
    (define-key map (kbd "C-r") 'magit-rebase-step)
    (define-key map (kbd "C-f") 'magit-pull)
    (define-key map (kbd "C-p") 'magit-push)
    (define-key map (kbd "z z") 'magit-stash)
    (define-key map (kbd "z p") 'magit-stash-pop)
    (define-key map (kbd "C-t") 'git-timemachine)
    (define-key map (kbd "C-c") 'magit-create-branch)
    map)
  "Keymap of commands to load magit.")

(define-key global-map (kbd "C-c g") my-git-command-map)
(define-key global-map (kbd "C-c C-g") my-git-command-map)

(define-key global-map (kbd "C-c r") 'vr/replace)

(global-set-key (kbd "<f9>") 'mu4e)
;; toggle vim bindings with C-f1
(global-set-key (kbd "<f12>") 'evil-local-mode)

;; custom indent function
(global-set-key (kbd "C-c \\") 'indent-buffer)

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

(global-set-key (kbd "C-x C-w") 'whitespace-mode)
(global-set-key (kbd "C-x C-b") 'helm-mini)

(global-set-key (kbd "C-x t") 'win-toggle-vert-split)
(global-set-key (kbd "C-x T") 'win-equalize-vert)

(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))


(define-key Info-mode-map (kbd "<mouse-8>") 'Info-history-back)
(define-key Info-mode-map (kbd "<mouse-9>") 'Info-history-forward)

;; cycle through errors in prog modes
(define-key prog-mode-map (kbd "C-c C-n") 'flycheck-tip-cycle)

;;; keybindings.el ends here
