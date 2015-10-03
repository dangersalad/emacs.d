;;; magit-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for magit package

;;; Code:

(require 'magit)
(setq magit-merge-arguments '("--no-ff"))

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


;;; magit-config.el ends here
