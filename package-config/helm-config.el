;;; helm-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for helm package

;;; Code:

(require 'helm-config)
(require 'diminish)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(require 'helm)
(require 'helm-net)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-command)


(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(helm-autoresize-mode 1)

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-M-x-fuzzy-match                  t ; fuzzy match M-x
      helm-recentf-fuzzy-match              t ; fuzzy match recent files
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)


(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-split-window-in-side-p t)
(setq helm-autoresize-max-height 25)
(setq helm-autoresize-min-height 25)

(helm-mode 1)

(diminish 'helm-mode)

;;; helm-config.el ends here
