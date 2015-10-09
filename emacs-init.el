
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("sunrise-commander" . "http://joseito.republika.pl/sunrise-commander/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require'use-package)

(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-10"))

(setq inhibit-startup-message t)

(global-visual-line-mode 0)
(add-hook 'prog-mode-hook (visual-line-mode 1))

(setq scroll-conservatively 20)
(setq scroll-margin 5)

(setq-default create-lockfiles nil)

(defvar my-backups
  (expand-file-name "tmp/backups" user-emacs-directory)
  "Where backups go.")
(defvar my-autosave
  (expand-file-name "tmp/autosave" user-emacs-directory)
  "Where autosaves go.")
(make-directory my-backups t)
(make-directory my-autosave  t)
(setq backup-by-copying t
      backup-directory-alist `((".*" .  ,my-backups))
      auto-save-file-name-transforms `((".*"  ,my-autosave t))
      auto-save-list-file-prefix  my-autosave
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-auto-revert-mode)

(defvar my-customs-file
  (expand-file-name "custom.el" user-emacs-directory)
  "File for customizations via \\[customize].")
(setq custom-file my-customs-file)
(if (file-readable-p my-customs-file)
    (progn
      (load custom-file)))

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(use-package custom-functions
  :commands (bol-with-prefix)
  :load-path "lib"
  :bind ("C-c \\" . indent-buffer)
  :init
  (global-set-key [remap kill-line] (bol-with-prefix kill-line))
  (global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
  (global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line)))

(use-package zenburn-theme
  :ensure t
  :demand
  :init (use-package zenburn-colors :load-path "lib")
  :config
  (load-theme 'zenburn t)
  ;; default face customizations
  (zenburn-with-color-variables
    ;; darker region selection
    (set-face-attribute 'region nil :background zenburn-bg-2)
    ;; flat mode and header lines
    (set-face-attribute 'header-line nil :background zenburn-bg+1 :box nil)
    (set-face-attribute 'mode-line nil :background zenburn-bg+1 :box nil)
    (set-face-attribute 'mode-line-inactive nil :foreground zenburn-bg+3 :background zenburn-bg+05 :box nil)
    ;; italic comments
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic))
  ;; powerline colors
  (with-eval-after-load 'powerline
    (zenburn-with-color-variables
      (set-face-attribute 'powerline-active1 nil :background zenburn-bg+05 :foreground zenburn-green+1)
      (set-face-attribute 'powerline-active2 nil :background zenburn-bg+1 :foreground zenburn-green+1)
      (set-face-attribute 'powerline-inactive1 nil :background zenburn-bg+05 :foreground zenburn-bg+3)
      (set-face-attribute 'powerline-inactive2 nil :background zenburn-bg+05 :foreground zenburn-bg+3)))
  ;; flycheck use straight underline instead of wave
  (with-eval-after-load 'flycheck
    (zenburn-with-color-variables
      (set-face-attribute 'flycheck-error nil :underline `(:style line :color ,zenburn-red-1))
      (set-face-attribute 'flycheck-warning nil :underline `(:style line :color ,zenburn-yellow-2))
      (set-face-attribute 'flycheck-info nil :underline `(:style line :color ,zenburn-blue-2))))
  ;; helm faces
  (with-eval-after-load 'helm
    (zenburn-with-color-variables
      (set-face-attribute 'helm-source-header nil :background zenburn-bg+2 :height 1.3 :box '(:style nil))
      (set-face-attribute 'helm-selection nil :background zenburn-bg+2 :weight 'bold)))
  (with-eval-after-load 'helm-utils
    (zenburn-with-color-variables
      (set-face-attribute 'helm-selection-line nil :background zenburn-bg+2)))
  ;; faces for ledger mode
  (with-eval-after-load 'ledger-mode
    (zenburn-with-color-variables
      (set-face-attribute 'ledger-font-auto-xact-face nil :foreground zenburn-yellow)
      (set-face-attribute 'ledger-font-periodic-xact-face nil :foreground zenburn-green+3)
      (set-face-attribute 'ledger-font-xact-cleared-face nil :foreground zenburn-fg)
      (set-face-attribute 'ledger-font-xact-pending-face nil :foreground zenburn-yellow-2)
      (set-face-attribute 'ledger-font-xact-open-face nil :foreground zenburn-bg-1)
      (set-face-attribute 'ledger-font-payee-uncleared-face nil :foreground zenburn-fg-1)
      (set-face-attribute 'ledger-font-payee-pending-face nil :foreground zenburn-yellow-2)
      (set-face-attribute 'ledger-font-pending-face nil :foreground zenburn-yellow-2)
      (set-face-attribute 'ledger-font-other-face nil :foreground zenburn-blue-1)
      (set-face-attribute 'ledger-font-posting-account-face nil :foreground zenburn-blue-3 )
      (set-face-attribute 'ledger-font-posting-amount-face nil :foreground zenburn-green+4 )
      (set-face-attribute 'ledger-font-posting-date-face nil :foreground zenburn-orange :underline t)
      (set-face-attribute 'ledger-font-report-clickable-face nil :foreground zenburn-fg+1))))

(use-package ag
  :ensure t)
(use-package helm-ag
  :ensure t)

(use-package helm
  :ensure t
  :pin melpa-stable
  :diminish helm-mode
  :commands (helm-select-action)
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :init
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (define-key helm-map (kbd "C-i")  'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>")  'helm-execute-persistent-action)
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
  (setq helm-split-window-in-side-p t)
  (setq helm-autoresize-max-height 25)
  (setq helm-autoresize-min-height 25)

  (helm-mode 1))

(use-package magit
  :ensure t
  :pin melpa-stable
  :config
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
  (define-key global-map (kbd "C-c C-g") my-git-command-map))

(use-package adaptive-wrap
  :init (defvar adaptive-wrap-extra-indent 2)
  :config (add-hook 'visual-line-mode-hook
                    '(lambda ()
                       (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))))
