
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa.org/packages/") t)
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

(use-package zenburn-theme
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
