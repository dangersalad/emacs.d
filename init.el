;;; package --- Summary
;;; Commentary:
;;; My init
;;; Code:

;; no splash
(setq inhibit-startup-message t)
;; file paths vairables
(defvar my-custom-lib
  (expand-file-name "lib" user-emacs-directory)
  "Custom elisp library.")
(defvar my-customs-file
  (expand-file-name "custom.el" user-emacs-directory)
  "File for customizations via \\[customize].")
(defvar my-backups
  (expand-file-name "tmp/backups" user-emacs-directory)
  "Where backups go.")
(defvar my-autosave
  (expand-file-name "tmp/autosave" user-emacs-directory)
  "Where autosaves go.")
(defvar my-package-configs
  (expand-file-name "package-config" user-emacs-directory)
  "Configuration for MELPA packages.")
(defvar my-keybindings
  (expand-file-name "keybindings" user-emacs-directory)
  "Custom keybindings.
Kept here for easier viewing rather than each package's config.")
(defvar my-macros
  (expand-file-name "macros" user-emacs-directory)
  "Custom keyboard macros.")
(defvar my-local-dir
  (expand-file-name "local" user-emacs-directory)
  "Directory for local, non git controlled files.")
(defvar my-local-init
  (expand-file-name "init.el" my-local-dir)
  "Local init file to be loaded at the end, ignored by version control.")

(defvar my-email-settings
  (expand-file-name "email" user-emacs-directory)
  "Email settings location.")

;; my load path
(add-to-list 'load-path my-custom-lib)
(add-to-list 'load-path my-email-settings)

(defvar my-packages '(
                      ;; load theme first
                      zenburn-theme
                      ;; general packages
                      ag adaptive-wrap linum-relative zoom-frm undo-tree visual-regexp
                      window-purpose projectile powerline smartparens yasnippet linum-relative
                      ;; mpd client
                      mingus
                      ;; completion and error checking
                      company company-quickhelp flycheck
                      ;; evil stuff
                      evil evil-leader evil-nerd-commenter evil-surround key-chord
                      ;; git related
                      magit git-gutter-fringe git-timemachine ibuffer-vc
                      ;; helm packages
                      helm helm-ag helm-descbinds helm-flycheck helm-git-files
                      helm-go-package helm-projectile helm-swoop flx
                      ;; org mode packages
                      org org-journal org-trello
                      ;; golang stuff
                      go-mode go-eldoc go-scratch company-go
                      ;; javascript
                      js2-mode js2-refactor ac-js2
                      ;; typescript
                      typescript-mode tide
                      ;; ledger stuff
                      ledger-mode flycheck-ledger
                      ;; web stuff
                      web-mode less-css-mode markdown-mode restclient yaml-mode
                      ;; docker packages
                      dockerfile-mode
                      ;; fancy file manager I never use...
                      sunrise-commander sunrise-x-popviewer sunrise-x-tree
                      )
  "A list of packages to install.")

;; use tcp for server
(defvar server-use-tcp t)

;; Packages in lib/
;; These can all be compiled
(require 'auto-byte-compile)      ; auto compile lisp, only if a .elc file exists
(require 'custom-functions)       ; misc custom functions
(require 'set-local-variable)     ; allows setting a local variable easily
(require 'indentation)            ; indentation rules
(require 'line-opening)           ; vim like line opening C-o and M-o
(require 'marks)                  ; mark tweaks
(require 'popup-terminal)         ; terminal set to <F11>
(require 'diminish)               ; hide minor modes in modeline
(require 'zenburn-colors)         ; custom colors for zenburn theme
(require 'erc-settings)           ; settings for erc
(require 'unit-file-mode)         ; a major mode for editing systemd unit files

;; use the package loader functions to install missing packages, then
;; load all the packages in MY-PACKAGES, either simply requiring
;; them, or loading package-config/{package name}-config.el to
;; customize
(require 'package-loader)         ; auto load packages from MELPA
(message "Looking for missing packages")
(install-missing-packages)
(require 'local-powerline-themes) ; my theme for powerline
;; init packages
(message "Loading package configurations")
(my-package-init)

(setq newline-and-indent t)   ; enable indentation detection for line-opening

;; visual line mode
(global-visual-line-mode 0)
(add-hook 'prog-mode-hook (lambda () (visual-line-mode 1)))

;; scroll less aggressivly
(setq scroll-step 1)
(setq scroll-margin 1)

;; no lock files (fucks with git and grunt)
(setq-default create-lockfiles nil)

;; tramp settings
(require 'tramp)
(setq tramp-default-method "ssh")

(add-hook 'before-save-hook (lambda ()
                              (if (equal major-mode 'markdown-mode)
                                  (message "Retaining whitespace")
                                (delete-trailing-whitespace))))

(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))
(add-hook 'prog-mode-hook (lambda()
                            (unless (equal major-mode 'web-mode)
                              (hs-minor-mode)
                              (diminish 'hs-minor-mode))))


;; make whitespace-mode use just basic coloring
(require 'whitespace)
(setq whitespace-style
      '(face trailing tab-mark lines-tail))

(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '((space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [8614 9] [92 9]))) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」

(setq whitespace-line-column 120)

;; backup settings
;; setup directories
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

;; move windows with Shift + <arrow>
(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; no menu bar please
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; calendar shortcut
(global-set-key (kbd "<f3>") 'calendar)

;; font for gui
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-10"))

;; winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; unique buffer names
(require 'uniquify)
(customize-set-variable 'uniquify-buffer-name-style 'post-forward)

;; ibuffer
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-use-other-window t)

;; detect changes on disk and update buffers
(global-auto-revert-mode)

;; custom customs file
(setq custom-file my-customs-file)
(if (file-readable-p my-customs-file)
    (progn
      (load custom-file)))

;;enable stuff
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

;; jump tp config
(set-register ?z '(file  . user-emacs-directory))

;; load keybindings
(load my-keybindings)

;; load macros
(load my-macros)

;; load email configuration
(let ((email-settings-file (concat my-email-settings "/email-settings.el")))
  (if (file-readable-p email-settings-file)
      (require 'email-config)
    (message (concat "Missing " email-settings-file ", skipping email setup."))))

;; dired
(setq dired-listing-switches "-lha --group-directories-first")

;; load local init if available
(if (file-readable-p my-local-init)
    (load my-local-init))

(provide 'init)
;;; init.el ends here
