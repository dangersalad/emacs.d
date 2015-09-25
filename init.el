;;; package --- Summary
;;; Commentary:
;;; My init
;;; Code:

;; no splash
(setq inhibit-startup-message t)

;; file paths vairables
(defvar my-custom-lib
  "~/.emacs.d/lib"
  "Custom elisp library.")
(defvar my-customs-file
  "~/.emacs.d/custom.el"
  "File for customizations via \\[customize].")
(defvar my-backups
  "~/.emacs.d/tmp/backups"
  "Where backups go.")
(defvar my-autosave
  "~/.emacs.d/tmp/autosave"
  "Where autosaves go.")
(defvar my-package-configs
  "~/.emacs.d/package-config"
  "Configuration for MELPA packages.")
(defvar my-keybindings
  "~/.emacs.d/keybindings"
  "Custom keybindings.
Kept here for easier viewing rather than each package's config.")
(defvar my-macros
  "~/.emacs.d/macros"
  "Custom keyboard macros.")
(defvar my-local-dir
  "~/.emacs.d/local"
  "Directory for local, non git controlled files.")
(defvar my-local-init
  (concat my-local-dir "/init.el")
  "Local init file to be loaded at the end, ignored by version control.")

(defvar my-email-settings
  "~/.emacs.d/email"
  "Email settings location.")

;; my load path
(add-to-list 'load-path my-custom-lib)
(add-to-list 'load-path my-email-settings)

(defvar my-packages '(
                      ag
                      org
                      org-journal
                      org-trello
                      adaptive-wrap
                      flycheck
                      linum-relative
                      company
                      company-quickhelp
                      magit
                      undo-tree
                      git-gutter-fringe
                      git-timemachine
                      js2-mode
                      ac-js2
                      js2-refactor
                      restclient
                      dockerfile-mode
                      go-mode
                      company-go
                      go-eldoc
                      go-scratch
                      less-css-mode
                      ledger-mode
                      flycheck-ledger
                      helm
                      helm-projectile
                      helm-git-files
                      helm-ag
                      helm-flycheck
                      helm-descbinds
                      helm-swoop
                      helm-go-package
                      ac-helm
                      markdown-mode
                      yaml-mode
                      web-mode
                      ibuffer-vc
                      evil
                      evil-surround
                      evil-leader
                      evil-nerd-commenter
                      key-chord
                      visual-regexp
                      projectile
                      window-purpose
                      aggressive-indent
                      sunrise-commander
                      sunrise-x-popviewer
                      sunrise-x-tree
                      smartparens
                      yasnippet
                      powerline
                      zoom-frm
                      zenburn-theme
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
(require 'package-loader)         ; auto load packages from MELPA
(require 'local-powerline-themes) ; hide minor modes in modeline
(require 'erc-settings)           ; settings for erc
(require 'unit-file-mode)         ; a major mode for editing systemd unit files

(setq newline-and-indent t)   ; enable indentation detection for line-opening

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
(setq
 backup-by-copying t
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

;; jump tp config
(set-register ?z '(file  . "~/.emacs.d"))

;; init packages
(message "Looking for missing packages")
(install-missing-packages)
(message "Loading package configurations")
(my-package-init)

;; load keybindings
(load my-keybindings)


;; load macros
(load my-macros)

;; load email configuration
(require 'email-config)

;; dired
(setq dired-listing-switches "-lha --group-directories-first")

;; load local init if available
(if (file-readable-p my-local-init)
    (load my-local-init))

(provide 'init)
;;; init.el ends here
