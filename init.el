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
(defvar my-custom-themes
  "~/.emacs.d/themes"
  "Customized Emacs themes.")
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
(defvar my-local-dir
  "~/.emacs.d/local"
  "Directory for local, non git controlled files.")
(defvar my-local-init
  (concat my-local-dir "/init.el")
  "Local init file to be loaded at the end, ignored by version control.")

;; my load path
(add-to-list 'load-path my-custom-lib)

(defvar my-packages '(ag
                      flycheck
                      linum-relative
                      auto-complete
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
                      less-css-mode
                      highlight-symbol
                      helm
                      markdown-mode
                      yaml-mode
                      org
                      org-journal
                      web-mode
                      idomenu
                      imenu-anywhere
                      web-mode
                      ibuffer-vc
                      evil
                      evil-surround
                      evil-leader
                      evil-nerd-commenter
                      key-chord
                      visual-regexp
                      volatile-highlights
                      projectile
                      yasnippet)
  "A list of packages to install.")

;; my cusomized zenburn with darker background
(add-to-list 'custom-theme-load-path my-custom-themes)
(load-theme 'zenburn t)


;; Packages in lib/
;; These can all be compiled
(require 'auto-byte-compile) ; auto compile lisp, only if a .elc file exists
(require 'custom-functions)
(require 'set-local-variable) ; allows setting a local variable easily
(require 'package-loader) ; auto load packages from MELPA
(require 'indentation) ; indentation rules
(require 'line-opening) ; vim like line opening C-o and M-o
(require 'marks)        ; mark tweaks
;;(require 'major-mode-from-name) ; set major mode from buffer name as well as file name
(require 'popup-terminal)
(require 'diminish)

(setq newline-and-indent t) ; enable indentation detection for line-opening

;; scroll less aggressivly
(setq scroll-step 1)
(setq scroll-margin 1)

;; no lock files (fucks with git and grunt)
(setq-default create-lockfiles nil)

;; tramp settings
(setq tramp-default-method "ssh")

(add-hook 'before-save-hook (lambda ()
                              (if (equal major-mode 'markdown-mode)
                                  (message "Retaining whitespace")
                                (delete-trailing-whitespace))))

(add-hook 'prog-mode (lambda () (interactive) (setq show-trailing-whitespace 1)))
(global-set-key (kbd "C-c w") 'whitespace-mode)

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
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(show-paren-mode 1)

;; no menu bar please
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; calendar shortcut
(global-set-key (kbd "<f3>") 'calendar)

;; font for gui
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-8"))

;; ido mode
(ido-mode)

;; winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; unique buffer names
(require 'uniquify)
(customize-set-variable 'uniquify-buffer-name-style 'post-forward)

;; ibuffer
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-use-other-window t)

(global-auto-revert-mode)

;; custom custom fils
(setq custom-file my-customs-file)
(if (file-readable-p my-customs-file)
    (progn
      (load custom-file)))

;;enable stuff
(put 'narrow-to-region 'disabled nil)

;; jump tp config
(set-register ?z '(file  . "~/.emacs.d"))

;; erc settings
(defvar erc-hide-list '("JOIN" "PART" "QUIT"))
(require 'erc)

;; init packages
(message "Looking for missing packages")
(install-missing-packages)
(message "Loading package configurations")
(my-package-init)

;; load keybindings
(load "~/.emacs.d/keybindings")

;; load email configuration
(load "~/.emacs.d/email/email-config")

;; diminished mode line
(diminish 'js2-minor-mode)
(diminish 'volatile-highlights-mode)
(diminish 'undo-tree-mode (string 32 #x236b))
(diminish 'magit-auto-revert-mode)

;; dired
(setq dired-listing-switches "-lha --group-directories-first")

;; load local init if available
(if (file-readable-p my-local-init)
    (load (concat my-local-init)))

(provide 'init)
;;; init.el ends here
