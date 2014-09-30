;;; package --- Summary
;;; Commentary:
;;; My init
;;; Code:

;; no splash
(setq inhibit-startup-message t)

;; file paths vairables
(defvar my-custom-lib "~/.emacs.d/lib" "Custom elisp library.")
(defvar my-custom-themes "~/.emacs.d/themes" "Customized Emacs themes.")
(defvar my-customs-file "~/.emacs.d/custom.el" "File for customizations via \\[customize].")
(defvar my-backups "~/.emacs.d/tmp/backups" "Where backups go.")
(defvar my-autosave "~/.emacs.d/tmp/autosave" "Where autosaves go.")
(defvar my-package-configs "~/.emacs.d/package-config" "Configuration for MELPA packages.")
(defvar my-keybindings "~/.emacs.d/keybindings" "Custom keybindings, kept here for easier viewing rather than each package's config.")
(defvar my-local-dir "~/.emacs.d/local" "Directory for local, non git controlled files.")

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
                      markdown-mode
                      yaml-mode
                      org
                      org-journal
                      web-mode
                      idomenu
                      imenu-anywhere
                      web-mode
                      stripe-buffer
                      evil-nerd-commenter
                      visual-regexp
                      projectile
                      yasnippet)
  "A list of packages to install.")

;; my cusomized zenburn with darker background
(add-to-list 'custom-theme-load-path my-custom-themes)
(load-theme 'zenburn t)


;; Packages in lib/
;; These can all be compiled
(require 'auto-byte-compile) ; auto compile lisp, only if a .elc file exists
(require 'set-local-variable) ; allows setting a local variable easily
(require 'package-loader) ; auto load packages from MELPA
(require 'indentation) ; indentation rules
(require 'line-opening) ; vim like line opening C-o and M-o
(require 'marks)        ; mark tweaks
(require 'major-mode-from-name) ; set major mode from buffer name as well as file name
(require 'popup-terminal)
(require 'diminish)


(setq newline-and-indent t) ; enable indentation detection for line-opening

;; scroll less aggressivly
(setq scroll-step 1)
(setq scroll-margin 10)

;; no lock files (fucks with git and grunt)
(setq-default create-lockfiles nil)

;; tramp settings
(setq tramp-default-method "ssh")

;; function for fixing format errors in a file
(defun clean-whitespace-region (start end)
  "Untabify, remove trailing whitespace, and re-indent a region from START to END."
  (interactive "r")
  (save-excursion
    (untabify start end)
    (indent-according-to-mode)
    (delete-trailing-whitespace)))

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

;; whitespace mode
;; Only displays tab characters
(setq whitespace-display-mappings
       ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (tab-mark 9 [8594 9] [92 9])))

(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

(global-whitespace-mode 1)

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

;; diminished mode line
(diminish 'js2-minor-mode)
(diminish 'undo-tree-mode (string 32 #x236b))
(diminish 'magit-auto-revert-mode)



(provide 'init)
;;; init.el ends here
