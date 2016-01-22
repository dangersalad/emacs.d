;;; window-purpose-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for purpose package

;;; Code:

(require 'window-purpose)
(require 'window-purpose-prefix-overload)

(add-to-list 'purpose-user-mode-purposes '(js-mode . code))
(add-to-list 'purpose-user-mode-purposes '(web-mode . code))
(add-to-list 'purpose-user-mode-purposes '(prog-mode . code))
(add-to-list 'purpose-user-mode-purposes '(term-mode . aux))
(add-to-list 'purpose-user-mode-purposes '(erc-mode . aux))
(add-to-list 'purpose-user-mode-purposes '(eshell-mode . aux))
(add-to-list 'purpose-user-mode-purposes '(dired-mode . aux))
(add-to-list 'purpose-user-mode-purposes '(git-commit-mode . popup))
(add-to-list 'purpose-user-regexp-purposes '("magit" . popup))
(add-to-list 'purpose-user-name-purposes '("*Help*" . popup))
(add-to-list 'purpose-user-name-purposes '("*scratch*" . aux))

(defalias 'purpose-friendly-find-file
  (purpose-ido-caller #'ido-find-file #'helm-find-files))

(defalias 'find-file-without-purpose
  (without-purpose-command #'helm-find-files))

(define-purpose-prefix-overload purpose-find-file-overload
  '(purpose-friendly-find-file find-file-without-purpose))

(defalias 'purpose-friendly-switch-buffer
  (purpose-ido-caller #'ido-find-file #'helm-mini))

(defalias 'switch-buffer-without-purpose
  (without-purpose-command #'helm-mini))

(define-purpose-prefix-overload purpose-find-file-overload
  '(purpose-friendly-find-file find-file-without-purpose))

(define-purpose-prefix-overload purpose-switch-buffer-overload
  '(purpose-friendly-switch-buffer switch-buffer-without-purpose))

(define-key purpose-mode-map (kbd "C-x C-f") 'purpose-find-file-overload)
(define-key purpose-mode-map (kbd "C-x b") 'purpose-switch-buffer-overload)

(purpose-compile-user-configuration)

;;; window-purpose-config.el ends here
