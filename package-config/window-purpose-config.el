;;; window-purpose-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for purpose package

;;; Code:

(require 'window-purpose)

(add-to-list 'purpose-user-mode-purposes '(js-mode . code))
(add-to-list 'purpose-user-mode-purposes '(web-mode . code))
(add-to-list 'purpose-user-mode-purposes '(prog-mode . code))
(add-to-list 'purpose-user-mode-purposes '(term-mode . aux))
(add-to-list 'purpose-user-mode-purposes '(dired-mode . aux))
(add-to-list 'purpose-user-mode-purposes '(git-commit-mode . popup))
(add-to-list 'purpose-user-regexp-purposes '("magit" . popup))
(add-to-list 'purpose-user-regexp-purposes '("helm" . popup))
(add-to-list 'purpose-user-name-purposes '("*Help*" . popup))
(add-to-list 'purpose-user-name-purposes '("*scratch*" . aux))


(purpose-compile-user-configuration)

;;; window-purpose-config.el ends here
