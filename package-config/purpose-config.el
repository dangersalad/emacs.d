;;; purpose-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for purpose package

;;; Code:

(require 'purpose)

(add-to-list 'purpose-user-mode-purposes '(js-mode . code))
(add-to-list 'purpose-user-mode-purposes '(web-mode . code))
(add-to-list 'purpose-user-mode-purposes '(term-mode . term))
(add-to-list 'purpose-user-regexp-purposes '("magit" . git))

(purpose-compile-user-configuration)

;;; purpose-config.el ends here
