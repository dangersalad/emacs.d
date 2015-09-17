;;; ledger-mode-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for ledger-mode package

;;; Code:

(require 'ledger-mode)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.ldg$" . ledger-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ledger . t)))



;;; ledger-mode-config.el ends here
