;;; ledger-mode-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for ledger-mode package

;;; Code:

(require 'ledger-mode)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.ldg$" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.rec$" . ledger-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ledger . t)))

(add-hook 'ledger-mode-hook
          (lambda ()
            (if (not (string= (buffer-name) ledger-schedule-buffer-name))
            (set-local-variable
             'ledger-schedule-file
             (replace-regexp-in-string
              "\\.\\(ledger\\|ldg\\)" ".rec" (buffer-file-name) nil 'literal)))))


;;; ledger-mode-config.el ends here
