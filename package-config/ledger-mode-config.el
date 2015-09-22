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

(defun look-for-ledger-schedule-file ()
  "See if there is a file in the same directory as this ledger file with the same basename and a \".rec\" extenxtion. If so, set the `ledger-schedule-file variable' to this file for the local buffer."
  (if (not (string= (buffer-name) ledger-schedule-buffer-name))
            (set-local-variable
             'ledger-schedule-file
             (replace-regexp-in-string
              "\\.\\(ledger\\|ldg\\)" ".rec" (buffer-file-name) nil 'literal))))

(add-hook 'ledger-mode-hook #'look-for-ledger-schedule-file)


;;; ledger-mode-config.el ends here
