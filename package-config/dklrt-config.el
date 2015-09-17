;;; dklrt-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for dklrt package

;;; Code:

(require 'dklrt)
(setq dklrt-PythonProgram "python2")
(add-hook 'ledger-mode-hook 'dklrt-AppendRecurringMaybe)
(add-hook 'ledger-mode-hook 'dklrt-SetCcKeys)
(add-to-list 'auto-mode-alist '("\\.rec$" . ledger-mode))



;;; dklrt-config.el ends here
