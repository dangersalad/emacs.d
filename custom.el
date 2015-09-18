(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-reports
   (quote
    (("asset/liabilities" "ledger -f %(ledger-file) bal assets liabilities")
     ("profit/loss" "ledger -f %(ledger-file) bal income expenses")
     ("checkbook" "ledger -f %(ledger-file) reg personal:assets:checking")
     ("cc" "ledger -f %(ledger-file) reg personal:liabilities and visa")
     ("loans" "ledger -f %(ledger-file) reg personal:liabilities and loan personal:expense and loan")
     ("bal" "ledger -f %(ledger-file) bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("account" "ledger -f %(ledger-file) reg %(account)"))))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-color-mode-line-error-face ((t (:background "#bc8383" :foreground "#1c1c1c"))))
 '(flycheck-color-mode-line-info-face ((t (:background "#8cd0d3" :foreground "#1c1c1c"))))
 '(flycheck-color-mode-line-warning-face ((t (:background "#d0bf8f" :foreground "#1c1c1c")))))
