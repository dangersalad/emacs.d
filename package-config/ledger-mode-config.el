;;; ledger-mode-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for ledger-mode package

;;; Code:

(require 'ledger-mode)
(require 'set-local-variable)
(require 'subr-x)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.ldg$" . ledger-mode))
(add-to-list 'auto-mode-alist '("\\.rec$" . ledger-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ledger . t)))

(defun find-ledger-directory ()
  "Get directory with ledger files."
  (let ((ledgerrc (concat (getenv "HOME") "/.ledgerrc")))
    (if (file-readable-p ledgerrc)
        (let ((conffile (with-temp-buffer
                          (insert-file-contents ledgerrc)
                          (split-string (buffer-string) "\n")))
              (filename ""))
          (dolist (ln conffile filename)
            (message ln)
            (if (string-match "^--file" ln)
                (setq filename (replace-regexp-in-string "^--file \\([[:graph:]]\+\\)" "\\1" ln))
              nil))
          (string-trim (shell-command-to-string
           (concat
            "dirname "
            filename)))))))

(defun look-for-ledger-schedule-file ()
  "See if there is a file in the same directory as this ledger file with the same basename and a \".rec\" extenxtion. If so, set the `ledger-schedule-file variable' to this file for the local buffer."
  (if (not (string= (buffer-name) ledger-schedule-buffer-name))
      (set-local-variable
       'ledger-schedule-file
       (replace-regexp-in-string
        "\\.\\(ledger\\|ldg\\)" ".rec" (buffer-file-name) nil 'literal))))

(add-hook 'ledger-mode-hook #'look-for-ledger-schedule-file)


(defun org-to-tc ()
  "Convert the current org file into a timeclock file for ledger."
  (message "Saving timeclock file")
  (let ((cmdstr (concat "~/.emacs.d/bin/org2tc "
                        (buffer-file-name)
                        " > " (find-ledger-directory) "/"
                        (replace-regexp-in-string
                         (regexp-quote "\.org") ".timeclock" (buffer-name) nil 'literal))))
    (message cmdstr)
    (shell-command cmdstr)))


(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'org-to-tc nil 'local-please)))

(setq ledger-reports
      '(("asset/liabilities" "ledger -f %(ledger-file) bal assets liabilities")
        ("profit/loss" "ledger -f %(ledger-file) bal income expenses")
        ("checkbook" "ledger -f %(ledger-file) reg personal:assets:checking")
        ("cc" "ledger -f %(ledger-file) reg personal:liabilities and visa")
        ("loans" "ledger -f %(ledger-file) reg personal:liabilities and loan personal:expense and loan")
        ("bal" "ledger -f %(ledger-file) bal")
        ("reg" "ledger -f %(ledger-file) reg")
        ("payee" "ledger -f %(ledger-file) reg @%(payee)")
        ("account" "ledger -f %(ledger-file) reg %(account)")))



;;; ledger-mode-config.el ends here
