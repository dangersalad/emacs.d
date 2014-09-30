;;; indentation --- Indentation settings
;;; Commentary:
;;; Indentation settings
;;; Spaces instead of tabs

;;; Code:
;; auto indent
(electric-indent-mode 1)

;; ignore for python
(defun electric-indent-ignore-python (char)
  "Ignore Electric Indent for Python, CHAR is ignored."
  (if (or
       (equal major-mode 'python-mode)
       (equal major-mode 'markdown-mode)
       (equal major-mode 'org-mode)
       (equal major-mode 'org-journal-mode))
      `no-indent'
    t))
(add-to-list 'electric-indent-functions 'electric-indent-ignore-python)

(defun set-newline-and-indent ()
  "Map RET key to `newline-and-indent'."
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'markdown-mode-hook 'set-newline-and-indent)

;; fuck tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))

;; 4 space tabs for html
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 4)))

(provide 'indentation)
;;; indentation.el ends here
