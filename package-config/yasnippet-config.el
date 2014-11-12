;;; yasnippet-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for yasnippet package

;;; Code:

(defvar my-snippets (concat my-local-dir "/snippets") "Local, non git controlled snippets.")
(make-directory my-snippets t)
(setq yas-snippet-dirs
      (append yas-snippet-dirs
              `(,my-snippets))) ;add local snippets
(add-hook 'git-commit-mode-hook
          '(lambda ()
             (yas-minor-mode)))
(yas-reload-all)

;;; yasnippet-config.el ends here
