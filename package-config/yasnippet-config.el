;;; yasnippet-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for yasnippet package

;;; Code:

(setq yas-snippet-dirs
      (append yas-snippet-dirs
              '("~/.emacs.d/local/snippets"))) ;add local snippets
(add-hook 'git-commit-mode-hook
          '(lambda ()
             (yas-minor-mode)))
(yas-reload-all)

;;; yasnippet-config.el ends here
