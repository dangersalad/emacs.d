;;; flycheck-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for flycheck package

;;; Code:

(setq-default flycheck-emacs-lisp-load-path 'inherit)
(require 'flycheck)
(require 's)
;; enable flycheck everywhere
(add-hook 'after-init-hook #'global-flycheck-mode)

(defvar goflymake-dir (concat
                       (getenv "GOPATH")
                       "/src/github.com/dougm/goflymake"))

(message goflymake-dir)
(if (file-readable-p goflymake-dir)
    (message "it's there")
  (message "it's not there"))
(when (file-readable-p goflymake-dir)
  (message "Loading golang flycheck stuff")
  (add-to-list 'load-path goflymake-dir)
  (require 'go-flycheck)

  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c e"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
    flycheck-command-map))

;;; flycheck-config.el ends here
