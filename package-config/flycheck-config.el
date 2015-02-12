;;; flycheck-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for flycheck package

;;; Code:

(require 'flycheck)
(require 's)
;; enable flycheck everywhere
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-to-list 'load-path
             (concat
              (getenv "GOPATH")
              "/src/github.com/dougm/goflymake"))
(require 'go-flycheck)

(define-key flycheck-mode-map flycheck-keymap-prefix nil)
(setq flycheck-keymap-prefix (kbd "C-c e"))
(define-key flycheck-mode-map flycheck-keymap-prefix
  flycheck-command-map)

;;; flycheck-config.el ends here
