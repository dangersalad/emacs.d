;;; rust-mode-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for rust-mode package

;;; Code:

(require 'rust-mode)
(require 'company)
(require 'racer)
(require 'rust-mode)
(require 'eldoc)
(require 'flycheck)
(require 'flycheck-rust)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook  #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook  #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook
          '(lambda ()
             (setq racer-rust-src-path (concat (getenv "HOME") "/.rust-dev/rust/src"))
             (setq company-tooltip-align-annotations t)
             (rust-enable-format-on-save)
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))

;;; rust-mode-config.el ends here
