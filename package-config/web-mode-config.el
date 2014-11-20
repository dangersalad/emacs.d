;;; web-mode-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for web-mode package

;;; Code:

(require 'web-mode)
(defun web-mode-auto ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4))


(add-hook 'web-mode-hook #'web-mode-auto)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(setq web-mode-engines-alist '(("erb" . "\\.ejs\\'") ("html" . "\\.html\\'")))
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-quoting nil)
(setq web-mode-enable-auto-opening t)

;;; web-mode-config.el ends here
