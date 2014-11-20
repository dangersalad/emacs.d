;;; js2-mode-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for js2-mode package

;;; Code:

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(js2-mode-hide-warnings-and-errors)     ; let flycheck handle this

(add-hook 'js-mode-hook 'js2-minor-mode)
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(diminish 'js2-minor-mode)

;;; js2-mode-config.el ends here
