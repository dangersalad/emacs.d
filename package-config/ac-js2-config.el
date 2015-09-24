;;; ac-js2-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for ac-js2 package

;;; Code:

(require 'ac-js2)
(setq ac-js2-evaluate-calls t)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;;; ac-js2-config.el ends here
