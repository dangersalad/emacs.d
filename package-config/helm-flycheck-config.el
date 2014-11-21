;;; helm-flycheck-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for helm-flycheck package

;;; Code:

(require 'helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-command-map (kbd "l") 'helm-flycheck))

;;; helm-flycheck-config.el ends here
