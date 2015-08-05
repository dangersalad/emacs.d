;;; helm-projectile-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for helm-projectile package

;;; Code:


(require 'helm-projectile)

;; use helm for al possible projectile commangs
(helm-projectile-toggle 1)

(setq helm-projectile-fuzzy-match t)

;;; helm-projectile-config.el ends here
