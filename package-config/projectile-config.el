;;; projectile-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for projectile package

;;; Code:

(require 'grep)
(require 'projectile)
(projectile-global-mode)
(setq projectile-remember-window-configs t)

(define-key projectile-command-map (kbd "s s") 'helm-projectile-ag)

;;; projectile-config.el ends here
