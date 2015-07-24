;;; ac-helm-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for ac-helm package

;;; Code:

(require 'ac-helm)
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)


;;; ac-helm-config.el ends here
