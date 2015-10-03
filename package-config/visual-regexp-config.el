;;; visual-regexp-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for visual-regexp package

;;; Code:

(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c M-r") 'vr/query-replace)



;;; visual-regexp-config.el ends here
