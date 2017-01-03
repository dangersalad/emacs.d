;;; multiple-cursors-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for multiple-cursors package

;;; Code:

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)


;;; multiple-cursors-config.el ends here
