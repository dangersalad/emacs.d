;;; linum-relative-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for linum-relative package

;;; Code:

(defvar linum-relative-current-symbol "")
(defvar linum-relative-format "%3s ")
(require 'linum-relative)
(global-linum-mode 1)
(global-set-key (kbd "C-x l") 'linum-relative-toggle)

;;; linum-relative.el ends here
