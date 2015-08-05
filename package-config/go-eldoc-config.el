;;; go-eldoc-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for go-eldoc package

;;; Code:

(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)

(set-face-attribute 'eldoc-highlight-function-argument nil
                    :foreground "blue"
                    :weight 'bold)

;;; go-eldoc-config.el ends here
