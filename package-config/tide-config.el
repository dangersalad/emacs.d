;;; tide-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for tide package

;;; Code:

(require 'typescript-mode)
(require 'tide)


(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (tide-mode)))

;;; tide-config.el ends here
