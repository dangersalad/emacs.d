;;; aggressive-indent-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for aggressive-indent package

;;; Code:
(require 'aggressive-indent)
(add-hook 'prog-mode-hook (lambda ()
                            (unless (equal major-mode "makefile-gmake-mode")
                              (aggressive-indent-mode))))

;;; aggressive-indent-config.el ends here
