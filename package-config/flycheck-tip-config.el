;;; flycheck-tip-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for flycheck-tip package

;;; Code:

(defvar error-tip-timer-delay nil
  "Whether how much delay showing error popup.
If you set nil to this variable, then do not use delay timer.")

(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)
(setq error-tip-notify-keep-messages t)
(setq flycheck-tip-avoid-show-func nil)

;;; flycheck-tip-config.el ends here
