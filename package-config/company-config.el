;;; company-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for company package

;;; Code:

(require 'company)
(require 'diminish)

(add-hook 'after-init-hook 'global-company-mode)

(setq company-tooltip-limit 20) ; bigger popup window
(setq company-idle-delay .3)    ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)     ; remove annoying blinking

(diminish 'company-mode)


;;; company-config.el ends here
