;;; zenburn-theme-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for zenburn-theme package

;;; Code:

(require 'zenburn-colors)
(require 'zenburn-theme)
(load-theme 'zenburn t)


;; change some stuff
(zenburn-with-color-variables
  ;; flat mode line and better color
  (set-face-attribute 'region nil :background zenburn-bg-2)
  (set-face-attribute 'header-line nil :background zenburn-bg+1 :box nil)
  (set-face-attribute 'mode-line nil :background zenburn-bg+1 :box nil)
  (set-face-attribute 'mode-line-inactive nil :foreground zenburn-bg+3 :background zenburn-bg+05 :box nil)
  (set-face-attribute 'powerline-active1 nil :background zenburn-bg+05 :foreground zenburn-green+1)
  (set-face-attribute 'powerline-active2 nil :background zenburn-bg+1 :foreground zenburn-green+1)
  (set-face-attribute 'powerline-inactive1 nil :background zenburn-bg+05 :foreground zenburn-bg+3)
  (set-face-attribute 'powerline-inactive2 nil :background zenburn-bg+05 :foreground zenburn-bg+3)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'flycheck-error nil :underline `(:style line :color ,zenburn-red-1))
  (set-face-attribute 'flycheck-warning nil :underline `(:style line :color ,zenburn-yellow-2))
  (set-face-attribute 'flycheck-info nil :underline `(:style line :color ,zenburn-blue-2))
  (set-face-attribute 'helm-source-header nil :background zenburn-bg+2 :height 1.3 :box '(:style nil))
  (set-face-attribute 'helm-selection nil :background zenburn-bg+2 :weight 'bold)
  (set-face-attribute 'helm-selection-line nil :background zenburn-bg+2)


  (set-face-attribute 'ledger-font-auto-xact-face nil :foreground zenburn-yellow)

  (set-face-attribute 'ledger-font-periodic-xact-face nil :foreground zenburn-green+3)

  (set-face-attribute 'ledger-font-xact-cleared-face nil :foreground zenburn-fg)

  (set-face-attribute 'ledger-font-xact-pending-face nil :foreground zenburn-yellow-2)

  (set-face-attribute 'ledger-font-xact-open-face nil :foreground zenburn-bg-1)

  (set-face-attribute 'ledger-font-payee-uncleared-face nil :foreground zenburn-fg-1)

  (set-face-attribute 'ledger-font-payee-pending-face nil :foreground zenburn-yellow-2)

  (set-face-attribute 'ledger-font-pending-face nil :foreground zenburn-yellow-2)

  (set-face-attribute 'ledger-font-other-face nil :foreground zenburn-blue-1)

  (set-face-attribute 'ledger-font-posting-account-face nil :foreground zenburn-blue-3 )

  (set-face-attribute 'ledger-font-posting-amount-face nil :foreground zenburn-green+4 )

  (set-face-attribute 'ledger-font-posting-date-face nil :foreground zenburn-orange :underline t)

  (set-face-attribute 'ledger-font-report-clickable-face nil :foreground zenburn-fg+1)


  )


;;; zenburn-theme-config.el ends here
