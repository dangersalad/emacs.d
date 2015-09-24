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
  (set-face-attribute 'mode-line nil :background zenburn-bg+1 :box nil)
  (set-face-attribute 'mode-line-inactive nil :foreground zenburn-bg+3 :background zenburn-bg+05 :box nil)
  (set-face-attribute 'powerline-active1 nil :background zenburn-bg+05 :foreground zenburn-green+1)
  (set-face-attribute 'powerline-active2 nil :background zenburn-bg+1 :foreground zenburn-green+1)
  (set-face-attribute 'powerline-inactive1 nil :background zenburn-bg+05 :foreground zenburn-bg+3)
  (set-face-attribute 'powerline-inactive2 nil :background zenburn-bg+05 :foreground zenburn-bg+3)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'flycheck-error nil :underline '(:style line))
  (set-face-attribute 'flycheck-warning nil :underline '(:style line))
  (set-face-attribute 'flycheck-info nil :underline '(:style line))
  (set-face-attribute 'helm-source-header nil :background zenburn-bg+2 :height 1.3 :box '(:style nil))
  (set-face-attribute 'helm-selection nil :background zenburn-bg+2 :weight 'bold)
  (set-face-attribute 'helm-selection-line nil :background zenburn-bg+2)

  (set-face-attribute 'flycheck-color-mode-line-error-face nil :background zenburn-red-2 :foreground zenburn-bg-1)
  (set-face-attribute 'flycheck-color-mode-line-warning-face nil :background zenburn-yellow-4 :foreground zenburn-bg-1)
  (set-face-attribute 'flycheck-color-mode-line-info-face nil :background zenburn-blue-2 :foreground zenburn-bg-1)

  )


;;; zenburn-theme-config.el ends here
