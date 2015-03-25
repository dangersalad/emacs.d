;;; powerline-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for powerline package

;;; Code:

(defface powerline-active1 '((t (:background "grey22" :inherit mode-line)))
  "Powerline face 1."
  :group 'powerline)
(defface powerline-active2 '((t (:background "grey27" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)
(defface powerline-inactive1
  '((t (:background "grey25" :foreground "grey11" :italic :inherit mode-line-inactive)))
  "Powerline face 1."
  :group 'powerline)
(defface powerline-inactive2
  '((t (:background "grey25" :foreground "grey11" :italic :inherit mode-line-inactive)))
  "Powerline face 2."
  :group 'powerline)

(require 'powerline)
(setq powerline-default-separator 'arrow-fade)
(powerline-default-theme)
;;; powerline-config.el ends here
