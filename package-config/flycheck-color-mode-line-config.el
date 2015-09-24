;;; flycheck-color-mode-line-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for flycheck-color-mode-line package

;;; Code:

(defface flycheck-color-mode-line-error-face
  '((t))
  "Face for the modeline in buffers with Flycheck errors."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-warning-face
  '((t))
  "Face for the modeline in buffers with only Flycheck warnings."
  :group 'flycheck-faces)

(defface flycheck-color-mode-line-info-face
  '((t))
  "Face for the modeline in buffers with only Flycheck info."
  :group 'flycheck-faces)

(require 'flycheck-color-mode-line)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;;; flycheck-color-mode-line-config.el ends here
