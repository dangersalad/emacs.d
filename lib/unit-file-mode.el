;;; unit-file-mode --- Major mode for editing systemd unit files
;;; Commentary:
;;;
;;; a work in progress

;;; Code:

(define-generic-mode 'unit-file-mode
  '("#")
  '()
  '(("=" . 'font-lock-operator)
    ("\\[[A-Za-z-]+\\]" . 'font-lock-function-name-face)
    ("^[A-Z][a-zA-Z]+" . 'font-lock-variable-name-face))
  '("\\.service"
    "\\.timer"
    "\\.target")
  nil
  "A mode for systemd unit files")

;;; unit-file-mode.el ends here
