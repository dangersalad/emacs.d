;;; unit-file-mode --- Major mode for editing systemd unit files
;;; Commentary:
;;;
;;; a work in progress

;;; Code:

(define-generic-mode 'unit-file-mode
  '("#")
  '("Unit" "Service" "Install" "Timer")
  '(("=" . 'font-lock-operator)
    ("^[A-Z][a-zA-Z]+" . 'font-lock-variable-name-face))
  '("\\.service"
    "\\.timer"
    "\\.target")
  nil
  "A mode for systemd unit files")

;;; unit-file-mode.el ends here
