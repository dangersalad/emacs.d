;;; check-dpi --- Auto byte compile elisp
;;; Commentary:
;;;
;;; Auto compiles Emacs Lisp when a buffer is saved
;;; only if a compile file already exists

;;; Code:

(defun check-dpi ()
  "Return the dpi scaling."
  (if (and (< (/ (display-pixel-height) (float (display-mm-height))) 3.8)
         (< (/ (display-pixel-width) (float (display-mm-width))) 3.8))
      'medium
    'low))


(defun check-dpi-low-p ()
  "Is low dpi screen."
  (eq 'low (check-dpi)))

(defun check-dpi-medium-p ()
  "Is medium dpi screen."
  (eq 'medium (check-dpi)))

(provide 'check-dpi)

;;; check-dpi.el ends here
