;;; line-opening -- Line opening like vim for emacs
;;; Commentary:
;;; Behave like vi's o command
;;;
;;; if newline-and-indent is true, follow indent rules
;;; when opening lines
;;; C-o opens line below
;;; M-o opens line above
;;; Code:

(defvar newline-and-indent t "Make line openings use auto indent.")

(defun open-next-line (count)
        "Open COUNT lines after the current one.

See also `newline-and-indent'."
        (interactive "p")
        (end-of-line)
        (open-line count)
        (forward-line count)
        (when newline-and-indent
          (indent-according-to-mode)))
;; Behave like vi's O command
(defun open-previous-line (count)
        "Open COUNT new line before the current one.

See also `newline-and-indent'."
        (interactive "p")
        (beginning-of-line)
        (open-line count)
        (when newline-and-indent
          (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

(provide 'line-opening)

;;; line-opening.el ends here
