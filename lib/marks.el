;;; marks --- Mark functions
;;; Commentary:
;;; making marks work better with ransient mark mode

;;; Code:

;; mark without selecting
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-,") 'push-mark-no-activate)


(defun jump-to-mark ()
  "Jumps to the local mark, respecting `mark-ring' order.
This is the same as \\[set-mark-command] with the prefix argument"
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(provide 'marks)
;;; marks.el ends here
