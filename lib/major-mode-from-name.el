;;; major-mode-from-name -- Apply major mode selection logic to buffer names

;;; Commentary:

;;; When using a buffer not associated with a file, this will set the
;;; major mode based on the name of the buffer

;;; Code:

(defun major-mode-from-name ()
  "Choose proper mode for buffers created by `switch-to-buffer'."
  (if (not (string-match "^\*.*\*$" (buffer-name)))
      (let ((buffer-file-name (or buffer-file-name (buffer-name))))
        (set-auto-mode))))
(setq-default major-mode 'major-mode-from-name)

(provide 'major-mode-from-name)

;;; major-mode-from-name.el ends here
