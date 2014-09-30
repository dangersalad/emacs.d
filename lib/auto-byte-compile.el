;;; auto-byte-compile --- Auto byte compile elisp
;;; Commentary:
;;;
;;; Auto compiles Emacs Lisp when a buffer is saved
;;; only if a compile file already exists

;;; Code:

(defun byte-compile-current-buffer ()
  "\"byte-compile\" current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(provide 'auto-byte-compile)

;;; auto-byte-compile.el ends here
