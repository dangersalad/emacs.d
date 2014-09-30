;;; popup-terminal --- A popup terminal (ansi term)
;;; Commentary:
;;; uses /usr/bin/zsh by default
;;; (setq popup-terminal-command "/path/to/shell") to change

;;; Code:

(defvar popup-terminal-buffer nil)
(defvar popup-terminal-command "/usr/bin/zsh")

(defun popup-terminal ()
  "Toggle the popup `ansi-term' instance."
  (interactive)
  (unless (buffer-live-p popup-terminal-buffer)
    (save-window-excursion (ansi-term "/usr/bin/zsh" "Popup Shell"))
    (setq popup-terminal-buffer (get-buffer "*Popup Shell*")))
  (let ((win (get-buffer-window popup-terminal-buffer)))
    (if win
        (quit-window nil win)
      (pop-to-buffer popup-terminal-buffer nil t))))

(global-set-key (kbd "<f11>") 'popup-terminal)

(provide 'popup-terminal)
;;; popup-terminal.el ends here
