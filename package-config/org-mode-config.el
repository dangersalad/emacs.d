;;; org-mode-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for org-mode package

;;; Code:

(condition-case nil
    (make-directory "~/org/journal" t) ; make the org and journal dirs if they are not there already
  (error nil))
(setq org-journal-dir "~/org/journal/")

(defun org-agenda-reload ()
  "Reset org agenda files by rescanning the org directory."
  (interactive)
  (setq org-agenda-files (directory-files "~/org" t "^[^.]")))

(org-agenda-reload)
(setq org-agenda-file-regexp "\\([^.].*\\.org\\)\\|\\([0-9]+\\)")
;; keybindings
(define-key global-map "\C-ca" 'org-agenda)
(require 'org)
(setq org-log-done 'time)


;;; org-mode-config.el ends here
