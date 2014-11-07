;;; org-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for org-mode package

;;; Code:

(require 'org)
(require 'org-agenda)

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
(define-key global-map (kbd "C-c a") 'org-agenda)

(setq org-log-done 'time)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-catch-invisible-edits t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING" "|" "WILL-NOT-IMPLEMENT" "DONE(d)")
        (sequence "BUG(b)" "RESOLVING(r!)" "|" "NON-ISSUE" "PATCHED(p)")))

;;; org-config.el ends here
