;;; org-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for org-mode package

;;; Code:

(defvar org-directory "~/org" "Directory for org files.")
(defvar org-mobile-directory "~/.org-mobile" "Directory for mobile org files.")
(setq org-journal-dir (concat org-directory "/journal/"))
(require 'org)
(require 'org-agenda)

(condition-case nil
    (make-directory org-journal-dir t) ; make the org and journal dirs if they are not there already
  (error nil))
(condition-case nil
    (make-directory org-mobile-directory t) ; make the org and journal dirs if they are not there already
  (error nil))

(defun org-agenda-reload ()
  "Reset org agenda files by rescanning the org directory."
  (interactive)
  (setq org-agenda-files (directory-files-recursive org-directory))
  (setq org-refile-targets '((org-agenda-files . (:level . 1)))))

(org-agenda-reload)
(setq org-agenda-file-regexp "\\([^.].*\\.org\\)\\|\\([0-9]+\\)")
;; keybindings
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)

(setq org-log-done 'time)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-catch-invisible-edits t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING(w@)" "|" "WILL-NOT-IMPLEMENT(k@)" "DONE(d)")
        (sequence "BUG(b)" "RESOLVING(r!)" "|" "NON-ISSUE(n@)" "PATCHED(p)")))

;; defaut capture file
(setq org-default-notes-file (concat org-directory "/todo.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Todo") "* TODO %? %^{Schedule}t\n  %A")
        ("n" "Note" entry (file+headline (concat org-directory "/notes.org") "Notes") "* %? %U\n  %i")))


;;; org-config.el ends here
