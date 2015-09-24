;;; org-trello-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for org-trello package

;;; Code:

(require 'org-trello)

(eval-after-load 'org-trello
  (setq org-trello-current-prefix-keybinding "C-c o"))

;;; org-trello-config.el ends here
