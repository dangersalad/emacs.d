;;; evil-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for evil-mode package

;;; Code:

(defvar evil-want-C-u-scroll t)
(require 'evil)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)


(evil-set-initial-state 'prog-mode 'normal)
(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'emacs-lisp-mode 'emacs)
(evil-set-initial-state 'fundamental-mode 'emacs)
(evil-mode)

;;; evil-config.el ends here
