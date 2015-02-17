;;; evil-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for evil-mode package

;;; Code:

(defvar evil-want-C-u-scroll t)

;; still use4 emacs bindings by default
(defvar evil-default-state 'emacs)

(require 'evil)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; use vim bindings for programming modes
(evil-set-initial-state 'prog-mode 'normal)

;; we need to specify this since elip is a sub mode of prog-mode
(evil-set-initial-state 'emacs-lisp-mode 'emacs)

(evil-mode)

;;; evil-config.el ends here
