;;; evil-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for evil-mode package

;;; Code:

(defvar evil-want-C-u-scroll t)
(require 'evil)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; toggle vim bindings with C-f1
(global-set-key (kbd "<f12>") 'evil-local-mode)


;;; evil-config.el ends here
