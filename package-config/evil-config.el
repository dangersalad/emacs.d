;;; evil-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for evil-mode package

;;; Code:

(defvar evil-want-C-u-scroll t)

(require 'evil)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))

;; toggle vim bindings with C-f1
(global-set-key (kbd "<f12>") 'evil-local-mode)


;;; evil-config.el ends here
