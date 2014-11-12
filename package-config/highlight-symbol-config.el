;;; highlight-symbol-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for highlight-symbol package

;;; Code:

(require 'highlight-symbol)
(highlight-symbol-nav-mode)

(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'org-mode-hook (lambda () (highlight-symbol-mode)))

(setq highlight-symbol-idle-delay 0.2
      highlight-symbol-on-navigation-p t)

(global-set-key (kbd "M-n") 'highlight-symbol-next-in-defun)
(global-set-key (kbd "M-p") 'highlight-symbol-prev-in-defun)

;;; highlight-symbol-config.el ends here
