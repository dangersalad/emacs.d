;;; tide-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for tide package

;;; Code:

(require 'typescript-mode)
(require 'tide)


(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (tide-mode)))

;; (require 'web-mode)
;; (require 'company)
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (tide-setup)
;;               (flycheck-mode +1)
;;               (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;               (eldoc-mode +1)
;;               (company-mode-on))))

;;; tide-config.el ends here
