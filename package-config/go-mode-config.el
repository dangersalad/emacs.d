;;; go-mode-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for go-mode package

;;; Code:

(require 'go-mode)

(defun my-go-hook ()
  "Hook for go-mode."
  ;; call gofmt for every save
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; customize the compile command
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test && go vet")))

(add-hook 'go-mode-hook 'my-go-hook)
;;; go-mode-config.el ends here
