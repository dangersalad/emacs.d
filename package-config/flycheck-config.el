;;; flycheck-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for flycheck package

;;; Code:

(require 'flycheck)
;; enable flycheck everywhere
(add-hook 'after-init-hook #'global-flycheck-mode)

(flycheck-define-checker go-build
                         "A Go syntax and type checker using the `go build' command.

See URL `https://golang.org/cmd/go'."
                         :command ("go" "build" "-o" "/dev/null" source-inplace)
                         :error-patterns
                         ((error line-start (file-name) ":" line ":"
                                 (optional column ":")" " (message) line-end))
                         :modes go-mode
                         :predicate
                         (lambda ()
                           (and (buffer-file-name)
                                (not (s-ends-with? "_test.go" (buffer-file-name))))))



;;; flycheck-config.el ends here
