;;; macros --- My keyboard macros
;;; Commentary:
;;;
;;; Config for package

;;; Code:

(fset 'js-log-debug
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote
          ("logger.debug(\"q;" 0 "%d"))
         arg)))

;;; macros.el ends here
