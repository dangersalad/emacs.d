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

(fset 'ng-add-dep
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote
          ([39 21 24 113 134217730 2 6 201326624 134217847 19 41 2 44 32 25 18 39 6 44 32] 0 "%d"))
         arg)))


(fset 'js-hide-funcs
      [?\C-s ?f ?u ?n ?c ?t ?i ?o ?n return ?\C-e ?\C-c ?@ ?\C-c ?\C-e])


;;; macros.el ends here
