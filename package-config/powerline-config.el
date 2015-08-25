;;; powerline-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for powerline package

;;; Code:

(require 'powerline)

;; make the separator dissapear
(defmacro pl/nothing (dir)
  "Generate a bar XPM function for DIR."
  (pl/pattern-defun "nothing" dir 1
                    '((1 1))))
(pl/memoize (pl/nothing right))
(pl/memoize (pl/nothing left))
(setq powerline-default-separator 'nothing)

(require 'local-powerline-themes)
(my-powerline-theme)

;;; powerline-config.el ends here
