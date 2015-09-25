;;; powerline-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for powerline package

;;; Code:

(require 'local-powerline-themes)
(require 'powerline)
(require 'evil)

;; make the separator dissapear
(defmacro pl/nothing (dir)
  "Generate a bar XPM function for DIR."
  (pl/pattern-defun "nothing" dir 1
                    '((1 1))))


(setq powerline-height 20)

(pl/memoize (pl/nothing right))
(pl/memoize (pl/nothing left))
(setq powerline-default-separator 'nothing)

(my-powerline-theme)

;;; powerline-config.el ends here
