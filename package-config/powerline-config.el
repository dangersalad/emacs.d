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


(defun adjust-powerline-to-font ()
  "Set the powerline height to a padded height of the current char size."
  (interactive)
  (setq powerline-height (+ 2 (frame-char-height)))
  (my-powerline-theme))

(pl/memoize (pl/nothing right))
(pl/memoize (pl/nothing left))
(setq powerline-default-separator 'nothing)

(adjust-powerline-to-font)
(my-powerline-theme)

;;; powerline-config.el ends here
