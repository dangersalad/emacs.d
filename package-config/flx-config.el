;;; flx-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for flx package

;;; Code:


(require 'flx)
(require 'helm-lib nil t)

(with-eval-after-load 'helm
  ;; this is a bit hackish, ATM, redefining functions I don't own
  (defvar helm-flx-cache (flx-make-string-cache #'flx-get-heatmap-str))

  (defun helm-score-candidate-for-pattern (candidate pattern)
    (or (car (flx-score candidate pattern helm-flx-cache)) 0))

  (defun helm-fuzzy-default-highlight-match (candidate)
    (let* ((pair (and (consp candidate) candidate))
           (display (if pair (car pair) candidate))
           (real (cdr pair)))
      (with-temp-buffer
        (insert display)
        (goto-char (point-min))
        (if (string-match-p " " helm-pattern)
            (cl-loop with pattern = (split-string helm-pattern)
                     for p in pattern
                     do (when (search-forward p nil t)
                          (add-text-properties
                           (match-beginning 0) (match-end 0) '(face helm-match))))
          (cl-loop with pattern = (cdr (flx-score display
                                                  helm-pattern helm-flx-cache))
                   for index in pattern
                   do (add-text-properties
                       (1+ index) (+ 2 index) '(face helm-match))))
        (setq display (buffer-string)))
      (if real (cons display real) display))))


;;; flx-config.el ends here
