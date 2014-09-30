;;; set-local-variable --- Set a local variable in one step

;;; Commentary:

;;;Allows makeing a variable local to the buffer and
;;; setting it's value in one step

;;; Code:
(defun set-local-variable (varname value)
  "Make a variable VARNAME local to the buffer if needed, then set to VALUE."
  (interactive "vVariable Name: \nsNew Value: ")
  (let  ((number (string-to-number value)))
  (make-variable-buffer-local varname)
  (if (and (= 0 number) (not (string-equal "0" value)))
      (set-variable varname value)
    (set-variable varname number))))

(provide 'set-local-variable)
;;; set-local-variable.el ends here
