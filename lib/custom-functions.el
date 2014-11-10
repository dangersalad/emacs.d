;;; custom-functions -- Line opening like vim for emacs
;;; Commentary:
;;; Behave like vi's o command
;;;
;;; if newline-and-indent is true, follow indent rules
;;; when opening lines
;;; C-o opens line below
;;; M-o opens line above
;;; Code:

(defun directory-files-recursive(directory &optional match)
  "Get all files in DIRECTORY recursivley.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 Otherwise, the list returned is sorted with `string-lessp'.
 NOSORT is useful if you plan to sort the result yourself."
  (interactive)
  (let (file-list
        (current-dir-list (directory-files-and-attributes directory t))
        (match (if match match "^[^.].*"))) ; ignore hidden files by default
    (while current-dir-list
      (let ((file-name (car (car current-dir-list)))
            (is-dir (car (cdr (car current-dir-list)))))
        (cond
         ;; if the filename matches the match string
         (is-dir
          ;; make sure it is not a hidden dir
          (if (or
               (equal "." (substring file-name -1))
               (equal "." (substring (file-name-nondirectory file-name) 0 1)))
              (message "skipping %s" file-name)
            ;; recurse it adding the result to the list
            (setq file-list
                  (append
                   (directory-files-recursive file-name match)
                   file-list))))
         ((string-match match (file-name-nondirectory file-name))
          (setq file-list (cons file-name file-list)))))
      (setq current-dir-list (cdr current-dir-list)))
    file-list))

;; function for fixing format errors in a file
(defun clean-whitespace-region (start end)
  "Untabify, remove trailing whitespace, and re-indent a region from START to END."
  (interactive "r")
  (save-excursion
    (untabify start end)
    (indent-according-to-mode)
    (delete-trailing-whitespace)))


(provide 'custom-functions)

;;; custom-functions.el ends here
