;;; custom-functions -- Line opening like vim for emacs
;;; Commentary:
;;; Behave like vi's o command
;;;
;;; if newline-and-indent is true, follow indent rules
;;; when opening lines
;;; C-o opens line below
;;; M-o opens line above
;;; Code:

(require 'dired)

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
            (is-dir (equal t (car (cdr (car current-dir-list))))))
        (cond
         ;; if the filename matches the match string
         (is-dir
          ;; make sure it is not a hidden dir
          (if (or
               (equal "." (substring file-name -1))
               (equal "." (substring (file-name-nondirectory file-name) 0 1)))
              ()
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


(defmacro bol-with-prefix (function)
  "Define a wrapper for FUNCTION, move to beginning of line with prefix.
Moves to beginning of line before calling FUNCTION when called
with a prefix argument.  The FUNCTION still receives the prefix
argument."
  (let ((name (intern (format "endless/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

(defun my-dired-exec-last-macro ()
  "Execute the last macro across all marked files in dired."
  (interactive)
  (dolist (file (dired-get-marked-files nil nil 'dired-nondirectory-p))
    (let ((buffer (find-file file)))
      (kmacro-end-and-call-macro 1))))


(defun indent-buffer ()
  "Indent entire buffer using `indent-according-to-mode'."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-region (region-beginning) (region-end))))

(defvar enlarge-window-char ?+)
(defvar enlarge-window-char-alt ?=)
(defvar shrink-window-char ?-)
(defvar shrink-window-char-alt ?-)
(defvar enlarge-window-horizontally-char ?\])
(defvar shrink-window-horizontally-char ?\[)

;; window resizing things


(defun resize-window (&optional arg)
  "Interactively resize the selected window.
Repeatedly prompt whether to enlarge or shrink the window until
the response is neither `enlarge-window-char' or
`shrink-window-char'.  When called with a prefix arg, resize the
window by ARG lines."
  (interactive "p")
  (let ((prompt (format "Enlarge/Shrink window: vertical %c/%c horizontal %c/%c"
                        enlarge-window-char shrink-window-char
                        enlarge-window-horizontally-char shrink-window-horizontally-char))
        response)
    (while (progn
             (setq response (read-event prompt))
             (cond
              ((or (equal response enlarge-window-char) (equal response enlarge-window-char-alt))
               (enlarge-window arg) t)
              ((or (equal response shrink-window-char) (equal response shrink-window-char-alt))
               (enlarge-window (- arg)) t)
              ((equal response enlarge-window-horizontally-char)
               (enlarge-window-horizontally arg) t)
              ((equal response shrink-window-horizontally-char)
               (shrink-window-horizontally arg) t)
              (t nil))))
    (push response unread-command-events)))


(defun win-resize-top-or-bot (window)
  "Figure out if the WINDOW is on top, bottom or in the middle."
  (let* ((win-edges (window-edges window))
         (this-window-y-min (nth 1 win-edges))
         (this-window-y-max (nth 3 win-edges))
         (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right (window)
  "Figure out if the WINDOW is to the left, right or in the middle."
  (let* ((win-edges (window-edges window))
         (this-window-x-min (nth 0 win-edges))
         (this-window-x-max (nth 2 win-edges))
         (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-toggle-vert-split ()
  "Toggle a vertical window split between two windows."
  (interactive)
  (let* ((this-window (get-buffer-window))
         (win-edges (window-edges this-window))
         (top-or-bot (win-resize-top-or-bot this-window)))
    (cond ((equal top-or-bot "top")
           (select-window (window-at (nth 0 win-edges) (+ (nth 3 win-edges) 2)))
           (message (buffer-name))
           (enlarge-window 100))
          ((equal top-or-bot "bot")
           (message (buffer-name))
           (select-window (window-at (nth 0 win-edges) (- (nth 1 win-edges) 2)))
           (enlarge-window 100))
          (t (message "passthrough")))))

(defun win-equalize-vert ()
  "Equalize two windows vertically."
  (interactive)
  (let* ((this-window (get-buffer-window))
         (win-edges (window-edges this-window))
         (top-or-bot (win-resize-top-or-bot this-window))
         (other-win)
         (other-win-edges)
         (max-y)
         (current-y)
         (win-to-resize))
    (cond ((equal top-or-bot "top")
           (setq other-win (window-at (nth 0 win-edges) (+ (nth 3 win-edges) 2)))
           (setq win-to-resize this-window)
           (setq other-win-edges (window-edges other-win))
           (setq max-y (nth 3 other-win-edges))
           (setq current-y (nth 3 win-edges)))
          ((equal top-or-bot "bot")
           (setq other-win (window-at (nth 0 win-edges) (- (nth 1 win-edges) 2)))
           (setq win-to-resize other-win)
           (setq other-win-edges (window-edges other-win))
           (setq max-y (nth 3 win-edges))
           (setq current-y (nth 3 other-win-edges)))
          (t (message "passthrough")))
    (adjust-window-trailing-edge win-to-resize (- (/ max-y 2) current-y))))




(provide 'custom-functions)

;;; custom-functions.el ends here
