;;; package-loader --- Load packages from list
;;; Commentary:
;;; Loads packages in a list from MELPA

;;; Code:
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; package installer
(eval-when-compile
  (require 'cl))

(defvar my-packages '())

(defun packages-installed-p ()
  "Check installed packages."
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; if there are missing packages, install them
(unless (packages-installed-p)
  ;; check for new packages
  (message "%s" "Refreshing packages")
  (package-refresh-contents)
  (message "%s" "done")
  ;; install missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(loop for package-name in my-packages
      do (let ((package-file (concat "~/.emacs.d/package-config/" (symbol-name package-name) "-config.el")))
           (if (file-readable-p package-file)
             (progn (message "loading config") (load package-file))
             (progn (message "requiring package") (require package-name)))))

(provide 'package-loader)

;;; package-loader.el ends here
