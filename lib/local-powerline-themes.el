;;; local-powerline-themes.el --- Themes for Powerline
;; Copyright (C) 2012-2013 Donald Ephraim Curtis
;; Copyright (C) 2013 Jason Milkins
;; Copyright (C) 2012 Nicolas Rougier
;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; URL: http://github.com/milkypostman/powerline/
;; Version: 2.0
;; Keywords: mode-line
;;; Commentary:
;;
;; Themes for Powerline.
;; Included themes: default, center, center-evil, vim, and nano.
;;
;;; Code:

(require 'zenburn-colors)
(require 'zenburn-theme)
(require 'powerline)
(require 'flycheck)
(require 'evil)

(defcustom powerline-evil-tag-style 'verbose
  "The style to use for displaying the evil state tag.
Valid Values: standard, verbose, visual-expanded"
  :group 'powerline
  :type '(choice (const standard)
                 (const verbose)
                 (const visual-expanded)))

(zenburn-with-color-variables
  (defface powerline-evil-base-face
    `((t (:foreground ,zenburn-bg-1 :inherit mode-line)))
    "Base face for powerline evil faces."
    :group 'powerline)
  (defface powerline-evil-normal-face
    `((t (:background ,zenburn-green-1 :inherit powerline-evil-base-face)))
    "Powerline face for evil NORMAL state."
    :group 'powerline)
  (defface powerline-evil-insert-face
    `((t (:background ,zenburn-red-1 :inherit powerline-evil-base-face)))
    "Powerline face for evil INSERT state."
    :group 'powerline)
  (defface powerline-evil-visual-face
    `((t (:background ,zenburn-orange :inherit powerline-evil-base-face)))
    "Powerline face for evil VISUAL state."
    :group 'powerline)
  (defface powerline-evil-operator-face
    `((t (:background ,zenburn-cyan :inherit powerline-evil-operator-face)))
    "Powerline face for evil OPERATOR state."
    :group 'powerline)
  (defface powerline-evil-replace-face
    `((t (:background ,zenburn-red :inherit powerline-evil-base-face)))
    "Powerline face for evil REPLACE state."
    :group 'powerline)
  (defface powerline-evil-motion-face
    `((t (:background ,zenburn-magenta :inherit powerline-evil-base-face)))
    "Powerline face for evil MOTION state."
    :group 'powerline)
  (defface powerline-evil-emacs-face
    `((t (:background ,zenburn-bg+2 :inherit powerline-evil-base-face)))
    "Powerline face for evil EMACS state."
    :group 'powerline)

  (defface powerline-hud1
    `((t (:background ,zenburn-fg-1 :inherit mode-line)))
    "Powerline HUD face 1."
    :group 'powerline)
  (defface powerline-hud2
    `((t (:background ,zenburn-bg :inherit mode-line)))
    "Powerline HUD face 1."
    :group 'powerline)
  (defface powerline-active1
    `((t (:background ,zenburn-bg+1 :inherit mode-line)))
    "Powerline face 1."
    :group 'powerline)
  (defface powerline-active2
    `((t (:background ,zenburn-bg+2 :inherit mode-line)))
    "Powerline face 2."
    :group 'powerline)
  (defface powerline-inactive1
    `((t (:background ,zenburn-bg-1 :foreground ,zenburn-bg+3 :italic :inherit mode-line-inactive)))
    "Powerline face 1."
    :group 'powerline)
  (defface powerline-inactive2
    `((t (:background ,zenburn-bg-05 :foreground ,zenburn-bg+3 :italic :inherit mode-line-inactive)))
    "Powerline face 2."
    :group 'powerline)

  (defface powerline-flycheck-base
    `((t (:foreground ,zenburn-bg-2 :inherit mode-line)))
    "Powerline flycheck face base."
    :group 'powerline)
  (defface powerline-flycheck-ok
    `((t (:background ,zenburn-green-2 :inherit powerline-flycheck-base)))
    "Powerline flycheck face for no errors."
    :group 'powerline)
  (defface powerline-flycheck-error
    `((t (:background ,zenburn-red-4 :inherit powerline-flycheck-base)))
    "Powerline flycheck face for errors."
    :group 'powerline)
  (defface powerline-flycheck-warning
    `((t (:background ,zenburn-yellow-4 :inherit powerline-flycheck-base)))
    "Powerline flycheck face for warnings."
    :group 'powerline)
  (defface powerline-flycheck-info
    `((t (:background ,zenburn-blue-5 :inherit powerline-flycheck-base)))
    "Powerline flycheck face for info."
    :group 'powerline)

  (defface powerline-flycheck-header-base
    `((t (:foreground ,zenburn-bg-2 :inherit header-line)))
    "Powerline flycheck face base."
    :group 'powerline)
  (defface powerline-flycheck-header-ok
    `((t (:background ,zenburn-green-2 :inherit powerline-flycheck-header-base)))
    "Powerline flycheck face for no errors."
    :group 'powerline)
  (defface powerline-flycheck-header-error
    `((t (:background ,zenburn-red-4 :inherit powerline-flycheck-header-base)))
    "Powerline flycheck face for errors."
    :group 'powerline)
  (defface powerline-flycheck-header-warning
    `((t (:background ,zenburn-yellow-4 :inherit powerline-flycheck-header-base)))
    "Powerline flycheck face for warnings."
    :group 'powerline)
  (defface powerline-flycheck-header-info
    `((t (:background ,zenburn-blue-5 :inherit powerline-flycheck-header-base)))
    "Powerline flycheck face for info."
    :group 'powerline))




;;;###autoload
(defun powerline-evil-face ()
  "Function to select appropriate face based on `evil-state'."
  (if (boundp 'evil-state)
      (let* ((face (intern (concat "powerline-evil-" (symbol-name evil-state) "-face"))))
        (if (facep face) face 'powerline-active2))
    'powerline-active2))

(defun powerline-evil-tag ()
  "Get customized tag value for current evil state."
  (let* ((visual-block (and (evil-visual-state-p)
                            (eq evil-visual-selection 'block)))
         (visual-line (and (evil-visual-state-p)
                           (eq evil-visual-selection 'line))))
    (cond ((eq powerline-evil-tag-style 'visual-expanded)
           (cond (visual-block " +V+ ")
                 (visual-line " -V- ")
                 (t evil-mode-line-tag)))
          ((eq powerline-evil-tag-style 'verbose)
           (upcase (concat (symbol-name evil-state)
                           (cond (visual-block " BLOCK")
                                 (visual-line " LINE")))))
          (t evil-mode-line-tag))))

(defun powerline-flycheck-face ()
  "Function to select appropriate face based on `flycheck-has-current-errors-p'."
  (if (bound-and-true-p flycheck-mode)
      (let* ((face (cond ((flycheck-has-current-errors-p 'error)
                          'powerline-flycheck-error)
                         ((flycheck-has-current-errors-p 'warning)
                          'powerline-flycheck-warning)
                         ((flycheck-has-current-errors-p 'info)
                          'powerline-flycheck-info))))
        (if (facep face) face 'powerline-flycheck-ok))
    'powerline-active2))

(defun powerline-flycheck-header-face ()
  "Function to select appropriate face based on `flycheck-has-current-errors-p'."
  (if (bound-and-true-p flycheck-mode)
      (let* ((face (cond ((flycheck-has-current-errors-p 'error)
                          'powerline-flycheck-header-error)
                         ((flycheck-has-current-errors-p 'warning)
                          'powerline-flycheck-header-warning)
                         ((flycheck-has-current-errors-p 'info)
                          'powerline-flycheck-header-info))))
        (if (facep face) face 'powerline-flycheck-header-ok))
    'powerline-active2))

(defun powerline-flycheck-tag ()
  "Get customized tag value for current flycheck state."
  (if (bound-and-true-p flycheck-mode)
      (let* ((tag (cond ((flycheck-has-current-errors-p 'error) "Error")
                        ((flycheck-has-current-errors-p 'warning) "Warning")
                        ((flycheck-has-current-errors-p 'info) "Info"))))
        (concat "  FlyC: " (if (stringp tag) tag "OK") "    "))
    ""))


;;;###autoload
(defun my-powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (hudface1 (if active 'powerline-hud1 'powerline-inactive1))
                          (hudface2 (if active 'powerline-hud2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (active-modes (mapc (lambda (mode) (condition-case nil
                                                                 (if (and (symbolp mode) (symbol-value mode))
                                                                     (add-to-list 'active-modes mode))
                                                               (error nil) ))
                                              minor-mode-list))
                          (evil-face (if active (powerline-evil-face) 'powerline-inactive1))
                          (flycheck-face (if active (powerline-flycheck-face) 'powerline-inactive1))
                          (lhs (list (powerline-raw "%*" flycheck-face 'l)
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size flycheck-face 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info flycheck-face 'l))
                                     (powerline-raw " " flycheck-face)
                                     (funcall separator-left flycheck-face nil)
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 flycheck-face)
                                     (powerline-raw (powerline-flycheck-tag) flycheck-face 'l)
                                     (funcall separator-left flycheck-face nil)
                                     (powerline-vc nil 'r)
                                     (funcall separator-left nil evil-face)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) evil-face 'l))
                                     (when (or (bound-and-true-p evil-mode)
                                               (bound-and-true-p evil-local-mode))
                                       (powerline-raw (powerline-evil-tag) evil-face 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right evil-face face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 flycheck-face)
                                     (powerline-raw " " flycheck-face)
                                     (powerline-raw "%p" flycheck-face 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill evil-face (powerline-width rhs))
                             (powerline-render rhs))))))
  (setq-default header-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (flycheck-face (if active (powerline-flycheck-header-face) 'powerline-inactive1))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (display (list (powerline-raw (powerline-flycheck-tag) flycheck-face 'c)))
                          (filename (list (powerline-raw " " nil)
                                          (powerline-raw (buffer-file-name) nil)
                                          (powerline-raw " " nil)))
                          (spacer-l (list (funcall separator-left flycheck-face flycheck-face)
                                          (powerline-fill-center flycheck-face 0)))
                          (spacer-r (powerline-fill flycheck-face 0)))
                     (concat (powerline-render filename)
                             (powerline-render spacer-l)
                             (powerline-render display)
                             spacer-r
                             ))))))


(provide 'local-powerline-themes)
;;; local-powerline-themes.el ends here
