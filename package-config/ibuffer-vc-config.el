;;; ibuffer-vc-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for ibuffer-vc package

;;; Code:

(require 'ibuffer-vc)

(add-hook 'ibuffer-hook
          (lambda ()
            (setq ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root))
            (add-to-list 'ibuffer-filter-groups
                         '("Folders" (mode . dired-mode)) t)
            (let ((ibuf (get-buffer "*Ibuffer*")))
              (when ibuf
                (with-current-buffer ibuf
                  (pop-to-buffer ibuf)
                  (ibuffer-update nil t))))
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

;;; ibuffer-vc-config.el ends here
