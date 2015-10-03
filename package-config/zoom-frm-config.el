;;; zoom-frm-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for zoom-frm package

;;; Code:

(require 'zoom-frm)

(setq frame-zoom-font-difference 4)

;; zoom text in frames
(define-key ctl-x-map [(control ?+)] 'zoom-in/out)
(define-key ctl-x-map [(control ?=)] 'zoom-in/out)
(define-key ctl-x-map [(control ?-)] 'zoom-in/out)
(define-key ctl-x-map [(control ?0)] 'zoom-in/out)

;;; zoom-frm-config.el ends here
