;;; zenburn-colors --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Customs zenburn colors

;;; Code:

(defvar zenburn-colors-alist
  '(("zenburn-fg+1"     . "#FFFFEF")
    ("zenburn-fg"       . "#DCDCCC")
    ("zenburn-fg-1"     . "#656555")
    ("zenburn-bg-2"     . "#000000")
    ("zenburn-bg-1"     . "#0C0C0C")
    ("zenburn-bg-05"    . "#121212")
    ("zenburn-bg"       . "#1C1C1C")
    ("zenburn-bg+05"    . "#222222")
    ("zenburn-bg+1"     . "#2C2C2C")
    ("zenburn-bg+2"     . "#3C3C3C")
    ("zenburn-bg+3"     . "#4C4C4C")
    ("zenburn-red+1"    . "#DCA3A3")
    ("zenburn-red"      . "#CC9393")
    ("zenburn-red-1"    . "#BC8383")
    ("zenburn-red-2"    . "#AC7373")
    ("zenburn-red-3"    . "#9C6363")
    ("zenburn-red-4"    . "#8C5353")
    ("zenburn-orange"   . "#DFAF8F")
    ("zenburn-yellow"   . "#F0DFAF")
    ("zenburn-yellow-1" . "#E0CF9F")
    ("zenburn-yellow-2" . "#D0BF8F")
    ("zenburn-yellow-4" . "#B09F6F")
    ("zenburn-green-2"  . "#4F6F4F")
    ("zenburn-green-1"  . "#5F7F5F")
    ("zenburn-green"    . "#7F9F7F")
    ("zenburn-green+1"  . "#8FB28F")
    ("zenburn-green+2"  . "#9FC59F")
    ("zenburn-green+3"  . "#AFD8AF")
    ("zenburn-green+4"  . "#BFEBBF")
    ("zenburn-cyan"     . "#93E0E3")
    ("zenburn-blue+1"   . "#94BFF3")
    ("zenburn-blue"     . "#8CD0D3")
    ("zenburn-blue-1"   . "#7CB8BB")
    ("zenburn-blue-2"   . "#6CA0A3")
    ("zenburn-blue-3"   . "#5C888B")
    ("zenburn-blue-4"   . "#4C7073")
    ("zenburn-blue-5"   . "#366060")
    ("zenburn-magenta"  . "#DC8CC3"))
  "List of Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.

This overrides the colors provided by the `zenburn-theme' package.")

(provide 'zenburn-colors)

;;; zenburn-colors.el ends here
