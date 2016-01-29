;;; haskell-mode-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for haskell-mode package

;;; Code:

(require 'haskell-mode)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(add-hook 'haskell-mode-hook (lambda ()
                               (ghc-init)
                               (haskell-indent-mode)))

;;; haskell-mode-config.el ends here
