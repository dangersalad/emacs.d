;;; email-config --- Config for email
;;; Commentary:
;;
;; Configuration for mu4e and related things
;; example email-settings.el file, minimum it needs it "(provide 'email-settings)"
;;
;; (setq mu4e-maildir "~/mail"
;;       mu4e-drafts-folder "/accountname/Drafts"
;;       mu4e-sent-folder   "/accountname/Sent"
;;       mu4e-trash-folder  "/accountname/Trash"
;;       mu4e-update-interval 180
;;       message-send-mail-function 'smtpmail-send-it
;;       smtpmail-stream-type 'starttls
;;       message-kill-buffer-on-exit t
;;       mu4e-user-mail-address-list '("paul@dangersalad.com"))
;;
;; ;; multiple accounts can go here to pick different smtp servers
;; (defvar my-mu4e-account-alist
;;   `(("accountname"
;;      (mu4e-drafts-folder ,mu4e-drafts-folder)
;;      (mu4e-sent-folder   ,mu4e-sent-folder)
;;      (mu4e-trash-folder  ,mu4e-trash-folder)
;;      (user-mail-address  "user@mailserver.tld")
;;      (smtpmail-default-smtp-server "smtp.mailserver.tld")
;;      (smtpmail-smtp-server "smtp.mailserver.tld")
;;      (smtpmail-smtp-service 587)))
;;   "List of account mu4e accounts to fill in the emails-settings file.")
;;
;;; Code:

(require 'mu4e nil t)
(require 'mu4e-contrib nil t)
(require 'email-settings nil t)
(require 'smtpmail)
(require 'gnus-dired)
(require 'shr)
(require 'shr-color)

(when (and (require 'mu4e) (require 'email-settings))
  ;; this file must be put here after git checkout

  (setq mail-user-agent 'mu4e-user-agent
        mu4e-get-mail-command "~/.emacs.d/bin/email-fetch"
        mu4e-compose-dont-reply-to-self t)

  (add-to-list 'mu4e-bookmarks
               '("flag:flagged" "Flagged" ?f))
  (add-to-list 'mu4e-bookmarks
               '("date:2d..now" "Past 2 days" ?2))
  (add-to-list 'mu4e-bookmarks
               '("date:7d..now AND NOT flag:trashed AND NOT maildir:sent" "Recent Inbox" ?r))

  (defun my-mu4e-set-account ()
    "Set the account for composing a message."
    (let* ((account
            (if mu4e-compose-parent-message
                (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                  (string-match "/\\(.*?\\)/" maildir)
                  (match-string 1 maildir))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                               (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                               nil t nil nil (caar my-mu4e-account-alist))))
           (account-vars (cdr (assoc account my-mu4e-account-alist))))
      (if account-vars
          (mapc #'(lambda (var)
                    (set (car var) (cadr var)))
                account-vars)
        (error "No email account found"))))

  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
  (setq mu4e-view-html-plaintext-ratio-heuristic 15)
  (setq mu4e-view-show-images nil)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (setq mu4e-view-prefer-html nil)
  ;; shr html parser settings
  (setq shr-inhibit-decoration t
        shr-color-visible-luminance-min 80
        shr-color-visible-distance-min 15)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq mail-user-agent 'mu4e-user-agent)

  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))


(provide 'email-config)

;;; email-config.el ends here
