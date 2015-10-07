;;; erc-settings --- ERC Settings
;;; Commentary:
;;;
;;; put setting for nick and password in the local/init.el file
;;; along with any auto join channels
;;;
;;; Code:

(defvar erc-hide-list '("JOIN" "PART" "QUIT"))
(defvar erc-auto-discard-away t)
(defvar erc-auto-away-message "I have floated off into the ether.")
(require 'erc)
(setq erc-modules '(autoaway
                    pcomplete
                    netsplit
                    fill
                    button
                    match
                    track
                    completion
                    networks
                    ring
                    autojoin
                    noncommands
                    irccontrols
                    move-to-prompt
                    stamp
                    menu
                    list))

(erc-update-modules)

(setq erc-prompt (lambda () (concat (buffer-name) ">")))

(defadvice erc-display-prompt (after conversation-erc-display-prompt activate)
  "Insert last recipient after prompt."
  (let ((previous
         (save-excursion
           (if (and (search-backward-regexp (concat "^[^<]*<" erc-nick ">") nil t)
                    (search-forward-regexp (concat "^[^<]*<" erc-nick ">"
                                                   " *\\([^: ]*: ?\\)") nil t))
               (match-string 1)))))
    ;; when we got something, and it was in the last 3 mins, put it in
    (when (and
           previous
           (> 180 (time-to-seconds
                   (time-since (get-text-property 0 'timestamp previous)))))
      (set-text-properties 0 (length previous) nil previous)
      (insert previous))))

(setq erc-button-url-regexp
      (concat "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+"
              "[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]"))


(defvar erc-responded-once nil)
(defvar erc-away-reason nil)
(defun erc-respond-once-if-away (match-type nickuserhost msg)
  "Respond once if mentioned while away.
MATCH-TYPE NICKUSERHOST MSG"
  (if (erc-away-time)
      (if (eq match-type 'current-nick)
          (unless erc-responded-once
            (erc-send-action (erc-default-target) (concat "is away: " erc-away-reason))
            (setq erc-responded-once t)))))
(add-hook 'erc-text-matched-hook 'erc-respond-once-if-away)

(defadvice erc-process-away (after erc-away-reason-clear (proc away-p) activate)
  "Clear things."
  (unless away-p
    (setq erc-responded-once nil
          erc-away-reason nil)))

(defadvice erc-cmd-AWAY (after erc-store-reason (line) activate)
  "Store line."
  (when (string-match "^\\s-*\\(.*\\)$" line)
    (let ((reason (match-string 1 line)))
      (setq erc-away-reason reason))))

(defadvice erc-cmd-GAWAY (after erc-store-reason (line) activate)
  "Store line."
  (when (string-match "^\\s-*\\(.*\\)$" line)
    (let ((reason (match-string 1 line)))
      (setq erc-away-reason reason))))


(defvar erc-available-p t)
(defvar erc-x-timeout 600)
(defvar erc-away-status erc-auto-away-message)

(defun erc-global-away (msg)
  "Set all erc buffer to away with message MSG."
  (interactive "sAway Message: ")
  (setq erc-available-p nil)
  (erc-cmd-GAWAY msg)
  (setq erc-away-reason msg)
  (message "Set as away: %s" msg))

(defun erc-global-return ()
  "Set all erc buffer to return."
  (interactive)
  (setq erc-available-p t)
  (erc-cmd-GAWAY "")
  (setq erc-away-reason nil)
  (message "Set as back"))


(when (executable-find "xprintidle")
  (defun erc-x-timeout-away ()
    "Set away message to erc-away-status and clear it if already away."
    (interactive)
    (if (> (/
            (string-to-number (shell-command-to-string (executable-find "xprintidle")))
            1000.0)
           erc-x-timeout)
        (when erc-available-p
          (erc-global-away erc-away-status)
          (setq erc-available-p nil))
      (unless erc-available-p
        (erc-global-return)
        (setq erc-available-p t))))
  (run-at-time t 60 'erc-x-timeout-away))


(defun my-erc-notify (match-type nickuserhost message)
  "Play a sound based on match from ERC buffers using MATCH-TYPE, NICKUSERHOST, and MESSAGE."
  (unless (string-match "Server:[0-9]+" nickuserhost)
    (cond
     ((eq match-type 'current-nick)
      (start-process-shell-command
       "erc-notify" nil
       (concat
        "~/.emacs.d/bin/erc-notify '" (symbol-name match-type) "' '" nickuserhost "' '" message "' 2>>~/erc-notifier.error.log"))))))

(add-hook 'erc-text-matched-hook 'my-erc-notify)

(provide 'erc-settings)

;;; erc-settings.el ends here
