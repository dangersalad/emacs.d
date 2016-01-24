;;; avy-config --- Config for ELPA installed packaged
;;; Commentary:
;;;
;;; Config for avy package

;;; Code:

(require 'avy)

(setq avy-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
                    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z))

(avy-setup-default)
(global-set-key (kbd "C-;") 'avy-goto-char)

(global-set-key (kbd "C-c C-f") 'avy-goto-char-in-line)

(global-set-key (kbd "C-:") 'avy-goto-word-or-subword-1)

(global-set-key (kbd "C-'") 'avy-goto-char-2)

(global-set-key (kbd "C-\"") 'avy-goto-subword-1)

;;; avy-config.el ends here
