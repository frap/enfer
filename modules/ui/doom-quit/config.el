;;; ui/enfer-quit/config.el -*- lexical-binding: t; -*-

(defvar +enfer-quit-messages
  '(;; from Enfer 1
    "Please don't leave, there's more demons to toast!"
    "Let's beat it -- This is turning into a bloodbath!"
    "I wouldn't leave if I were you. DOS is much worse."
    "Ne pars pas encore - il y a un démon au coin de la rue!"
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    "Are you sure you want to quit this great editor?"
    ;; Custom
    "(setq nothing t everything 'permitted)"
    "Emacs will remember that."
    "Emacs, Emacs never changes."
    "Hey! Hey, M-x listen!"
    "Je suis l'homme qui va incendier votre maison! Avec des citrons!"
    "It's not like I'll miss you or anything, b-baka!"
    "Okay, look. We've both said a lot of things you're going to regret..."
    "Wake up, Mr. Stallman. Wake up and smell the ashes."
    "You are *not* prepared!")
  "A list of quit messages, picked randomly by `+enfer-quit'. Taken from
http://enfer.wikia.com/wiki/Quit_messages and elsewhere.")

(defun +enfer-quit (&rest _)
  (enfer-quit-p
   (format "%s  Quitter?"
           (nth (random (length +enfer-quit-messages))
                +enfer-quit-messages))))

;;
(setq confirm-kill-emacs #'+enfer-quit)
