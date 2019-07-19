;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit)
  (package! forge)
  (package! magit-gitflow)
  (package! magit-todos)
  (package! git-gutter)
  (package! evil-magit))
  
