;; -*- no-byte-compile: t; -*-
;;; core/test/test-autoload-buffers.el

(require 'core-projects)
(load! "autoload/buffers" enfer-core-dir)

;;
(describe "core/autoload/buffers"
  :var (a b c d)
  (before-all
    (spy-on 'buffer-list :and-call-fake
            (lambda (&optional _)
              (cl-remove-if-not #'buffer-live-p (list a b c d)))))
  (before-each
    (delete-other-windows)
    (setq a (switch-to-buffer (get-buffer-create "a"))
          b (get-buffer-create "b")
          c (get-buffer-create "c")
          d (get-buffer-create "d")))
  (after-each
    (kill-buffer a)
    (kill-buffer b)
    (kill-buffer c)
    (kill-buffer d))

  (describe "buffer-list"
    (it "should only see four buffers"
      (expect (enfer-buffer-list) :to-have-same-items-as (list a b c d))))

  (describe "project-buffer-list"
    :var (projectile-projects-cache-time projectile-projects-cache)
    (before-all (require 'projectile))
    (after-all  (unload-feature 'projectile t))

    (before-each
      (with-current-buffer a (setq default-directory enfer-emacs-dir))
      (with-current-buffer b (setq default-directory enfer-emacs-dir))
      (with-current-buffer c (setq default-directory "/tmp/"))
      (with-current-buffer d (setq default-directory "~"))
      (projectile-mode +1))
    (after-each
      (projectile-mode -1))

    (it "returns buffers in the same project"
      (with-current-buffer a
        (expect (enfer-project-buffer-list)
                :to-have-same-items-as (list a b))))

    (it "returns all buffers if not in a project"
      (with-current-buffer c
        (expect (enfer-project-buffer-list)
                :to-have-same-items-as (buffer-list)))))

  (describe "fallback-buffer"
    (it "returns a live buffer"
      (expect (buffer-live-p (enfer-fallback-buffer)))))

  (describe "real buffers"
    (before-each
      (enfer-set-buffer-real a t)
      (with-current-buffer b (setq buffer-file-name "x"))
      (with-current-buffer c (rename-buffer "*C*")))

    (describe "real-buffer-p"
      (it "returns t for buffers manually marked real"
        (expect (enfer-real-buffer-p a)))
      (it "returns t for file-visiting buffers"
        (expect (enfer-real-buffer-p b)))
      (it "returns nil for temporary buffers"
        (expect (enfer-real-buffer-p c) :to-be nil)
        (expect (enfer-real-buffer-p d) :to-be nil)))

    (describe "real-buffer-list"
      (it "returns only real buffers"
        (expect (enfer-real-buffer-list) :to-have-same-items-as (list a b)))))

  (describe "buffer/window management"
    (describe "buffer search methods"
      (before-each
        (with-current-buffer a (lisp-mode))
        (with-current-buffer b (text-mode))
        (with-current-buffer c (text-mode))
        (split-window)
        (switch-to-buffer b))

      (it "can match buffers by regexp"
        (expect (enfer-matching-buffers "^[ac]$") :to-have-same-items-as (list a c)))
      (it "can match buffers by major-mode"
        (expect (enfer-buffers-in-mode 'text-mode) :to-have-same-items-as (list b c)))
      (it "can find all buried buffers"
        (expect (enfer-buried-buffers)
                :to-have-same-items-as (list c d)))
      (it "can find all visible buffers"
        (expect (enfer-visible-buffers)
                :to-have-same-items-as (list a b)))
      (it "can find all visible windows"
        (expect (enfer-visible-windows)
                :to-have-same-items-as
                (mapcar #'get-buffer-window (list a b)))))

    (describe "kill-buffer-and-windows"
      (before-each
        (split-window) (switch-to-buffer b)
        (split-window) (switch-to-buffer a))

      (it "kills the selected buffers and all its windows"
        (enfer-kill-buffer-and-windows a)
        (expect (buffer-live-p a) :to-be nil)
        (expect (length (enfer-visible-windows)) :to-be 1)))

    ;; TODO
    (describe "kill-all-buffers")
    (describe "kill-other-buffers")
    (describe "kill-matching-buffers")
    (describe "cleanup-session")))
