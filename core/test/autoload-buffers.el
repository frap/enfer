;; -*- no-byte-compile: t; -*-
;;; core/test/autoload-buffers.el

(defmacro with-temp-buffers!! (buffer-args &rest body)
  (declare (indent defun))
  (let (buffers)
    (dolist (bsym buffer-args)
      (push `(,bsym (get-buffer-create ,(symbol-name bsym)))
            buffers))
    `(cl-flet ((buffer-list
                (lambda ()
                  (cl-remove-if-not #'buffer-live-p (list ,@(reverse (mapcar #'car buffers)))))))
       (let* (persp-mode
              ,@buffers)
         ,@body
         (mapc #'kill-buffer (buffer-list))))))

;;
(def-test! get-buffers
  (with-temp-buffers!! (a b c)
    (should (cl-every #'buffer-live-p (buffer-list)))
    (should (equal (buffer-list) (list a b c)))
    (dolist (buf (list (cons a enfer-emacs-dir)
                       (cons b enfer-emacs-dir)
                       (cons c "/tmp/")))
      (with-current-buffer (car buf)
        (setq-local default-directory (cdr buf))))
    (projectile-mode +1)
    (with-current-buffer a
      ;; should produce all buffers
      (let ((buffers (enfer-buffer-list)))
        (should (cl-every (lambda (x) (memq x buffers)) (list a b c))))
      ;; should produce only project buffers
      (let ((buffers (enfer-project-buffer-list)))
        (should (cl-every (lambda (x) (memq x buffers)) (list a b)))
        (should-not (memq c buffers))))
    ;; If no project is available, just get all buffers
    (with-current-buffer c
      (let ((buffers (enfer-project-buffer-list)))
        (should (cl-every (lambda (x) (memq x buffers)) (list a b c)))))
    (projectile-mode -1)))

(def-test! real-buffers
  (let (enfer-real-buffer-functions)
    (with-temp-buffers!! (a b c d)
      (dolist (buf (list a b))
        (with-current-buffer buf
          (setq-local buffer-file-name "x")))
      (with-current-buffer c
        (rename-buffer "*C*"))
      (with-current-buffer d
        (enfer-popup-mode +1))
      (should (enfer-real-buffer-p a))
      (should (enfer-real-buffer-p b))
      (should-not (enfer-real-buffer-p c))
      (should-not (enfer-real-buffer-p d))
      (let ((buffers (enfer-real-buffer-list)))
        (should (= (length buffers) 2))
        (should (cl-every  (lambda (x) (memq x buffers)) (list a b)))
        (should (cl-notany (lambda (x) (memq x buffers)) (list c d)))))))

;; `enfer-visible-windows'
;; `enfer-visible-buffers'
;; `enfer-buried-buffers'
(def-test! visible-buffers-and-windows
  (with-temp-buffers!! (a b c d)
    (switch-to-buffer a)
    (should (eq (current-buffer) a))
    (should (eq (selected-window) (get-buffer-window a)))
    (split-window nil 1)
    (switch-to-buffer b)
    (should (eq (current-buffer) b))
    (should (eq (selected-window) (get-buffer-window b)))
    (should (cl-intersection (list a b) (enfer-visible-buffers)))
    (should (cl-intersection (list c d) (enfer-buried-buffers)))
    (should (cl-intersection (mapcar #'get-buffer-window (list a b))
                             (enfer-visible-windows)))))

;; `enfer-matching-buffers'
(def-test! matching-buffers
  (with-temp-buffers!! (a b c)
    (let ((buffers (enfer-matching-buffers "^[ac]$")))
      (should (= 2 (length buffers)))
      (should (cl-every #'bufferp buffers))
      (should (cl-every (lambda (x) (memq x buffers)) (list a c)))
      (should (equal buffers (enfer-matching-buffers "^[ac]$"))))))

;; `enfer-buffers-in-mode'
(def-test! buffers-in-mode
  (with-temp-buffers!! (a b c d e)
    (dolist (buf (list a b))
      (with-current-buffer buf
        (emacs-lisp-mode)))
    (dolist (buf (list c d e))
      (with-current-buffer buf
        (text-mode)))
    (let ((el-buffers  (enfer-buffers-in-mode 'emacs-lisp-mode))
          (txt-buffers (enfer-buffers-in-mode 'text-mode)))
      (should (cl-every #'buffer-live-p (append el-buffers txt-buffers)))
      (should (= 2 (length el-buffers)))
      (should (= 3 (length txt-buffers))))))

;; `enfer-kill-buffer'
(def-test! kill-buffer
  (with-temp-buffers!! (a b)
    (enfer-kill-buffer a)
    (should-not (buffer-live-p a))
    ;; modified buffer
    (with-current-buffer b
      (set-buffer-modified-p t))
    (enfer-kill-buffer b t)
    (should-not (buffer-live-p a))))

;; `enfer--cycle-real-buffers'
(def-test! kill-buffer-then-show-real-buffer
  (with-temp-buffers!! (a b c d)
    (dolist (buf (list a b d))
      (with-current-buffer buf
        (setq-local buffer-file-name "x")))
    (should (cl-every #'buffer-live-p (buffer-list)))
    (switch-to-buffer a)
    (should (eq (current-buffer) a))
    (should (eq (selected-window) (get-buffer-window a)))
    (should (enfer-kill-buffer a))
    ;; eventually end up in the fallback buffer
    (let ((fallback (enfer-fallback-buffer)))
      (while (not (eq (current-buffer) fallback))
        (should (enfer-real-buffer-p))
        (enfer-kill-buffer))
      (should (eq (current-buffer) fallback)))))

;; TODO enfer/kill-all-buffers
;; TODO enfer/kill-other-buffers
;; TODO enfer/kill-matching-buffers
;; TODO enfer/cleanup-session
