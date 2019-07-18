;;; emacs/electric/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-electric! (modes &rest plist)
  "Declare that WORDS (list of strings) or CHARS (lists of chars) should trigger
electric indentation.

Enables `electric-indent-local-mode' in MODES.

\(fn MODES &key WORDS CHARS)"
  (declare (indent defun))
  (dolist (mode (enfer-enlist modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+electric|init-%s" mode))))
      (cond ((null (car-safe plist))
             (remove-hook hook fn)
             (unintern fn nil))
            ((fset fn
                   (lambda ()
                     (when (eq major-mode mode)
                       (setq-local electric-indent-inhibit nil)
                       (cl-destructuring-bind (&key chars words) plist
                         (electric-indent-local-mode +1)
                         (if chars (setq electric-indent-chars chars))
                         (if words (setq +electric-indent-words words))))))
             (add-hook hook fn))))))