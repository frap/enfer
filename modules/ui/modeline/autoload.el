;;; ui/modeline/autoload/modeline.el -*- lexical-binding: t; -*-

;;;###autodef
(defalias 'def-modeline-format! #'enfer-modeline-def-modeline)

;;;###autodef
(defalias 'def-modeline-segment! #'enfer-modeline-def-segment)

;;;###autodef
(defalias 'set-modeline! #'enfer-modeline-set-modeline)


(defvar +modeline--old-bar-height nil)
;;;###autoload
(defun +modeline|resize-for-font ()
  "Adjust the modeline's height when the font size is changed by
`enfer/increase-font-size' or `enfer/decrease-font-size'.

Meant for `enfer-change-font-size-hook'."
  (unless +modeline--old-bar-height
    (setq +modeline--old-bar-height enfer-modeline-height))
  (let ((default-height +modeline--old-bar-height)
        (scale (or (frame-parameter nil 'font-scale) 0)))
    (if (> scale 0)
        (let* ((font-size (string-to-number
                           (aref (enfer--font-name (frame-parameter nil 'font)
                                                  (selected-frame))
                                 xlfd-regexp-pixelsize-subnum)))
               (scale (frame-parameter nil 'font-scale)))
          (setq enfer-modeline-height (+ default-height (* scale enfer-font-increment))))
      (setq enfer-modeline-height default-height))
    ;; already has a variable watcher in Emacs 26+
    (unless EMACS26+ (enfer-modeline-refresh-bars))))

;;;###autoload
(defun +modeline|update-env-in-all-windows (&rest _)
  "Update version strings in all buffers."
  (dolist (window (window-list))
    (with-selected-window window
      (enfer-modeline-update-env)
      (force-mode-line-update))))

;;;###autoload
(defun +modeline|clear-env-in-all-windows (&rest _)
  "Blank out version strings in all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq enfer-modeline-env--version
            (bound-and-true-p enfer-modeline-load-string))))
  (force-mode-line-update t))
