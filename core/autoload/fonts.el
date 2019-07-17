;;; core/autoload/fonts.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar enfer-font-increment 2
  "How many steps to increase the font size each time `enfer/increase-font-size'
or `enfer/decrease-font-size' are invoked.")

;;;###autoload
(defvar enfer-big-font nil
  "The font to use for `enfer-big-font-mode'. If nil, `enfer-font' will be used,
scaled up by `enfer-big-font-increment'. See `enfer-font' for details on
acceptable values for this variable.")

;;;###autoload
(defvar enfer-big-font-increment 8
  "How many steps to increase the font size (with `enfer-font' as the base) when
`enfer-big-font-mode' is enabled and `enfer-big-font' is nil.")

;;;###autoload
(defvar enfer-change-font-size-hook nil
  "A hook run after adjusting the font size with `enfer/increase-font-size',
`enfer/decrease-font-size', or `enfer/reset-font-size'.")


;;
;;; Library

(defun enfer--font-name (fontname frame)
  (when (query-fontset fontname)
    (when-let (ascii (assq 'ascii (aref (fontset-info fontname frame) 2)))
      (setq fontname (nth 2 ascii))))
  (or (x-decompose-font-name fontname)
      (error "Cannot decompose font name")))

(defun enfer--frame-list (&optional frame)
  "Return a list consisting of FRAME and all of FRAME's child frames."
  (let ((frame (or frame (selected-frame))))
    (cons (selected-frame)
          (cl-loop for fr in (frame-list)
                   if (eq (frame-parameter fr 'parent-frame) frame)
                   collect fr))))

;;;###autoload
(defun enfer-adjust-font-size (increment &optional frame)
  "Increase size of font in FRAME by INCREMENT.
Interactively, INCREMENT is given by the prefix argument.
Optional FRAME parameter defaults to current frame."
  (interactive "p")
  (let* ((frame (or frame (selected-frame)))
         (font (frame-parameter frame 'font))
         (font (enfer--font-name font frame)))
    (let ((new-size (+ (string-to-number (aref font xlfd-regexp-pixelsize-subnum))
                       increment)))
      (unless (> new-size 0)
        (error "Font is to small at %d" new-size))
      (aset font xlfd-regexp-pixelsize-subnum (number-to-string new-size)))
    ;; Set point size & width to "*", so frame width will adjust to new font size
    (aset font xlfd-regexp-pointsize-subnum "*")
    (aset font xlfd-regexp-avgwidth-subnum "*")
    (setq font (x-compose-font-name font))
    (unless (x-list-fonts font)
      (error "Cannot change font size"))
    (dolist (fr (enfer--frame-list frame))
      (modify-frame-parameters fr `((font . ,font))))))


;;
;;; Commands

;;;###autoload
(defun enfer/increase-font-size (count)
  "Enlargens the font size across the current frame."
  (interactive "p")
  (let ((zoom-factor (or (frame-parameter nil 'font-scale) 0))
        (increment (* count enfer-font-increment)))
    (setq zoom-factor (+ zoom-factor increment))
    (if (= zoom-factor 0)
        (enfer/reset-font-size)
      (enfer-adjust-font-size increment)
      (modify-frame-parameters nil `((font-scale . ,zoom-factor)))
      (run-hooks 'enfer-change-font-size-hook))))

;;;###autoload
(defun enfer/decrease-font-size (count)
  "Shrinks the font size across the current frame."
  (interactive "p")
  (enfer/increase-font-size (- count)))

;;;###autoload
(defun enfer/reset-font-size ()
  "Reset font size.

Assuming it has been adjusted via `enfer/increase-font-size' and
`enfer/decrease-font-size'."
  (interactive)
  (let ((zoom-factor (frame-parameter nil 'font-scale)))
    (if (not zoom-factor)
        (user-error "Font size hasn't been changed")
      (set-frame-font enfer-font t (enfer--frame-list))
      (modify-frame-parameters nil '((font-scale)))
      (run-hooks 'enfer-change-font-size-hook))))

;;;###autoload
(define-minor-mode enfer-big-font-mode
  "A global mode that resizes the font, for streams, screen-sharing and
presentations.

This uses `enfer/increase-font-size' under the hood, and enlargens the font by
`enfer-big-font-increment'."
  :init-value nil
  :lighter " BIG"
  :global t
  (unless enfer-font
    (user-error "`enfer-font' must be set to a valid font"))
  (let ((frame (selected-frame)))
    (if enfer-big-font
        (progn
          (set-frame-font (if enfer-big-font-mode enfer-big-font enfer-font)
                          t (list frame))
          (run-hooks 'enfer-change-font-size-hook))
      (set-frame-font enfer-font t (enfer--frame-list frame))
      (when enfer-big-font-mode
        (enfer-adjust-font-size enfer-big-font-increment frame)))))
