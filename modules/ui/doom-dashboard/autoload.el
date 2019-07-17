;;; ui/enfer-dashboard/autoload.el -*- lexical-binding: t; -*-

(defun +enfer-dashboard--help-echo ()
  (when-let* ((btn (button-at (point)))
              (msg (button-get btn 'help-echo)))
    (message "%s" msg)))

;;;###autoload
(defun +enfer-dashboard/open (frame)
  "Switch to the dashboard in the current window, of the current FRAME."
  (interactive (list (selected-frame)))
  (with-selected-frame frame
    (switch-to-buffer (enfer-fallback-buffer))
    (+enfer-dashboard-reload t)))

;;;###autoload
(defun +enfer-dashboard/forward-button (n)
  "Like `forward-button', but don't wrap."
  (interactive "p")
  (forward-button n nil)
  (+enfer-dashboard--help-echo))

;;;###autoload
(defun +enfer-dashboard/backward-button (n)
  "Like `backward-button', but don't wrap."
  (interactive "p")
  (backward-button n nil)
  (+enfer-dashboard--help-echo))
