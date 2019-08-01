;;; core-popups.el -*- lexical-binding: t; -*-

;; I want a "real"-buffer-first policy in my Emacsian utpoia; popup buffers
;; ought to be second-class citizens to "real" buffers. No need for a wall or
;; controversial immigration policies -- all we need is `shackle' (and it will
;; actually work).
;;
;; The gist is: popups should be displayed on one side of the frame, away from
;; 'real' buffers. They should be easy to dispose of when we don't want to see
;; them and easily brought back in case we change our minds. Also, popups should
;; typically have no mode-line.
;;
;; Be warned, this requires a lot of hackery voodoo that could break with an
;; emacs update or an update to any of the packages it tries to tame (like helm
;; or org-mode).

(defvar enfer-popup-history nil
  "A list of popups that were last closed. Used by `enfer/popup-restore' and
`enfer*popups-save'.")

(defvar enfer-popup-other-window nil
  "The last window selected before a popup was opened.")

(defvar enfer-popup-no-fringes t
  "If non-nil, disable fringes in popup windows.")

(defvar enfer-popup-windows ()
  "A list of open popup windows.")

(defvar-local enfer-popup-rules nil
  "The shackle rule that caused this buffer to be recognized as a popup. Don't
edit this directly.")
(put 'enfer-popup-rules 'permanent-local t)

(defvar enfer-popup-window-parameters
  '(:noesc :modeline :autokill :autoclose :autofit :static)
  "A list of window parameters that are set (and cleared) when `enfer-popup-mode
is enabled/disabled.'")

(defvar enfer-popup-remember-history t
  "Don't modify this directly. If non-nil, ENFER will remember the last popup(s)
that was/were open in `enfer-popup-history'.")

(defvar enfer-popup-inhibit-autokill nil
  "Don't modify this directly. When it is non-nil, no buffers will be killed
when their associated popup windows are closed, despite their :autokill
property.")

(defvar enfer-popup-mode-map (make-sparse-keymap)
  "Active keymap in popup windows.")


(def-setting! :popup (&rest rules)
  "Prepend a new popup rule to `shackle-rules' (see for format details).

Several custom properties have been added that are not part of shackle, but are
recognized by ENFER's popup system. They are:

:noesc      If non-nil, the popup won't be closed if you press ESC from *inside*
            its window. Used by `enfer/popup-close-maybe'.

:modeline   By default, mode-lines are hidden in popups unless this is non-nil.
            If it is a symbol, it'll use `enfer-modeline' to fetch a modeline
            config (in `enfer-popup-mode').

:autokill   If non-nil, the popup's buffer will be killed when the popup is
            closed. Used by `enfer*delete-popup-window'. NOTE
            `enfer/popup-restore' can't restore non-file popups that have an
            :autokill property.

:autoclose  If non-nil, close popup if ESC is pressed from outside the popup
            window.

:autofit    If non-nil, resize the popup to fit its content. Uses the value of
            the :size property as the maximum height/width. This will not work
            if the popup has no content when displayed.

:static     If non-nil, don't treat this window like a popup. This makes it
            impervious to being automatically closed or tracked in popup
            history. Excellent for permanent sidebars."
  (if (cl-every #'listp (mapcar #'enfer-unquote rules))
      `(setq shackle-rules (nconc (list ,@rules) shackle-rules))
    `(push (list ,@rules) shackle-rules)))


;;
;;
;;

;; (defvar enfer-popup-parameters
;;   '(:esc :modeline :transient :fit :align :size)
;;   "TODO")

;; (defvar enfer-popup-whitelist
;;   '(("^ ?\\*" :size 15 :noselect t :autokill t :autoclose t))
;;   "TODO")

(defvar enfer-popup-blacklist
  '("^\\*magit")
  "TODO")


;;
;; Bootstrap
;;

(def-package! shackle
  :init
  (setq shackle-default-alignment 'below
        shackle-default-size 8
        shackle-rules
        '(("^\\*eww" :regexp t :size 0.5 :select t :autokill t :noesc t)
          ("^\\*ftp " :noselect t :autokill t :noesc t)
          ;; enfer
          ("^\\*enfer:scratch" :regexp t :size 15 :noesc t :select t :modeline t :autokill t :static t)
          ("^\\*enfer:" :regexp t :size 0.35 :noesc t :select t)
          ("^ ?\\*enfer " :regexp t :noselect t :autokill t :autoclose t :autofit t)
          ;; built-in (emacs)
          ("*compilation*" :size 0.25 :noselect t :autokill t :autoclose t)
          ("*ert*" :same t :modeline t)
          ("*info*" :size 0.5 :select t :autokill t)
          ("*Backtrace*" :size 20 :noselect t)
          ("*Warnings*"  :size 12 :noselect t :autofit t)
          ("*Messages*"  :size 12 :noselect t)
          ("*Help*" :size 0.3 :autokill t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 20 :noselect t :autokill t)
          (apropos-mode :size 0.3 :autokill t :autoclose t)
          (Buffer-menu-mode :size 20 :autokill t)
          (comint-mode :noesc t)
          (grep-mode :size 25 :noselect t :autokill t)
          (profiler-report-mode :size 0.3 :regexp t :autokill t :modeline minimal)
          (tabulated-list-mode :noesc t)
          ("^ ?\\*" :regexp t :size 15 :noselect t :autokill t :autoclose t)))

  :config
  ;; NOTE This is a temporary fix while I rewrite core-popups
  (defun enfer-display-buffer-condition (buffer _action)
    (and (cl-loop for re in enfer-popup-blacklist
                  when (string-match-p re buffer)
                  return nil
                  finally return t)
         (shackle-match buffer)))

  (defun enfer-display-buffer-action (buffer alist)
    (shackle-display-buffer buffer alist (shackle-match buffer)))

  (defun enfer|autokill-popups ()
    (or (not (enfer-popup-p))
        (prog1 (when (and (not enfer-popup-inhibit-autokill)
                          (plist-get enfer-popup-rules :autokill))
                 (enfer-popup-mode -1)
                 (when-let* ((process (get-buffer-process (current-buffer))))
                   (set-process-query-on-exit-flag process nil))
                 t))))

  (add-hook! enfer-post-init
    (setq display-buffer-alist
          (cons '(enfer-display-buffer-condition enfer-display-buffer-action)
                display-buffer-alist))
    (add-hook 'kill-buffer-query-functions #'enfer|autokill-popups))

  ;; no modeline in popups
  (add-hook 'enfer-popup-mode-hook #'enfer|hide-modeline-in-popup)
  ;; ensure every rule without an :align, :same or :frame property has an
  ;; implicit :align (see `shackle-default-alignment')
  (advice-add #'shackle--match :filter-return #'enfer*shackle-always-align)

  ;; bootstrap popup system
  (advice-add #'shackle-display-buffer :around #'enfer*popup-init)
  (advice-add #'balance-windows :around #'enfer*popups-save)
  (advice-add #'delete-window :before #'enfer*delete-popup-window)

  ;; Tell `window-state-get' and `current-window-configuration' to recognize
  ;; these custom parameters. Helpful for `persp-mode' and persisting window
  ;; configs that have popups in them.
  (dolist (param `(popup ,@enfer-popup-window-parameters))
    (push (cons param 'writable) window-persistent-parameters))

  (let ((map enfer-popup-mode-map))
    (define-key map [escape]    #'enfer/popup-close-maybe)
    (define-key map (kbd "ESC") #'enfer/popup-close-maybe)
    (define-key map [remap quit-window] #'enfer/popup-close-maybe)
    (define-key map [remap enfer/kill-this-buffer] #'enfer/popup-close-maybe)
    (define-key map [remap split-window-right]              #'ignore)
    (define-key map [remap split-window-below]              #'ignore)
    (define-key map [remap split-window-horizontally]       #'ignore)
    (define-key map [remap split-window-vertically]         #'ignore)
    (define-key map [remap mouse-split-window-horizontally] #'ignore)
    (define-key map [remap mouse-split-window-vertically]   #'ignore)))

(def-package! bind-key
 ;;:disabled
  ;; A simple way to manage personal keybindings, provided by `use-package'
  :init
  ;; If non-nil, extract docstrings from lambdas, closures and keymaps if possible.
  (setq bind-key-describe-special-forms t)

  (defun my-keyboard-translations (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      (define-key input-decode-map (kbd "C-h") (kbd "<backspace>"))
      (define-key input-decode-map (kbd "M-h") (kbd "<M-backspace>"))))
  (add-to-list 'after-make-frame-functions 'my-keyboard-translations)

;;;; personal functions
  (defun my-switch-to-scratch () (interactive)
         (switch-to-buffer "*scratch*"))

  (defun my-kill-buffer () (interactive)
         (kill-buffer (buffer-name)))

  (defun my-select-prev-window () (interactive)
         (select-window (previous-window)))

  (defun my-select-next-window () (interactive)
         (select-window (next-window)))

  (defun my-indent-whole-buffer () (interactive)
         (indent-region (point-min) (point-max)))

  (defun my-split-window()
    "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
    (interactive)
    (if (eq last-command 'my-split-window)
        (progn
          (jump-to-register :my-split-window)
          (setq this-command 'my-unsplit-window))
      (window-configuration-to-register :my-split-window)
      (switch-to-buffer-other-window nil)))

  (defmacro my-package-desc (info-type library)
    `(,(intern (format "package-desc-%s" info-type))
      (with-temp-buffer
        (insert-file-contents-literally (find-library-name (format "%s" ,library)))
        (package-buffer-info))))

  (defun my-insert-package-desc-summary ()
    (interactive)
    (let* ((name (thing-at-point 'symbol t))
           (summary (my-package-desc summary name)))
      (back-to-indentation)
      (open-line 1)
      (insert (format ";; %s" summary))))

  (defun my-show-file-name ()
    "Show the full path file name in the minibuffer."
    (interactive)
    (message (buffer-file-name))
    (kill-new (file-truename buffer-file-name)))

  (defun my-toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1)))))))

(defun my-timestamp ()
    (interactive)
    (let ((timestring (if current-prefix-arg
                          "%H:%M"
                        "%a %d.%m.%y %H:%M")))
      (insert (format-time-string timestring))))

;;;; global key bindings
  :bind
  ("<f6>" . my-kill-buffer)
  ("<f7>" . my-toggle-window-split)
  ("C-8" . my-split-window)
  ("<f2>" . split-window-vertically)
  ("S-<f2>" . make-frame-command)
  ("<f3>" . split-window-horizontally)
  ("<f4>" . delete-window)
  ("S-<f4>" . delete-frame)
  ("<f5>" . delete-other-windows)
  ("S-<f5>" . delete-other-frames)
  ("C-c c" . (org-capture nil "s")))

;;
;; Hacks
;;

(progn ; hacks for built-in functions
  (defun enfer*suppress-pop-to-buffer-same-window (orig-fn &rest args)
    (cl-letf (((symbol-function 'pop-to-buffer-same-window)
               (symbol-function 'pop-to-buffer)))
      (apply orig-fn args)))
  (advice-add #'info :around #'enfer*suppress-pop-to-buffer-same-window)
  (advice-add #'eww :around #'enfer*suppress-pop-to-buffer-same-window)
  (advice-add #'eww-browse-url :around #'enfer*suppress-pop-to-buffer-same-window)

  (defun enfer*popup-buffer-menu (&optional arg)
    "Open `buffer-menu' in a popup window."
    (interactive "P")
    (with-selected-window (enfer-popup-buffer (list-buffers-noselect arg))
      (setq mode-line-format "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %; q to quit; ? for help.")))
  (advice-add #'buffer-menu :override #'enfer*popup-buffer-menu))


(after! comint
  (defun enfer|popup-close-comint-buffer ()
    (when (and (enfer-popup-p)
               (derived-mode-p 'comint-mode)
               (not (process-live-p (get-buffer-process (current-buffer)))))
      (delete-window)))
  ;;(add-hook '+evil-esc-hook #'enfer|popup-close-comint-buffer t)
  )


(after! eshell
  ;; By tying buffer life to its process, we ensure that we land back in the
  ;; eshell buffer after term dies. May cause problems with short-lived
  ;; processes.
  ;; FIXME replace with a 'kill buffer' keybinding.
  (setq eshell-destroy-buffer-when-process-dies t)

  ;; When eshell runs a visual command (see `eshell-visual-commands'), it spawns
  ;; a term buffer to run it in, but where it spawns it is the problem...
  (defun enfer*eshell-undedicate-popup (orig-fn &rest args)
    "Force spawned term buffer to share with the eshell popup (if necessary)."
    (when (enfer-popup-p)
      (set-window-dedicated-p nil nil)
      (add-transient-hook! #'eshell-query-kill-processes :after
        (set-window-dedicated-p nil t)))
    (apply orig-fn args))
  (advice-add #'eshell-exec-visual :around #'enfer*eshell-undedicate-popup))



(after! helm
  ;; Helm tries to clean up after itself, but shackle has already done this,
  ;; causing problems. This fixes that. To reproduce, add a helm rule in
  ;; `shackle-rules', open two splits side-by-side, move to the buffer on the
  ;; right and invoke helm. It will close all but the left-most buffer.
  (setq-default helm-reuse-last-window-split-state t
                helm-split-window-in-side-p t)

  (after! helm-swoop
    (setq helm-swoop-split-window-function #'pop-to-buffer))

  (after! helm-ag
    ;; This prevents helm-ag from switching between windows and buffers.
    (defun enfer*helm-ag-edit-done (orig-fn &rest args)
      (cl-letf (((symbol-function 'select-window) #'ignore))
        (apply orig-fn args))
      (enfer/popup-close))
    (advice-add #'helm-ag--edit-commit :around #'enfer*helm-ag-edit-done)
    (advice-add #'helm-ag--edit-abort  :around #'enfer*helm-ag-edit-done)

    (defun enfer*helm-ag-edit (orig-fn &rest args)
      (cl-letf (((symbol-function 'other-window) #'ignore)
                ((symbol-function 'switch-to-buffer) #'enfer-popup-buffer))
        (apply orig-fn args)
        (with-current-buffer (get-buffer "*helm-ag-edit*")
          (use-local-map helm-ag-edit-map))))
    (advice-add #'helm-ag--edit :around #'enfer*helm-ag-edit)))


(defsubst enfer--switch-from-popup (location)
  (enfer/popup-close)
  (switch-to-buffer (car location) nil t)
  (if (not (cdr location))
      (message "Unable to find location in file")
    (goto-char (cdr location))
    (recenter)))

(after! help-mode
  ;; Help buffers use `other-window' to decide where to open followed links,
  ;; which can be unpredictable. It should *only* replace the original buffer we
  ;; opened the popup from. To fix this these three button types need to be
  ;; redefined to set aside the popup before following a link.
  (define-button-type 'help-function-def
    :supertype 'help-xref
    'help-function
    (lambda (fun file)
      (require 'find-func)
      (when (eq file 'C-source)
        (setq file (help-C-file-name (indirect-function fun) 'fun)))
      (enfer--switch-from-popup (find-function-search-for-symbol fun nil file))))

  (define-button-type 'help-variable-def
    :supertype 'help-xref
    'help-function
    (lambda (var &optional file)
      (when (eq file 'C-source)
        (setq file (help-C-file-name var 'var)))
      (enfer--switch-from-popup (find-variable-noselect var file))))

  (define-button-type 'help-face-def
    :supertype 'help-xref
    'help-function
    (lambda (fun file)
      (require 'find-func)
      (enfer--switch-from-popup (find-function-search-for-symbol fun 'defface file)))))


(after! magit
  (add-hook 'magit-mode-hook #'enfer-hide-modeline-mode))


(after! mu4e
  (defun enfer*mu4e-popup-window (buf _height)
    (enfer-popup-buffer buf '(:size 10 :noselect t))
    buf)
  (advice-add #'mu4e~temp-window :override #'enfer*mu4e-popup-window))


(after! multi-term
  (setq multi-term-buffer-name "enfer:terminal"))


(after! neotree
  ;; Neotree has its own window/popup management built-in, which is difficult to
  ;; police. For example, switching perspectives will cause neotree to forget it
  ;; is a neotree pane.
  ;;
  ;; By handing neotree over to shackle, which is better integrated into the
  ;; rest of my config (and persp-mode), this is no longer a problem.
  (set! :popup " *NeoTree*" :align neo-window-position :size neo-window-width :static t)
)


(after! persp-mode
  (defun enfer*persp-mode-restore-popups (&rest _)
    "Restore popup windows when loading a perspective from file."
    (dolist (window (window-list))
      (when-let* ((plist (enfer-popup-properties window)))
        (with-selected-window window
          (unless enfer-popup-mode
            (setq-local enfer-popup-rules plist)
            (enfer-popup-mode +1))))))
  (advice-add #'persp-load-state-from-file :after #'enfer*persp-mode-restore-popups))


(after! quickrun
  ;; don't auto-focus quickrun windows, shackle handles that
  (setq quickrun-focus-p nil))


(after! twittering-mode
  (setq twittering-pop-to-buffer-function #'pop-to-buffer))


(after! wgrep
  ;; close the popup after you're done with a wgrep buffer
  (advice-add #'wgrep-abort-changes :after #'enfer/popup-close)
  (advice-add #'wgrep-finish-edit   :after #'enfer/popup-close))


(after! xref
  (defun enfer*xref-follow-and-close (orig-fn &rest args)
    "Jump to the xref on the current line, select its window and close the popup
you came from."
    (interactive)
    (let ((popup-p (enfer-popup-p))
          (window (selected-window)))
      (apply orig-fn args)
      (when popup-p (enfer/popup-close window))))
  (advice-add #'xref-goto-xref :around #'enfer*xref-follow-and-close))


;;
;; Major modes
;;

(after! plantuml-mode
  (defun enfer*plantuml-preview-in-popup-window (orig-fn &rest args)
    (save-window-excursion
      (apply orig-fn args))
    (pop-to-buffer plantuml-preview-buffer))
  (advice-add #'plantuml-preview-string
              :around #'enfer*plantuml-preview-in-popup-window))

;; Ensure these settings are loaded as late as possible, giving other modules a
;; chance to reconfigure org popup settings before the defaults kick in.
(defun enfer|init-org-popups ()
  (add-hook! org-load
    (set! :popup
      '("*Calendar*"         :size 0.4 :noselect t)
      '(" *Org todo*"        :size 5   :noselect t)
      '("*Org Note*"         :size 10)
      '("*Org Select*"       :size 20  :noselect t)
      '("*Org Links*"        :size 5   :noselect t)
      '("*Org Export Dispatcher*" :noselect t)
      '(" *Agenda Commands*" :noselect t)
      '("^\\*Org Agenda"     :regexp t :size 20)
      '("*Org Clock*"        :noselect t)
      '("^\\*Org Src"        :regexp t :size 0.35 :noesc t)
      '("*Edit Formulas*"    :size 10)
      '("^\\*Org-Babel"      :regexp t :size 25 :noselect t)
      '("^CAPTURE.*\\.org$"  :regexp t :size 20))

    ;; Org has a scorched-earth window management system I'm not fond of. i.e.
    ;; it kills all windows and monopolizes the frame. No thanks. We can do
    ;; better with shackle's help.
    (defun enfer*suppress-delete-other-windows (orig-fn &rest args)
      (cl-letf (((symbol-function 'delete-other-windows)
                 (symbol-function 'ignore)))
        (apply orig-fn args)))
    (advice-add #'org-add-log-note :around #'enfer*suppress-delete-other-windows)
    (advice-add #'org-capture-place-template :around #'enfer*suppress-delete-other-windows)
    (advice-add #'org-export--dispatch-ui :around #'enfer*suppress-delete-other-windows)

    ;; Hand off the src-block window to a shackle popup window.
    (defun enfer*org-src-pop-to-buffer (buffer _context)
      "Open the src-edit in a way that shackle can detect."
      (if (eq org-src-window-setup 'switch-invisibly)
          (set-buffer buffer)
        (pop-to-buffer buffer)))
    (advice-add #'org-src-switch-to-buffer :override #'enfer*org-src-pop-to-buffer)

    ;; Ensure todo, agenda, and other minor popups are delegated to shackle.
    (defun enfer*org-pop-to-buffer (&rest args)
      "Use `pop-to-buffer' instead of `switch-to-buffer' to open buffer.'"
      (let ((buf (car args)))
        (pop-to-buffer
         (cond ((stringp buf) (get-buffer-create buf))
               ((bufferp buf) buf)
               (t (error "Invalid buffer %s" buf))))))
    (advice-add #'org-switch-to-buffer-other-window :override #'enfer*org-pop-to-buffer)

    ;; org-agenda
    (setq org-agenda-window-setup 'other-window
          org-agenda-restore-windows-after-quit nil)
      ;; Hide modeline in org-agenda
    (add-hook 'org-agenda-finalize-hook #'enfer-hide-modeline-mode)
    (add-hook 'org-agenda-finalize-hook #'org-fit-window-to-buffer)
    ;; Don't monopolize frame!
    (advice-add #'org-agenda :around #'enfer*suppress-delete-other-windows)
    ;; ensure quit keybindings work propertly
    ;;(map! :map* org-agenda-mode-map
    ;;      :m [escape] 'org-agenda-Quit
    ;;      :m "ESC"    'org-agenda-Quit)
    ))
(add-hook 'enfer-init-hook #'enfer|init-org-popups)

(provide 'core-popups)
;;; core-popups.el ends here
