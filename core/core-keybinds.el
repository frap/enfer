;;; core-keybinds.el -*- lexical-binding: t; -*-

;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' are ignored (i.e. omitted
;; entirely for performance reasons).

;;
;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar enfer-escape-hook nil
  "A hook run after C-g is pressed (or ESC in normal mode, for evil users). Both
trigger `enfer/escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun enfer/escape ()
  "Run `enfer-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'enfer-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'enfer/escape)


;;
;;; use-package bind-key
(require 'bind-key)

;;
;;; Packages

(def-package! crux
  :defer 1
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-r" . crux-recentf-find-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)))

(def-package! which-key
  :defer 1
  :after-call pre-command-hook
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  ;; Replacements for how KEY is replaced when which-key displays
       ;;   KEY → FUNCTION
       ;; Eg: After "C-c", display "right → winner-redo" as "▶ → winner-redo"
       (setq which-key-key-replacement-alist
             '(("<\\([[:alnum:]-]+\\)>" . "\\1")
               ("left"                  . "◀")
               ("right"                 . "▶")
               ("up"                    . "▲")
               ("down"                  . "▼")
               ("delete"                . "DEL") ; delete key
               ("\\`DEL\\'"             . "BS") ; backspace key
               ("next"                  . "PgDn")
               ("prior"                 . "PgUp"))

             ;; List of "special" keys for which a KEY is displayed as just
             ;; K but with "inverted video" face... not sure I like this.
             which-key-special-keys '("RET" "DEL" ; delete key
                                      "ESC" "BS" ; backspace key
                                      "SPC" "TAB")

             ;; Replacements for how part or whole of FUNCTION is replaced:
             which-key-description-replacement-alist
             '(("Prefix Command" . "prefix")
               ("\\`calc-"       . "") ; Hide "calc-" prefixes when listing M-x calc keys
               ("\\`projectile-" . "𝓟/")
               ("\\`org-babel-"  . "ob/"))

             ;; Underlines commands to emphasize some functions:
             which-key-highlighted-command-list
             '("\\(rectangle-\\)\\|\\(-rectangle\\)"
               "\\`org-"))

       ;; Change what string to display for a given *complete* key binding
       ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
       (which-key-add-key-based-replacements
         "C-x 8"   "unicode"
         "C-c T"   "toggles-"
         "C-c p s" "projectile-search"
         "C-c p 4" "projectile-other-buffer-"
         "C-x a"   "abbrev/expand"
         "C-x r"   "rect/reg"
         "C-c /"   "engine-mode-map"
         "C-c C-v" "org-babel")
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  (which-key-mode +1))

;; =M-g= interactively asks for a line number and jump to it (=goto-line)=.
(bind-key "M-g" 'goto-line)
;; hippe-expand
(bind-key "M-/" 'hippie-expand)
;; =M-`= focuses the next frame, if multiple ones are active (emulate the Mac "next app window" keybinding)
(bind-key "M-`" 'other-frame)

;;Interactive search key bindings -  [[https://github.com/benma/visual-regexp-steroids.el][visual-regexp-steroids]] provides sane regular expressions and visua
(def-package! pcre2el)
(def-package! visual-regexp-steroids
      :custom
      (vr/engine 'pcre2el "Use PCRE regular expressions")
      :bind
      ("C-c r" . vr/replace)
      ("C-c q" . vr/query-replace)
      ("C-r"   . vr/isearch-backward)
      ("C-S-s" . vr/isearch-forward)
      ("C-M-s" . isearch-forward)
      ("C-M-r" . isearch-backward))

;;  Emulating vi's =%= key
  (defun zz/goto-match-paren (arg)
    "Go to the matching paren/bracket, otherwise (or if ARG is not nil) insert %.
    vi style of % jumping to matching brace."
    (interactive "p")
    (if (not (memq last-command '(set-mark
                                  cua-set-mark
                                  zz/goto-match-paren
                                  down-list
                                  up-list
                                  end-of-defun
                                  beginning-of-defun
                                  backward-sexp
                                  forward-sexp
                                  backward-up-list
                                  forward-paragraph
                                  backward-paragraph
                                  end-of-buffer
                                  beginning-of-buffer
                                  backward-word
                                  forward-word
                                  mwheel-scroll
                                  backward-word
                                  forward-word
                                  mouse-start-secondary
                                  mouse-yank-secondary
                                  mouse-secondary-save-then-kill
                                  move-end-of-line
                                  move-beginning-of-line
                                  backward-char
                                  forward-char
                                  scroll-up
                                  scroll-down
                                  scroll-left
                                  scroll-right
                                  mouse-set-point
                                  next-buffer
                                  previous-buffer
                                  previous-line
                                  next-line
                                  back-to-indentation
                                  )))
        (self-insert-command (or arg 1))
      (cond ((looking-at "\\s\(") (sp-forward-sexp) (backward-char 1))
            ((looking-at "\\s\)") (forward-char 1) (sp-backward-sexp))
            (t (self-insert-command (or arg 1))))))

(bind-key "%" 'zz/goto-match-paren)




;;;###package hydra
(setq lv-use-seperator t)



;; Register keywords for proper indentation (see `map!')
(put :after        'lisp-indent-function 'defun)
(put :desc         'lisp-indent-function 'defun)
(put :leader       'lisp-indent-function 'defun)
(put :localleader  'lisp-indent-function 'defun)
(put :map          'lisp-indent-function 'defun)
(put :keymap       'lisp-indent-function 'defun)
(put :mode         'lisp-indent-function 'defun)
(put :prefix       'lisp-indent-function 'defun)
(put :prefix-map   'lisp-indent-function 'defun)
(put :unless       'lisp-indent-function 'defun)
(put :when         'lisp-indent-function 'defun)

;; specials
(defvar enfer--map-forms nil)
(defvar enfer--map-fn nil)
(defvar enfer--map-batch-forms nil)
(defvar enfer--map-state '(:dummy t))
(defvar enfer--map-parent-state nil)

(provide 'core-keybinds)
;;; core-keybinds.el ends here
