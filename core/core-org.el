;;; core-org.el -*- lexical-binding: t; -*-

(def-package! org
 :mode ("\\.org\\'" . org-mode)
 :init
       (font-lock-add-keywords 'org-mode
        '(("^ +\\([-*]\\) "
               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
 :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-M-|" . indent-rigidly)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto)
         ("C-c C-x C-o" . org-clock-out)
         ("C-M-|" . indent-rigidly)
         :map org-mode-map
         ("M-n" . outline-next-visible-heading)
         ("M-p" . outline-previous-visible-)
         ("M-C-n" . org-end-of-item-list)
         ("M-C-p" . org-beginning-of-item-list)
         ("C-s-f" . forward-sentence)
         ("C-s-b" . backward-sentence)
        )
  :config
  (setq org-modules '(org-habit org-id org-protocol org-timer org-bullets))
  (def-package! org-bullets
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(setq org-directory "~/org/gtd/")
(defconst enfer-org-agenda-file (concat org-directory "atea.org"))
(defconst enfer-org-journal-file (concat org-directory "journal.org"))
(setq org-default-notes-file (concat org-directory "inbox.org")
       org-agenda-files (list enfer-org-agenda-file))

(setq org-startup-indented t
       org-startup-with-inline-images t
       org-startup-with-latex-preview t
       org-pretty-entities t
       org-image-actual-width '(700)
       org-fontify-quote-and-verse-blocks t)

(add-hook! org-mode #'org-hide-block-all)

(setq  org-tags-column -92
        org-ellipsis " ↴ "
        org-catch-invisible-edits 'smart
        org-return-follows-link t
        org-list-allow-alphabetical t
        org-loop-over-headlines-in-active-region t
        org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

(defun enfer|org-summary-todo (n-done n-not-done)
  "Update todo keyword after changing the statistics cookie, when needed."
  (let ((keyword (org-get-todo-state)))
    (if (= n-not-done 0)
        (when (not (member keyword org-done-keywords)) (org-todo "FINI"))
      (when (member keyword org-done-keywords) (org-todo "TODO")))))
(add-hook! 'org-after-todo-statistics-hook #'enfer|org-summary-todo)

(defun enfer|org-project-set-next-after-done ()
  "Ask to move TODO to PROCHAINE when changing previous states from FINI."
  (let ((done-keywords (or org-done-keywords org-done-keywords-for-agenda)))
    (when (and (member org-state done-keywords) (+org-is-subtask))
      (org-with-wide-buffer
       (org-back-to-heading t)

       (let (point keyword break)
         (while (and (save-excursion (setq point (org-get-last-sibling))) (not break))
           (goto-char point)
           (setq keyword (org-get-todo-state))
           (when (or (member keyword done-keywords)
                     (and (not (+org-project-p))
                          (string= keyword "TODO")))
             (setq break t)
             (org-get-next-sibling))))

       (let (target keyword break)
         (while (not (or target break))
           (setq keyword (org-get-todo-state))
           (unless (+org-project-p)
             (if (string= keyword "TODO")
                 (setq target (cons (point) (org-get-heading t t t t)))
               (setq break (string= keyword "PROCHAINE"))))
           (setq break (or break (not (org-get-next-sibling)))))

         (when (consp target)
           (when (y-or-n-p (concat "Voulez-vous mettre " (cdr target) "à PROCHAINE?"))
             (goto-char (car target))
             (org-todo "PROCHAINE"))))))))
(add-hook 'org-after-todo-state-change-hook #'enfer|org-project-set-next-after-done)

(setq
 org-todo-keywords '(
     (sequence "TODO(t)" "PROCHAINE(n)" "|" "FINI(f)")
     (sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
     (sequence "SUSPENDUE(h@/!)" "ATTENTE(w@/!)" "|" "ANNULÉE(c@/!)"))
 org-treat-S-cursor-todo-selection-as-state-change nil
 org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
 org-global-properties '(("Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 5:00 7:00")))

(setq org-refile-use-outline-path 'file
       org-outline-path-complete-in-steps nil
       org-refile-allow-creating-parent-nodes 'confirm
       org-refile-targets `((nil . (:maxlevel . 9))
                            (org-agenda-files . (:maxlevel . 9))))

(add-hook! 'org-after-refile-insert-hook
  (org-up-heading-safe)
  (org-update-statistics-cookies nil))

(def-package! hydra
  :config
  ;; Define the templates
  (setq org-structure-template-alist
        '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
          ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
          ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
          ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n/verse>")
          ("n" "#+begin_note\n?\n#+end_note" "<note>\n?\n/note>")
          ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n/center>")
          ("l" "#+begin_export latex\n?\n#+end_export" "<literal style=\"latex\">\n?\n</literal>")
          ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
          ("h" "#+begin_export html\n?\n#+end_exrt" "<literal style=\"html\">\n?\n</literal>")
          ("H" "#+html: " "<literal style=\"html\">?</literal>")
          ("a" "#+begin_export ascii\n?\n#+end_export")
          ("A" "#+ascii: ")
          ("i" "#+index: ?" "#+index: ?")
          ("I" "#+include: %file ?" "<include file=%file markup=\"?\">")))

  ;; Shortcuts
  (defun hot-expand (str &optional mod)
    "Expand org template."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (org-try-structure-completion)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
     Org template

 block               src block         structure
--------------------------------------------------------------------------------------
_C_: center        _s_: src         _L_: LATEX:
_q_: quote         _e_: emacs lisp  _i_: index:
_E_: example       _p_: python      _I_: INCLUDE:
_v_: verse         _P_: perl        _H_: HTML:
_a_: ascii         _u_: Plantuml    _A_: ASCII:
_l_: latex         _d_: ditaa
_h_: html          _S_: shell
_n_: note          _c_: clojure
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("C" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("n" (hot-expand "<n"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("p" (hot-expand "<s" "python"))
    ("P" (hot-expand "<s" "perl"))
    ("j" (hot-expand "<s" "java"))
    ("c" (hot-expand "<s" "clojure"))
    ("S" (hot-expand "<s" "sh"))
    ("d" (hot-expand "<s" "ditaa :file CHANGE.png :cache yes"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.svg :cache yes"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("ESC" nil "quit"))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
          (hydra-org-template/body)
        (self-insert-command 1)))))

(setq org-log-done 'time
       org-log-reschedule 'time
       org-log-into-drawer t)

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (octave . t)
   (clojure . t)
   (python . t)
   (plantuml . t)
   (latex . t)
   (shell . t)
   (calc . t)))

(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.10/libexec/plantuml.jar" )
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(defun +babel-get-src-info ()
  "Return (LANG . SESSION)."
  (let* ((info (org-babel-get-src-block-info t))
         (params (nth 2 info)))
    (cons (car info)
          (cdr (assq :session params)))))

(defun +babel/kill-session ()
  "Kill session for current code block."
  (interactive)
  (org-babel-when-in-src-block
   (let ((config (current-window-configuration)))
     (org-babel-switch-to-session)
     (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
     (kill-buffer)
     (set-window-configuration config))))

(defun +babel/restart-session-to-point (&optional arg)
  "Restart session up to the src-block in the current point.
Goes to beginning of buffer and executes each code block with
`org-babel-execute-src-block' that has the same language and
session as the current block. ARG has same meaning as in
`org-babel-execute-src-block'."
  (interactive "P")
  (org-babel-when-in-src-block
   (let ((search-bound (point-marker))
         (info (+babel-get-src-info))
         break)
     (org-with-wide-buffer
      (goto-char (point-min))
      (while (and (not break) (re-search-forward org-babel-src-block-regexp nil t))
        (goto-char (match-beginning 0))
        (if (> (point) search-bound)
            (setq break t)
          (when (equal info (+babel-get-src-info)) (org-babel-execute-src-block arg)))
        (forward-line))))))

(defun +babel/remove-session-results ()
  "Remove results from every code block of the selected session, in buffer."
  (interactive)
  (org-babel-when-in-src-block
   (let ((info (+babel-get-src-info)))
     (org-with-wide-buffer
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (when (equal info (+babel-get-src-info))
          (org-babel-remove-result)))))))

(defun enfer-org-has-subtasks-p ()
  "Any heading with subtasks."
  (org-with-wide-buffer
   (let ((subtree-end (save-excursion (org-end-of-subtree t)))
         has-subtasks)
     (end-of-line)
     (while (and (not has-subtasks) (re-search-forward org-todo-line-regexp subtree-end t))
       (when (member (match-string 2) org-todo-keywords-1) (setq has-subtasks t)))
     has-subtasks)))

(defun +org-project-p ()
  "Any task that has subtasks."
  (and (org-get-todo-state) (enfer-org-has-subtasks-p)))

(defun +org-is-subtask (&optional first)
  "Return t if this task is a subtask."
  (let (return)
    (org-with-wide-buffer
     (org-back-to-heading 'invisible-ok)
     (while (and (not return) (org-up-heading-safe))
       (when (org-get-todo-state) (setq return t))))
    return))

(setq org-tag-alist (quote (("@errand" . ?e)
                            ("@bureau" . ?o)
                            ("@maison" . ?h)
                            ("@ferme"  . ?f)
                            (:newline)
                            ("ATTENTE"  . ?w)
                            ("SUSPENDUE" . ?H)
                            ("ANNULÉE"    . ?c)
                            ("RÉUNION"   . ?m)
                            ("TÉLÉPHONE" . ?p))))

(defun enfer|org-offer-all-agenda-tags ()
  (setq-local org-complete-tags-always-offer-all-agenda-tags t))

)

(def-package! org-agenda
  :config
  (defun +agenda|check-sync-conflicts ()
    (when (directory-files org-directory nil "sync-conflict")
      (message-box "CONSEIL: Il y a des conflits de synchronisation!")))
  (add-hook 'org-agenda-finalize-hook #'+agenda|check-sync-conflicts)
  (setq  org-agenda-custom-commands
   '(("n" "Ordre de jour"
      ((tags-todo "@important"
                  ((org-agenda-overriding-header "Tâches hautement importante")))
       (tags-todo "@urgent"
                  ((org-agenda-overriding-header "Tâches à faible priorité")))
       (tags-todo "TODO/!@urgent"
                  ((org-agenda-overriding-header "Tâches isolées")))
       (tags-todo "ATTENTE"
                  ((org-agenda-overriding-header "Tâches en attente")))
       )))
   org-agenda-prefix-format '((agenda . "  %?-12t% s"))
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   org-agenda-tags-todo-honor-ignore-options t
   org-agenda-todo-ignore-scheduled 'all
   org-agenda-todo-ignore-deadlines 'far
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-clockreport-parameter-plist `(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 100)
   org-agenda-columns-add-appointments-to-effort-sum t
   org-agenda-dim-blocked-tasks nil
   org-agenda-todo-list-sublevels nil
   org-agenda-block-separator ""
   org-agenda-time-grid '((daily today require-timed) nil "......" "----------------")
   )
  (add-hook 'org-agenda-mode-hook 'enfer|org-offer-all-agenda-tags)
  (def-package! org-gcal
    :after '(auth-source-pass password-store)
    :config
    (setq org-gcal-client-id "887865341451-orrpnv3cu0fnh8hdtge77sv6csqilqtu.apps.googleusercontent.com"
          org-gcal-client-secret "WmOGOCr_aWPJSqmwXHV-29bv"
          org-gcal-file-alist
          '(("agasson@ateasystems.com" . "~/org/gtd/calendars/atea-cal.org")
            ("ateasystems.com_0ie21uc26j0a41g60b8f99mh1k@group.calendar.google.com" . "~/org/gtd/calendars/changecontrol-cal.org")))
    )
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  )

(def-package! org-capture
  :init
  (defun enfer-org-capture-frame ()
    (modify-frame-parameters nil '((name . "Org Capture")
                                   (org-capture-frame . t)
                                   (width . 110) (height . 40)))
    (org-capture))

  :config
  (setq
   org-capture-templates
      '(("t" "T" entry (file "")
         "* PROCHAINE %i%?" :clock-in t :clock-resume t)
        ("r" "respond" entry (file "~/org/gtd/inbox.org")
         "* PROCHAINE Respond to %:from on %:subject :@bureau:\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("c" "Calendrier" entry (file "")
         "* %?\n%^t")
        ("n" "Remarque" entry (file "")
         "* %?" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+olp+datetree enfer-org-journal-file))
        ("m" "Meeting" entry (file "~/org/gtd/calendars/atea-cal.org")
         "* RÉUNION with %? :RÉUNION:@bureau:\n%U" :clock-in t :clock-resume t)
        ("p" "Phone call" entry (file+headline "~/org/gtd/atea.org" "Interruptions")
         "* TÉLÉPHONE %? :TÉLÉPHONE:@bureau:\n%U" :clock-in t :clock-resume t)
        ("h" "Habit🙈" entry (file "~/org/gtd/atea.org")
         "* TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: PROCHAINE\n:END:\n")
        ("w" "Web bookmark" entry (file "")
         "* [[%:link][%^{Title|%:description}]]\n%?" :clock-in t :clock-resume t))
      ))

(def-package! org-clock
  :config
  (setq org-clock-in-resume t
         org-clock-out-remove-zero-time-clocks t
         org-clock-report-include-clocking-task t
         org-clock-persist t
         org-clock-persist-file (concat enfer-etc-dir "org-clock-save.el")
         org-clock-history-length 25)
  (org-clock-persistence-insinuate))



(def-package! org-habit
  :config
  (setq org-habit-graph-column 75
         org-habit-preceding-days 30
         org-habit-following-days 1
         org-habit-today-glyph ?@))


(def-package! org-src
  :custom
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  :config
  (add-to-list 'org-src-lang-modes '("html" . web)))

(provide 'core-org)
;;; core-org.el ends here
