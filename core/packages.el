;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core.el
(package! dotenv-mode)

;; core-os.el
(if (not IS-MAC)
    (package! xclip)
  (package! osx-clipboard)
  (package! ns-auto-titlebar))

;; core-ui.el
(package! all-the-icons)
(package! hide-mode-line)
(package! highlight-numbers)
(unless (locate-library "display-line-numbers")
  (package! nlinum)
  (package! nlinum-hl)
  (package! nlinum-relative))
(package! rainbow-delimiters)
(package! restart-emacs)

;; core-editor.el
(package! better-jumper)
(package! command-log-mode)
(package! dtrt-indent)
(package! helpful)
(package! pcre2el)
(package! smartparens)
(package! undo-tree
 :recipe (undo-tree :fetcher git :url "http://www.dr-qubit.org/git/undo-tree.git"))
(package! ws-butler)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! bind-key)
(package! general)
(package! which-key)
(package! hydra)
(package! pcre2el)
(package! visual-regex-steroids
:recipe (visual-regex-steroids :fetcher url :url "https://github.com/benma/visual-regexp-steroids.el/blob/master/visual-regexp-steroids.el"))

;; core-packages.el
;;(package! gnu-elpa-keyring-update :recipe (gnu-elpa-keyring-update :fetcher url :url "https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html"))

;; core-org.el
(when-let (orglib (locate-library "org" nil enfer-site-load-path))
  (setq load-path (delete (substring (file-name-directory orglib) 0 -1)
                          load-path)))
(package! org-plus-contrib) ; install cutting-edge version of org-mode
(package! org :ignore t)    ; ignore org on ELPA, if possible
(package! toc-org)
(package! org-bullets :recipe (:fetcher github :repo "Kaligule/org-bullets"))
(package! htmlize)
(package! ox-clip)
(package! org-yt :recipe (:fetcher github :repo "TobiasZawada/org-yt"))

;;; Babel
(package! ob-async)
(when (featurep! :lang rust)
  (package! ob-rust))
(when (featurep! :lang clojure)
  (package! ob-clojurescript))
(package! org-gcal)
(package! org-pomodoro)
(package! outshine)
;; autoload/debug.el
(package! esup)

;; cli/test.el
(package! buttercup)
