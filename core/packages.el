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
(package! delight
 :recipe (:fetcher github :repo "antonio/delight.el"))

;; core-projects.el
(package! projectile)

;; cli/test.el
(package! buttercup)

;; core-keybinds.el
(package! bind-key)
(package! general)
(package! which-key)
(package! hydra)

(package! visual-regexp-steroids
          :recipe (:fetcher github  :repo "benma/visual-regexp-steroids.el"  :files ("visual-regexp-steroids.el")))

;; core-packages.el
(package! gnu-elpa-keyring-update
  :recipe (:fetcher git :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git"
   :files ("*.el")))

;; autoload/debug.el
(package! esup)

