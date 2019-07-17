;;; lang/clojure/config.el -*- lexical-binding: t; -*-

;; `clojure-mode'

(def-package! clojure-mode
  :mode (("\\.edn$"  . clojurec-mode)
         ("\\.boot$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljc$" . clojurec-mode)
         )
    :config
    (defconst clojure--prettify-symbols-alist
    '(("fn"   . ?λ)
      ("__"   . ?⁈)
      ("<=" . ?≤)
      (">=" . ?≥)
      ("<-" . ?←)
      ("->" . ?→)
      ("<=" . ?⇐)
      ("=>" . ?⇒)
      ("lambda" . ?λ)
      ))
 )
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)


(def-package! cider
  ;; NOTE: if you don't have an org directory set (the dir doesn't exist),
  ;; cider jack in won't work.
  :commands (cider-jack-in cider-jack-in-clojurescript)
  :hook (clojure-mode-local-vars . cider-mode)
  :init
  (set-repl-handler! 'clojure-mode #'+clojure/repl)
  (set-eval-handler! 'clojure-mode #'cider-eval-region)
  (set-lookup-handlers! 'cider-mode
    :definition #'+clojure-cider-lookup-definition
    :documentation #'cider-doc)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  :config
  (set-popup-rules!
    '(("^\\*cider-error*" :ignore t)
      ("^\\*cider-repl" :quit nil)
      ("^\\*cider-repl-history" :vslot 2 :ttl nil)))

  (setq nrepl-hide-special-buffers t
        nrepl-log-messages nil
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t
        cider-prompt-for-symbol nil
        cider-repl-display-help-banner nil
        cider-repl-history-display-duplicates nil
        cider-repl-history-display-style 'one-line
        cider-repl-history-file (concat enfer-cache-dir "cider-repl-history")
        cider-repl-history-highlight-current-entry t
        cider-repl-history-quit-action 'delete-and-restore
        cider-repl-history-highlight-inserted-item t
        cider-repl-history-size 1000
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-result-prefix ";; => "
        cider-repl-print-length 100
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history nil
        cider-stacktrace-default-filters '(tooling dup))

 )


(def-package! clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :init
  (set-lookup-handlers! 'clj-refactor-mode
    :references #'cljr-find-usages)
  :bind (:map clojure-mode-map
         ("R" .  #'hydra-cljr-help-menu/body)))


(def-package! flycheck-joker
  :when (featurep! :tools flycheck)
  :after flycheck)
