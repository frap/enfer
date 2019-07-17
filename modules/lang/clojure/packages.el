;; -*- no-byte-compile: t; -*-
;;; lang/clojure/packages.el
(package! clojure-mode)
(package! queue
:recipe (queue :fetcher github :repo "emacsmirror/queue"))
(package! seq 
 :recipe (seq :fetcher github :repo "NicolasPetton/seq.el"))
(package! spinner 
 :recipe (spinner :fetcher github :repo "Malabarba/spinner.el"))
;;(package! cider
;; :recipe (cider
;;           :fetcher github
;;           :repo "clojure-emacs/cider"
;;           :files ("*.el" (:exclude ".dir-locals.el"))
 ;;          :old-names (nrepl)))
(package! clj-refactor)
;;/lisp-interaction-mode-map(package! flycheck-clojure)
(package! eval-sexp-fu
 :recipe (eval-sexp-fu  :fetcher github :repo "hchbaw/eval-sexp-fu.el"))

(when (featurep! :tools flycheck)
  (package! flycheck-joker)
  )
