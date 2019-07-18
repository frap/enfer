;; -*- no-byte-compile: t; -*-
;;; ../core/test/test-core-ui.el

(require 'core-ui)

(describe "core/ui"
  (describe "enfer|protect-fallback-buffer"
    :var (kill-buffer-query-functions a b)
    (before-all
      (setq kill-buffer-query-functions '(enfer|protect-fallback-buffer)))

    (it "should kill other buffers"
      (expect (kill-buffer (get-buffer-create "a"))))

    (it "shouldn't kill the fallback buffer"
      (expect (not (kill-buffer (enfer-fallback-buffer)))))))
