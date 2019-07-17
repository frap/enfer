;;; core/cli/debug.el -*- lexical-binding: t; -*-

(dispatcher! info (enfer/info)
  "Output system info in markdown for bug reports.")

(dispatcher! (version v) (enfer/version)
  "Reports the version of Enfer and Emacs.")
