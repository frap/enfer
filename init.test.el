;;; init.test.el -- for automated unit tests -*- lexical-binding: t; -*-

(require 'core (concat user-emacs-directory "core/core"))

(doom! :feature

       :completion
       company

       :ui
       doom-dashboard
       workspaces

       :tools
       pass

       :lang
       org
       web

       :private
       hlissner)
