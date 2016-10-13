;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; spelling.el --- spell checksetting file
;;
;; $Id$

;;; Commentary:
;;
;;

;;; Code:
;;
;; configuration of spell check
(custom-set-variables
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-use-meta-tab nil))

(with-eval-after-load 'ispell
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;;; spelling.el ends here
