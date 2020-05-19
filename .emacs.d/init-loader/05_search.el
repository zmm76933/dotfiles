;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; search.el --- search setting file
;;
;; $Id$

;;; Commentary:
;;
;; search関連の設定

;;; Code:
;;
;; anzu
(global-anzu2-mode +1)
(custom-set-variables
 '(anzu2-mode-lighter "")
 '(anzu2-deactivate-region t)
 '(anzu2-search-threshold 1000)
 '(anzu2-replace-threshold 50)
 '(anzu2-replace-to-string-separator " => "))

;; search.el ends here
