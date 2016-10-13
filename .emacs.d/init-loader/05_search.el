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
(global-anzu-mode +1)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => "))

;; avy
(custom-set-variables
 '(avy-case-fold-search nil))

;; search.el ends here
