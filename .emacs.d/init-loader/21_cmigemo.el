;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; cmigemo.el --- cmigemo setting file
;;
;; $Id$

;;; Commentary:
;;
;; cmigemo の設定

;;; Code:
;;
;; 基本設定
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (custom-set-variables
   '(migemo-options '("-q" "--emacs"))
   '(migemo-user-dictionary nil)
   '(migemo-regex-dictionary nil)
   '(migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
   '(migemo-coding-system 'utf-8-unix))
  (load-library "migemo")
  (migemo-init)
)

;;; cmigemo.el ends here
