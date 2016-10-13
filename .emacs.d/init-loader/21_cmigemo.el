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
(require 'migemo)
(custom-set-variables
 `(migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
 '(migemo-command "cmigemo")
 '(migemo-options '("-q" "--emacs"))
 '(migemo-user-dictionary nil)
 '(migemo-regex-dictionary nil)
 '(migemo-coding-system 'utf-8-unix)
 '(migemo-isearch-enable-p nil))

(migemo-init)

;;; cmigemo.el ends here
