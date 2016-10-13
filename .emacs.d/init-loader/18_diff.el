;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; diff.el --- diff and ediff init file
;;
;; $Id$

;;; Commentary:
;;
;; diff および ediff の動作を設定

;;; Code:
;;
;; ediff
(custom-set-variables
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-diff-options "-twB"))

;;; diff.el ends here
