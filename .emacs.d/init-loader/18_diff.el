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

(defun keymap-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference)
  (define-key ediff-mode-map "z" 'ediff-scroll-vertically)
  (define-key ediff-mode-map "C-z" 'ediff-scroll-vertically))

(add-hook 'ediff-mode-hook 'keymap-ediff-hook)



;;; diff.el ends here
