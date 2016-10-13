;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; keychord.el --- key-chord setting
;;
;; $Id$

;;; Commentary:
;;
;; http://emacswiki.org/emacs/key-chord.el

;;; Code:
;;
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
(key-chord-define-global "jk" 'view-mode)

;;; keychord.el ends here
