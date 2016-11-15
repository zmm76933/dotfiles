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
;;
(setq key-chord-two-keys-delay 0.05)
(key-chord-mode 1)
(key-chord-define-global "ui" 'view-mode)
(key-chord-define-global "jk" 'evil-mode)
(key-chord-define evil-insert-state-map  "jj" 'evil-normal-state)

;;; keychord.el ends here
