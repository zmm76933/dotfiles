;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;;; window.el --- window setting file
;;
;; $Id$

;;; Commentary:
;;
;; Window configuration

;; popwin
(require 'popwin)

(defvar popwin:special-display-config-backup popwin:special-display-config)
(custom-set-variables
 '(display-buffer-function 'popwin:display-buffer))

;; remove from default config
(dolist (stuff '("*vc-diff*" "*vc-change-log*"))
  (delete stuff popwin:special-display-config))

;; basic
(push '("*Help*" :stick t :noselect t) popwin:special-display-config)
(push '("*sdic*") popwin:special-display-config)

;; python
(push '("*Python*"   :stick t) popwin:special-display-config)
(push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
(push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

;; Go
(push '("^\*go-direx:" :position left :width 0.3 :dedicated t :stick t :regexp t)
      popwin:special-display-config)

;; flycheck
(push '(flycheck-error-list-mode :stick t) popwin:special-display-config)

;; quickrun
(push '("*quickrun*" :stick t) popwin:special-display-config)

;; direx
(push '(direx:direx-mode :position left :width 30 :dedicated t)
      popwin:special-display-config)

;;; window.el ends here
