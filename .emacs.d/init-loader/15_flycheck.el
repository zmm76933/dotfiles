;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; flycheck.el --- flycheck setting file
;;
;; $Id$

;;; Commentary:
;;
;; flycheck-mode

;;; Code:
;;
;;
(custom-set-variables
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-idle-change-delay 1.0)
 '(flycheck-display-errors-function nil))

(with-eval-after-load 'flycheck
  (define-key flycheck-command-map (kbd "M-g M-n") 'flycheck-next-error)
  (define-key flycheck-command-map (kbd "M-g M-p") 'flycheck-previous-error))

;;; flycheck.el ends here
