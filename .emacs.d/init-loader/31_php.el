;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; php.el --- php setting file
;;
;; $Id$

;;; Commentary:
;;
;; php-mode

;;; Code:
;;
;;
(defun my-php-mode-init ()
  (setq-local ac-disable-faces '(font-lock-comment-face font-lock-string-face))
  (setq-local indent-tabs-mode nil)
  (setq-local page-delimiter "\\_<\\(class\\|function\\|namespace\\)\\_>.+$")

  ;; If you feel phumped and phpcs annoying, invalidate them.
  (when (boundp 'flycheck-disabled-checkers)
    (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
    (add-to-list 'flycheck-disabled-checkers 'php-phpcs)))

(add-hook 'php-mode-hook #'my-php-mode-init)
;;; php.el ends here
