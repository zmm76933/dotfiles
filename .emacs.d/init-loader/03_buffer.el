;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; buffer.el --- buffer setting file
;;
;; $Id$

;;; Commentary:
;;
;; 利用する環境共通の設定

;;; Code:
;;
;; setting about 'buffer'

;; naming of same name file
(require 'uniquify)
(custom-set-variables
 '(uniquify-buffer-name-style 'post-forward-angle-brackets))

(with-eval-after-load 'bs
  (fset 'bs-message-without-log 'ignore))

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
  (define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line))

;;; Buffer.el ends here
