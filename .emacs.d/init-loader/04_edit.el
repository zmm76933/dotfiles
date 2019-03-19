;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; edit.el --- editing setting file
;;
;; $Id$

;;; Commentary:
;;
;; 利用する環境共通の設定

;;; Code:
;;
;; Use regexp version as Default
(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "ESC M-%") 'anzu-query-replace-at-cursor)
(global-set-key (kbd "C-x %") 'anzu-replace-at-cursor-thing)
(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
(define-key isearch-mode-map (kbd "M-a") 'avy-isearch)
(define-key minibuffer-local-completion-map (kbd "C-w") 'editutil-backward-delete-word)

;; electrict-mode
(custom-set-variables
 '(electric-indent-mode nil))

;;; edit.el ends here
