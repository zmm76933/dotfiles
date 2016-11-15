;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; evil.el --- evil setting
;;
;; $Id$

;;; Commentary:
;;
;;

;;; Code:
;;
;; evil
(custom-set-variables
 '(evil-move-cursor-back nil)
 '(evil-search-module 'evil-search))

(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-b") 'evil-backward-char)
(define-key evil-insert-state-map (kbd "C-f") 'evil-forward-char)
(define-key evil-insert-state-map (kbd "C-d") 'evil-delete-char)
(define-key evil-insert-state-map (kbd "C-n") 'evil-next-line)
(define-key evil-insert-state-map (kbd "C-p") 'evil-previous-line)
(define-key evil-insert-state-map (kbd "C-w") 'evil-delete)
(define-key evil-insert-state-map (kbd "C-y") 'yank)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)

(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)

(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ":") 'evil-repeat-find-char)

(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

(define-key evil-ex-completion-map (kbd "C-a") 'beginning-of-line)
(define-key evil-ex-completion-map (kbd "C-e") 'end-of-line)
(define-key evil-ex-completion-map (kbd "C-b") 'evil-backward-char)
(define-key evil-ex-completion-map (kbd "C-f") 'evil-forward-char)
(define-key evil-ex-completion-map (kbd "C-d") 'evil-delete-char)

;;; evil.el ends here
