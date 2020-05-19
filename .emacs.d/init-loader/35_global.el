;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; global.el --- gtag setting file
;;
;; $Id$

;;; Commentary:
;;
;;

;;; Code:
;;
;; GNU gtags
(with-eval-after-load 'helm-gtags2
  (define-key helm-gtags2-mode-map (kbd "M-t") #'helm-gtags2-find-tag)
  (define-key helm-gtags2-mode-map (kbd "M-r") #'helm-gtags2-find-rtag)
  (define-key helm-gtags2-mode-map (kbd "M-s") #'helm-gtags2-find-symbol)
  (define-key helm-gtags2-mode-map (kbd "M-g M-s") #'helm-gtags2-select)
  (define-key helm-gtags2-mode-map (kbd "C-c >") #'helm-gtags2-next-history)
  (define-key helm-gtags2-mode-map (kbd "C-c <") #'helm-gtags2-previous-history)
  (define-key helm-gtags2-mode-map (kbd "C-t") #'helm-gtags2-pop-stack))


;;; Enable helm-gtags-mode
(dolist (hook '(c-mode-common-hook
                java-mode-hook
                asm-mode-hook))
  (add-hook hook #'helm-gtags2-mode))

;;; global.el ends here
