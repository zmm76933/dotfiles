;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; company.el --- compan setting file
;;
;; $Id$

;;; Commentary:
;;
;; 補完関連の設定

;;; Code:
;;
;; company-mode
(custom-set-variables
 '(company-selection-wrap-around t)
 '(company-idle-delay nil))

(global-company-mode +1)
;; suppress minibuffer message
(fset 'company-echo-show #'ignore)

(global-set-key (kbd "C-M-i") 'company-complete)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-i") 'company-complete-selection)

(define-key lisp-interaction-mode-map (kbd "C-M-i") 'company-elisp)
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

;;; company.el ends here
