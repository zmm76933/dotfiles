;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; region.el --- region setting file
;;
;; $Id$

;;; Commentary:
;;
;; @see https://github.com/magnars/expand-region.el
;; @see https://github.com/magnars/mark-multiple.el

;;; Code:
;;
;; expand region
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

;; mark-multiple
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(global-set-key (kbd "C-M-,") 'mark-previous-like-this)
(global-set-key (kbd "C-M-.") 'mark-next-like-this)

;;; region.el ends here
