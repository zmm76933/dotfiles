;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; region.el --- region setting file
;;
;; $Id$

;;; Commentary:
;;
;; @see https://github.com/magnars/expand-region.el

;;; Code:
;;
;; expand region
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

;;; region.el ends here
