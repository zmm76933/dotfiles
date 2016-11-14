;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; multiple-cursors.el --- multiple setting file
;;
;; $Id$

;;; Commentary:
;;
;; @See http://emacsrocks.com

;;; Code:
;;
;;
(setq mc/list-file (concat my/history-dir "mc-lists.el"))
(declare-function smartrep-define-key "smartrep")

(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-g") 'mc/mark-all-in-region)

(global-unset-key "\C-]")

(smartrep-define-key
    global-map "C-]"  '(("C-]"      . 'mc/mark-next-like-this)
                        ("n"        . 'mc/mark-next-like-this)
                        ("p"        . 'mc/mark-previous-like-this)
                        ("m"        . 'mc/mark-more-like-this-extended)
                        ("u"        . 'mc/unmark-next-like-this)
                        ("U"        . 'mc/unmark-previous-like-this)
                        ("s"        . 'mc/skip-to-next-like-this)
                        ("S"        . 'mc/skip-to-previous-like-this)
                        ("*"        . 'mc/mark-all-like-this)
                        ("d"        . 'mc/mark-all-like-this-dwim)
                        ("i"        . 'mc/insert-numbers)
                        ("o"        . 'mc/sort-regions)
                        ("O"        . 'mc/reverse-regions)))

;;; multiple-cursors.el ends here
