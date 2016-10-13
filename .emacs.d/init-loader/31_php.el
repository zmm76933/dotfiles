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
(autoload 'php-mode "php-mode" "major mode for editing PHP code" t)
(add-to-list 'auto-mode-alist '("\\.php[s34]?\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(setq auto-mode-alist (cons '("\\.ctp$" . html-mode) auto-mode-alist))

(add-hook  'php-mode-hook
           (lambda ()
             ;; インデントの設定
             (setq php-mode-force-pear t)))

;;; php.el ends here
