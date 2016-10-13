;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; yasnippet.el --- yasnippet setting file
;;
;; $Id$

;;; Commentary:
;;
;; @see http://code.google.com/p/yasnippet/

;;; Code:
;;
;; @see http://d.hatena.ne.jp/syohex/20121207/1354885367
(custom-set-variables
 '(yas-snippet-dirs (list (concat user-emacs-directory "etc/snippets")))
 '(yas-alias-to-yas/prefix-p nil))

(yas-global-mode 1)

(with-eval-after-load 'yasnippet
  (setq-default yas-verbosity 1)
  (yas-reload-all))

;; TABキーを無効にする
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-insert-snippet)

;; utility functions
(defun my-yas/perl-package-name ()
  (let ((file-path (file-name-sans-extension (buffer-file-name))))
    (if (string-match "lib/\\(.+\\)\\'" file-path)
        (replace-regexp-in-string "/" "::" (match-string 1 file-path))
      (file-name-nondirectory file-path))))

(defun my-yas/parent-directory ()
  (let ((curdir (directory-file-name (file-name-directory (buffer-file-name)))))
    (file-name-nondirectory curdir)))

;;; yasnippet.el ends here
