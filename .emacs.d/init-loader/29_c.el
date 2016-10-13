;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; c.el --- C and C++ setting file
;;
;; $Id$

;;; Commentary:
;;
;; Cの設定


;;; Code:
;;
;; スタイル設定
(add-hook 'c-mode-common-hook
          '(lambda()

             ;; 自動保管company mode
             (setq company-backends '(company-clang company-dabbrev))

             ;; styleには GNU,cc-mode,ktr,bsd,stroustrup,whitesmith,ellemtel,linux等がある
             (c-set-style "k&r")

             ;; tabモード
             (setq indent-tabs-mode t)

             ;; 基本オフセット
             (setq c-basic-offset 4)

             ;; コメント行のオフセット
             (setq c-comment-only-line-offset 0)

             ;; 全自動インデントを有効
             ;;(setq c-auto-newline t)

             ;; @see http://d.hatena.ne.jp/syohex/20110624/1308871777
             (c-toggle-electric-state -1)

             ;; TABキーでインデント
             ;;(setq c-tab-always-indent t)

             ;; namespace {}の中はインデントしない
             (c-set-offset 'innamespace 0)

             ;; 関数の引数リストの閉じ括弧はインデントしない
             (c-set-offset 'arglist-close 0)

             ;; 連続するスペースをバックスペース一回で削除する
             (c-toggle-hungry-state t)

             ;; キーコンフィグ
             (local-set-key (kbd "C-c C-d") 'gdb)
             (local-set-key (kbd "C-c C-c") 'compile)
             (local-set-key (kbd "C-c C-p") 'ff-find-other-file)
             (local-set-key (kbd "C-c C-n") 'ff-find-other-file)
             ))

;; Makefile用の設定
(add-hook 'makefile-mode-hook
          (function (lambda ()
                      (whitespace-mode t)
                      ;; suspicious-lines を無視しておく
                      (fset 'makefile-warn-suspicious-lines 'ignore)
                      (setq indent-tabs-mode t))))

;;; c.el ends here
