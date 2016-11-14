;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; appearance.el --- appearance setting file
;;
;; $Id$

;;; Commentary:
;;
;; 利用する環境共通の設定

;;; Code:
;;
;;
(cond
 ;; デュアルだったりトリプルだったりするので width の方は条件に入れてない
 ;; 設定は (frame-parameter (selected-frame) 'height) などで値を取得して設定する
 ((= (display-pixel-height) 1440)
  (setq default-frame-alist
        (append (list
                 '(width . 200)
                 '(height . 80)
                 '(top . 100)
                 '(left . 650)
                 )
                default-frame-alist)))
 ;; 1920 * 1080 ディスプレイ
 ((= (display-pixel-height) 1080)
  (setq default-frame-alist
        (append (list
                 '(width . 180)
                 '(height . 68)
                 '(top . 22)
                 '(left . 0)
                 )
                default-frame-alist)))
 ;; MacBook Pro 15inch ディスプレイ
 ((= (display-pixel-height) 900)
  (setq default-frame-alist
        (append (list
                 '(width . 140)
                 '(height . 56)
                 '(top . 22)
                 '(left . 0)
                 )
                default-frame-alist)))
 ;; とりあえずその他 完全に未確認で分岐できる事を確認するためのコード
 (t
  (setq default-frame-alist
        (append (list
                 '(width . 140)
                 '(height . 50)
                 '(top . 90)
                 '(left . 100)
                 )
                default-frame-alist))))

;; 背景を半透明にする
(setq default-frame-alist
      (append (list
               '(alpha . (90 85))
               ) default-frame-alist))

;; 垂直スクロール用のスクロールバーを付けない
(setq-default horizontal-scroll-bar nil)

;; ツールバーとスクルールバーを無効
(when window-system
  (set-scroll-bar-mode 'nil)
  (tool-bar-mode 0))

;; メニューバーを消す
(menu-bar-mode -1)

;; 選択領域を強調
(transient-mark-mode t)

(require 'hl-line)
;;; hl-lineを無効にするメジャーモードを指定する
(defvar global-hl-line-timer-exclude-modes '(todotxt-mode
                                             eshell-mode))
(defun global-hl-line-timer-function ()
  (unless (memq major-mode global-hl-line-timer-exclude-modes)
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight))))
(setq global-hl-line-timer
      (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;;; appearance.el ends here
