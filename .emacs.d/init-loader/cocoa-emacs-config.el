;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; cocoa-emacs-init.el --- Emacs.app init setting file
;;
;; $Id$

;;; Commentary:
;;
;;

;;; Code:
;;
;;
(require 'ucs-normalize)

;; Mac のファイル名の UTF-8 は NFD
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8)

;; コマンドキーをMetaキーとして利用
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; emacsが保持するterminfoを利用する
(setq system-uses-terminfo nil)

;; ミニバッファに入力時、自動的に英語モード
(when (functionp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1))

;; ドラッグ&ドロップで新しくウィンドウ開かない
(setq ns-pop-up-frames nil)

;; フォント設定
(when (find-font (font-spec :family "Monaco"))

  (set-face-attribute 'default nil
                      :family "monaco"
                      :height 110)
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0212
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'katakana-jisx0201
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'mule-unicode-0100-24ff
   '("monaco" . "iso10646-1"))
  (set-fontset-font
   t 'symbol
   (font-spec :family "Apple Color Emoji") nil 'prepend)

  (setq face-font-rescale-alist
        '(("^-apple-hiragino.*" . 1.2)
          (".*osaka-bold.*" . 1.2)
          (".*osaka-medium.*" . 1.2)
          (".*courier-bold-.*-mac-roman" . 1.0)
          (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
          (".*monaco-bold-.*-mac-roman" . 0.9)
          ("-cdac$" . 1.3)))

  (set-face-bold 'show-paren-match nil))

;; See @http://blog.fenrir-inc.com/jp/2013/04/mac-quicklook-xattr.html
(defun my-set-com.apple.TextEncoding()
  (setq bfcs (prin1-to-string buffer-file-coding-system))
  (cond ((string-match "utf-8" bfcs)
         (setq cate "UTF-8;134217984"))
        ((string-match "sjis" bfcs)
         (setq cate "SHIFT_JIS;2561"))
        ((string-match "shift-jis" bfcs)
         (setq cate "SHIFT_JIS;2561"))
        ((string-match "euc-jp" bfcs)
           (setq cate "EUC-JP;2361"))
        ((string-match "japanese-iso-8bit" bfcs)
         (setq cate "EUC-JP;2361"))
        ((string-match "euc-japan" bfcs)
         (setq cate "EUC-JP;2361"))
        ((string-match "iso-2022-jp" bfcs)
         (setq cate "ISO-2022-JP;2080"))
        (t (setq cate "MACINTOSH;0")))
  (call-process
   "xattr" nil 0 nil "-w" "com.apple.TextEncoding" cate buffer-file-name))
(add-hook 'after-save-hook 'my-set-com.apple.TextEncoding)

;; smooth scroll を on
(setq mac-mouse-wheel-smooth-scroll t)
;; ホイールスクロールを控えめに
;; cf: http://tjun.org/blog/2009/11/emacs_mouse_whee/
(defun scroll-down-with-lines ()
  "" (interactive) (scroll-down 3))
(defun scroll-up-with-lines ()
  "" (interactive) (scroll-up 3))

(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)
(global-set-key [double-wheel-up] 'scroll-down-with-lines)
(global-set-key [double-wheel-down] 'scroll-up-with-lines)
(global-set-key [triple-wheel-up] 'scroll-down-with-lines)
(global-set-key [triple-wheel-down] 'scroll-up-with-lines)

;; マウスでURLを開ける
(global-set-key [s-mouse-1] 'browse-url-at-mouse)

;; メニューバーを表示
(when window-system
  (menu-bar-mode t))

;; dark mode
(add-to-list 'default-frame-alist '(ns-appearance . 'dark))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(defun dired-open-by-macosx ()
  "Opens a file in dired with the Mac OS X command 'open'."
  (interactive)
  (shell-command (concat "open " (shell-quote-argument (expand-file-name (dired-file-name-at-point))))))

(define-key dired-mode-map (kbd "z") 'dired-open-by-macosx)
;;; cocoa-emacs-init.el ends here
