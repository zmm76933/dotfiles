;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; basic-setting.el --- emacs basic setting file
;;
;; $Id$

;;; Commentary:
;;
;; 利用する環境共通の設定

;;; Code:
;;
;; encoding
(set-language-environment  "Japanese")
(prefer-coding-system 'utf-8-unix)

;; C-h に backspace と同じ処理を割り当てる
(keyboard-translate ?\C-h ?\C-?)

;; Font coloring
(global-font-lock-mode +1)

;; @see http://dan-project.blog.so-net.ne.jp/2012-06-04
(setq backup-directory-alist
  (cons (cons "\\.*$" (expand-file-name "var/backup" user-emacs-directory))
    backup-directory-alist))

;; create auto-save file in ~/.emacs.d/backup
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "var/backup/" user-emacs-directory) t)))

;; ロックファイルを無効
(setq create-lockfiles nil)

;; 自動保存ファイルのリストファイルを無効
(setq auto-save-list-file-prefix nil)

;; 自動保存ファイルの編集で使用したバッファをsaveまたはkillすると削除される
(setq delete-auto-save-files t)

;;ファイルを開く際にシンボリックリンクをすべて辿って解決した絶対パスとして開く
(setq find-file-visit-truename t)

;; コメントスタイルの変更
(setq comment-style 'extra-line)

;; emacsclient を利用するためにサーバ起動
(require 'server)
(unless (server-running-p)
  (server-start))

;; 起動時のmessageを表示しない
(setq inhibit-startup-message t)

;; scratch のメッセージを空にする
(setq initial-scratch-message nil)

;; 巨大ファイルを開く際に警告
(setq large-file-warning-threshold (* 25 1024 1024))

;; インストールしたパッケージを自動的にautoloadしない
(setq package-enable-at-startup nil)

;; 連続でマークを辿れるようにする
(setq set-mark-command-repeat-pop t)

;; テキストクオート
(setq text-quoting-style 'grave)

;; ユーザネーム
(setq user-full-name "Masato Katayama")

;; cursor
(blink-cursor-mode t)

;; モードラインにライン数
(line-number-mode t)

;; モードラインにカラム数
(column-number-mode t)

;; リージョンを kill-ring に入れないで削除できるようにする
(delete-selection-mode t)

;; info for japanese
(auto-compression-mode t)

;; for GC
(setq-default gc-cons-threshold (* gc-cons-threshold 10))

;; echo
(setq-default echo-keystrokes 0)

;; 最終行を示す
(setq-default indicate-empty-lines t
              indicate-buffer-boundaries 'right)

;; TAB はスペース 4 個ぶんを基本
(setq-default tab-width 4)

;; タブ文字を使用しない
(setq-default indent-tabs-mode nil)

;; ビープ恩
(setq-default ring-bell-function #'ignore)

;; クリップボードでコピー＆ペーストできるようにする
(setq x-select-enable-clipboard t)

;; PRIMARY selectionを使う(Windowsでは対象外)
(setq x-select-enable-primary t)

;; クリップボードでコピー・カットした文字列をキルリングにも保存させる
(setq save-interprogram-paste-before-kill t)

;;新規行を作成しない
(setq next-line-add-newlines nil)

;; スクロール時の移動量を1にする
(setq scroll-step 1)

;; 上端、下端における余白幅(初期設定 0)
;; (setq scroll-margin 10)

;; カーソル位置を変更しない
(setq scroll-preserve-screen-position t)

;; shell-mode において最後の行ができるだけウィンドウの一番下にくるようにする
(setq comint-scroll-show-maximum-output t)

;; C-v や M-v した時に以前の画面にあった文字を何行分残すか(初期設定 2)
(setq next-screen-context-lines 4)

;; 大文字小文字を区別
(setq-default case-fold-search nil)

;;動的略語展開で大文字小文字を区別
(setq dabbrev-case-fold-search nil)

;; ファイル名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; ファイル読込み時、大文字小文字を区別しない
(setq read-file-name-completion-ignore-case t)

;; バッファ読込み時、大文字小文字を区別しない
(setq read-buffer-completion-ignore-case t)

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;; シンボリックリンクを開くときの質問省略
(setq vc-follow-symlinks t)

;; imenuの再度入力を省略
(setq imenu-auto-rescan t)

;; 折り返し有効
(setq-default truncate-lines t)

;; ウィンドウを左右に分割したときに行を折り返さない
(setq-default truncate-partial-width-windows t)

;; アクティブなウィンドウだけにカーソルが出るようにする
(setq-default cursor-in-non-selected-windows nil)

;; キータイプ中、マウスカーソルを消す
(setq-default make-pointer-invisible t)

;; タブの削除指定
(setq backward-delete-char-untabify-method 'hungry)

;; ミニバッファを再帰的に呼び出せるようにする
(setq recursive-minibuffers t)

;; 履歴
(setq-default history-length 500)

;; 重複履歴を削除
(setq-default history-delete-duplicates t)

;; undo設定
(setq-default undo-no-redo t)

;; undo上限
(setq-default undo-limit 600000)

;; undo最上限
(setq-default undo-strong-limit 900000)

;; undo-tree
(global-undo-tree-mode)

;; ダイアログを使わないようにする
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;; ドラッグ&ドロップ時のファイルオープン動作
(setq dnd-open-file-other-window nil)

;; Emacs の質問を y/n に
(fset 'yes-or-no-p #'y-or-n-p)

;; fixed line position after scrollup, scrolldown
(defun my/scroll-move-around (orig-fn &rest args)
  (let ((orig-line (count-lines (window-start) (point))))
    (apply orig-fn args)
    (move-to-window-line orig-line)))
(advice-add 'scroll-up :around 'my/scroll-move-around)
(advice-add 'scroll-down :around 'my/scroll-move-around)

;; which-func
(which-function-mode +1)
(setq-default which-func-unknown "")

;; I never use C-x C-c
;; (defalias 'exit 'save-buffers-kill-emacs)

;; Don't disable commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; smart repetition
(require 'smartrep)
(custom-set-variables
 '(smartrep-mode-line-active-bg nil)
 '(smartrep-mode-line-string-activated "<<< SmartRep >>>"))

(add-to-list 'auto-mode-alist '("/\\(?:LICENSE\\|Changes\\)\\'" . text-mode))

(defun my/text-mode-hook ()
  (when (string-prefix-p "Changes" (buffer-name))
    (setq-local company-backends '(company-ispell company-files company-dabbrev))
    (flyspell-mode +1)))
(add-hook 'text-mode-hook 'my/text-mode-hook)

(with-eval-after-load "text-mode"
  (define-key text-mode-map (kbd "C-M-i") 'company-complete))

(custom-set-variables
 '(hippie-expand-verbose nil)
 '(hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-complete-file-name
     try-complete-file-name-partially
     try-expand-dabbrev-all-buffers)))

(custom-set-variables
 '(which-key-lighter "")
 '(which-key-idle-delay 0.5))
(which-key-mode +1)

(winner-mode +1)

;;; basic-setting.el ends here
