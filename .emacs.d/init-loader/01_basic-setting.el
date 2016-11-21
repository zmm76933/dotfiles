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

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key key-translation-map (kbd "<DEL>") (kbd "C-h"))

;; Font coloring
(global-font-lock-mode +1)

;; basic customize variables
(custom-set-variables
 '(auto-save-file-name-transforms `((".*" ,my/backup-dir t)))
 '(backup-directory-alist `((".*" . ,my/backup-dir)))
 '(bookmark-default-file (concat my/history-dir "bookmarks"))
 '(tramp-persistency-file-name (concat my/history-dir "tramps"))
 '(tramp-backup-directory-alist backup-directory-alist)
 '(savehist-file (concat my/history-dir "savehists"))
 '(save-place-file (concat my/history-dir "places"))
 '(create-lockfiles nil)
 '(auto-save-list-file-prefix nil)
 '(find-file-visit-truename t)
 '(comment-style 'extra-line)
 '(inhibit-startup-message t)
 '(initial-scratch-message nil)
 '(large-file-warning-threshold (* 25 1024 1024))
 '(package-enable-at-startup nil)
 '(set-mark-command-repeat-pop t)
 '(text-quoting-style 'grave)
 '(user-full-name "Masato Katayama")
 '(next-line-add-newlines nil)
 '(scroll-step 1)
 '(scroll-preserve-screen-position t)
 '(comint-scroll-show-maximum-output t)
 '(next-screen-context-lines 4)
 '(dabbrev-case-fold-search nil)
 '(completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(read-buffer-completion-ignore-case t)
 '(vc-follow-symlinks t)
 '(imenu-auto-rescan t)
 '(recursive-minibuffers t)
 '(backward-delete-char-untabify-method 'hungry)
 ;; additional
 '(save-kill-file-name (concat my/history-dir "kill-ring-saved"))
 '(undohist-directory (concat my/history-dir "undohist"))
 '(undohist-ignored-files '("/elpa/" "/el-get/" "MERGE_MSG" "COMMIT_EDITMSG")))

;; Don't disable commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(savehist-mode 1)
(save-place-mode +1)

;; cursor
(blink-cursor-mode t)

;; display line infomation
(line-number-mode t)
(column-number-mode t)

;; リージョンを kill-ring に入れないで削除できるようにする
(delete-selection-mode t)

;; info for japanese
(auto-compression-mode t)

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

;; which-func
(which-function-mode +1)
(setq-default which-func-unknown "")

;; 大文字小文字を区別
(setq-default case-fold-search nil)

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

;; 折り返し有効
(setq-default truncate-lines t
              truncate-partial-width-windows t)

;; アクティブなウィンドウだけにカーソルが出るようにする
(setq-default cursor-in-non-selected-windows nil)

;; キータイプ中、マウスカーソルを消す
(setq-default make-pointer-invisible t)

;; history
(setq-default history-length 500
              history-delete-duplicates t)

;; undo setting
(setq-default undo-no-redo t
              undo-limit 600000
              undo-strong-limit 900000)

;; Emacs の質問を y/n に
(fset 'yes-or-no-p #'y-or-n-p)

;; ダイアログを使わないようにする
(setq-default use-dialog-box nil)
(defalias 'message-box 'message)

;; emacsclient を利用するためにサーバ起動
(require 'server)
(unless (server-running-p)
  (server-start))

;; undo-tree
(global-undo-tree-mode)

;; undohist
(require 'undohist)
(undohist-initialize)

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
