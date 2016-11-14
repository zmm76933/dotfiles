;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; global-keys.el --- global keybind setting file
;;
;; $Id$

;;; Commentary:
;;
;; keybind

;;; Code:
;;
;; editutil mappings
(editutil-default-setup)
(global-set-key (kbd "C-x c") ctl-x-4-map)
(global-set-key (kbd "C-x c j") 'dired-jump-other-window)
(global-set-key (kbd "C-x c y") 'clipboard-yank)

;; デフォルトのスクロールキーを変更
(global-set-key (kbd "C-z") 'scroll-down)

;; "{"の後の改行インデント
(global-set-key (kbd "RET") 'newline-and-insert-newline-and-indent-after-brace)

;; fullscreen を toggle する
(global-set-key (kbd "C-c m") 'toggle-fullscreen)

;; popwin
(global-set-key (kbd "M-z") popwin:keymap)

;; gtag
(global-set-key (kbd "M-,") 'pop-tag-mark)

;; gtag
(global-set-key (kbd "M-*") 'tags-loop-continue)

;; next buffer
(global-set-key (kbd "M-[") 'bs-cycle-next)

;; previous buffer
(global-set-key (kbd "M-]") 'bs-cycle-previous)

;; window の移動
(global-set-key (kbd "M-`") 'editutil-other-window)

;; insert parentheses like M-1 M-(
(global-set-key (kbd "M-(") 'my/insert-parentheses)

;; yas
(global-set-key (kbd "M-=") 'yas-insert-snippet)

;; 行移動
(global-set-key (kbd "C-M-l") 'goto-line)

;; undo-tree
(global-set-key (kbd "C-M-/") 'undo-tree-redo)

;; 別ウインドウの下スクロール
(global-set-key (kbd "C-M-z") 'scroll-other-window-down)

;; backward kill
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;; exchange point and mark
(global-set-key (kbd "C-x C-x") 'exchange-point-and-mark)

;; revert buffer
(global-set-key (kbd "C-x RET R") 'revert-buffer)

;; helm
(global-set-key (kbd "C-;") 'helm-mini)
(global-set-key (kbd "C-'") 'helm-resume)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Ctrl-q map
(define-key global-map (kbd "C-q") 'my/ctrl-q-prefix)
(define-key my/ctrl-q-map (kbd "C-q") 'quoted-insert)
(define-key my/ctrl-q-map (kbd "C-c") 'column-highlight-mode)
(define-key my/ctrl-q-map (kbd "C-a") 'text-scale-adjust)
(define-key my/ctrl-q-map (kbd "C-f") 'flyspell-mode)
(define-key my/ctrl-q-map (kbd "C-m") 'flycheck-mode)

;; M-g mapping
(global-set-key (kbd "M-g .") 'helm-ag)
(global-set-key (kbd "M-g ,") 'helm-ag-pop-stack)
(global-set-key (kbd "M-g p") 'helm-ag-project-root)
(global-set-key (kbd "M-g s") 'helm-swoop)
(global-set-key (kbd "M-g M-f") 'ffap)
(global-set-key (kbd "M-g M-w") 'ffap-copy-string-as-kill)
(global-set-key (kbd "M-g M-t") 'ff-find-other-file)
(global-set-key (kbd "M-g r") #'recompile)
(global-set-key (kbd "M-g q") #'quickrun)

(smartrep-define-key
 global-map "M-g" '(("-" . 'goto-last-change)
                    ("+" . 'goto-last-change-reverse)))

;; undo tree
(smartrep-define-key
 undo-tree-map "C-x" '(("u" . 'undo-tree-undo)
                       ("U" . 'undo-tree-redo)))

;;; buffer-move
(global-set-key (kbd "M-g h") 'buf-move-left)
(global-set-key (kbd "M-g j") 'buf-move-down)
(global-set-key (kbd "M-g k") 'buf-move-up)
(global-set-key (kbd "M-g l") 'buf-move-right)

;;; global-keys.el ends here
