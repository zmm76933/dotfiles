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
(global-set-key (kbd "C-x c y") 'clipboard-yank)

;; global key setting(standard)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-o") 'open-line-next-indent)
(global-set-key (kbd "C-z") 'scroll-down)
(global-set-key (kbd "C-M-l") 'goto-line)
(global-set-key (kbd "C-M-z") 'scroll-other-window-down)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-x C-x") 'exchange-point-and-mark)
(global-set-key (kbd "C-x RET R") 'revert-buffer)
(global-set-key (kbd "C-x RET M") 'toggle-fullscreen)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; global key setting (additional)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-*") 'tags-loop-continue)
(global-set-key (kbd "M-=") 'yas-insert-snippet)
(global-set-key (kbd "M-'") 'avy-goto-word-1)
(global-set-key (kbd "C-x z") 'zoom-window-zoom)
(global-set-key (kbd "C-M-/") 'undo-tree-redo)

;; helm
(global-set-key (kbd "C-;") 'helm-mini)
(global-set-key (kbd "C-`") 'helm-resume)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-M-s") 'helm-swoop)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "C-h m") 'helm-man-woman)
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
(global-set-key (kbd "M-g M-f") 'ffap)
(global-set-key (kbd "M-g M-w") 'ffap-copy-string-as-kill)
(global-set-key (kbd "M-g M-t") 'ff-find-other-file)
(global-set-key (kbd "M-g r") #'recompile)
(global-set-key (kbd "M-g q") #'quickrun2)

(smartrep-define-key
 global-map "M-g" '(("-" . 'goto-last-change)
                    ("+" . 'goto-last-change-reverse)))

;;; buffer-move
(global-set-key (kbd "M-g h") 'buf-move-left)
(global-set-key (kbd "M-g j") 'buf-move-down)
(global-set-key (kbd "M-g k") 'buf-move-up)
(global-set-key (kbd "M-g l") 'buf-move-right)

;;; global-keys.el ends here
