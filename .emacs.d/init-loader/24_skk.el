;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; skk.el --- SKK setting file
;;
;; $Id$

;;; Commentary:
;;
;; SKK の設定ファイル

;;; Code:
;;
;; 以下を記述するだけで必要な物は全部ロードする
(custom-set-variables
 `(skk-user-directory ,(concat user-emacs-directory "var/ddskk"))
 '(skk-preload t)
 '(default-input-method "japanese-skk")
 '(skk-server-host "localhost")
 '(skk-server-portnum 1178)
 '(skk-jisyo-code 'utf-8-unix)
 '(skk-auto-insert-paren t)
 '(skk-henkan-strict-okuri-precedence t)
 '(skk-check-okurigana-on-touroku t)
 '(skk-show-inline t)
 '(skk-show-inline 'vertical)
 '(skk-isearch-start-mode 'latin)
 '(skk-dcomp-activate t)
 '(skk-dcomp-multiple-activate t)
 '(skk-dcomp-multiple-rows 5)
 '(skk-comp-use-prefix t)
 '(skk-comp-circulate t))

;; 全角記号の変換
(add-hook 'skk-load-hook
          (lambda ()
            (setq skk-rom-kana-rule-list
                  (nconc skk-rom-kana-rule-list
                         '((";" nil nil)
                           (":" nil nil)
                           ("?" nil nil)
                           ("!" nil nil)
                           ("\\" nil nil))))))

;; yatex編集時、句読点を変更
(add-hook 'yatex-mode-hook
          (lambda ()
            (setq skk-kutouten-type 'en)))

;; C-x C-fでファイルを開くとSKK
(add-hook 'find-file-hooks
          (lambda ()
            (skk-latin-mode t)
            ))

;;; Isearch設定
(add-hook 'isearch-mode-hook
          #'(lambda ()
              (when (and (boundp 'skk-mode)
                         skk-mode
                         skk-isearch-mode-enable)
                (skk-isearch-mode-setup))))

(add-hook 'isearch-mode-end-hook
          #'(lambda ()
              (when (and (featurep 'skk-isearch)
                         skk-isearch-mode-enable)
                (skk-isearch-mode-cleanup))))

;; migemo を使うので skk-isearch にはおとなしくしていて欲しい
(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
(add-hook 'isearch-mode-hook 'skk-isearch-mode-cleanup)

;;  lisp-interaction-modeにおいて英数モードを回避する
(defadvice skk-latin-mode (after no-latin-mode-in-lisp-interaction activate)
  "`lisp-interaction-mode' において英数モードを回避する"
  (when (eq major-mode 'lisp-interaction-mode)
    (skk-mode-off)))

;;; skk.el ends here
