(eval-when-compile (require 'skk))

(setq skk-sticky-key ";")

(setq skk-show-candidates-always-pop-to-buffer nil)

(setq skk-henkan-number-to-display-candidates 5)

(setq skk-japanese-message-and-error nil)

(setq skk-show-japanese-menu nil)

(setq skk-show-annotation nil)

;; (setq skk-latin-mode-string "[_A]")
;; (setq skk-hiragana-mode-string "[あ]")
;; (setq skk-katakana-mode-string "[ア]")
;; (setq skk-jisx0208-latin-mode-string "[Ａ]")
;; (setq skk-jisx0201-mode-string "[_ｱ]")
;; (setq skk-abbrev-mode-string "[aA]")
;; (setq skk-indicator-use-cursor-color nil)

(setq skk-status-indicator 'left)

(setq skk-use-color-cursor nil)

(global-set-key "\C-\\" 'skk-mode)

(setq skk-use-jisx0201-input-method t)

(setq skk-egg-like-newline t)

(setq skk-auto-insert-paren t)

(setq skk-kuten-touten-alist
      '(
        (jp    . ("。" . "、"))
        (jp-en . ("。" . ", "))
        (en-jp . ("．" . "，"))
        (en    . (". " . ", "))
        ))
(setq-default skk-kutouten-type 'en)

(setq skk-rom-kana-rule-list
      (append skk-rom-kana-rule-list
              '(("!" nil "!")
                (":" nil ":")
                (";" nil ";")
                ("?" nil "?")
                ("z " nil "　")
                ("\\" nil "\\")
                ("@" nil "@")
                )))

(setq skk-henkan-strict-okuri-precedence t)

(setq skk-share-private-jisyo t)

;; (setq skk-show-inline 'vertical)
(setq skk-show-inline nil)

;; ddskk <- yaskkserv2 のみ utf-8 で通信するための設定
(defun my:skk-open-server-decoding-utf-8 ()
  "辞書サーバと接続する。サーバープロセスを返す。 decoding coding-system が euc ではなく utf8 となる。"
  (unless (skk-server-live-p)
    (setq skkserv-process (skk-open-server-1))
    (when (skk-server-live-p)
      (let ((code (cdr (assoc "euc" skk-coding-system-alist))))
        (set-process-coding-system skkserv-process
                                   'utf-8 code)))) skkserv-process)
(setq skk-mode-hook
      (lambda ()
        (advice-add 'skk-open-server :override 'my:skk-open-server-decoding-utf-8)))
(cond
 ((getenv "SKKSERVER")
  (setq skk-server-host (getenv "SKKSERVER")
        skk-server-portnum "1178"
        skk-large-jisyo nil)
  (add-to-list 'skk-search-prog-list
               '(skk-server-completion-search) t)
  (add-to-list 'skk-search-prog-list
               '(skk-comp-by-server-completion) t))
 (t
  (setq skk-get-jisyo-directory (concat my:d:tmp "skk-jisyo")
        skk-large-jisyo (concat skk-get-jisyo-directory "/SKK-JISYO.L"))
  (when (file-exists-p "/usr/local/share/skkdic/SKK-JISYO.emoji.utf8")
    (setq skk-extra-jisyo-file-list
          (list '("/usr/local/share/skkdic/SKK-JISYO.emoji.utf8" . utf-8)))))
 )

(setq skk-check-okurigana-on-touroku 'auto)

(setq skk-check-okurigana-on-touroku t)

(setq skk-jisyo-code 'utf-8-unix)

(add-hook 'skk-mode-hook
          (lambda ()
            (and (skk-in-minibuffer-p)
                 (skk-mode-exit))))
(setq skk-isearch-start-mode 'latin)
