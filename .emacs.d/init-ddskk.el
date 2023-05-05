(eval-when-compile (require 'skk))

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
                ("z<" nil "＜")
                ("z>" nil "＞")
                ("\\" nil "\\")
                ("@" nil "@")
                )))

(setq skk-henkan-strict-okuri-precedence t)

(setq skk-share-private-jisyo t)

;; (setq skk-show-inline 'vertical)
(setq skk-show-inline nil)

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
        skk-large-jisyo (concat skk-get-jisyo-directory "/SKK-JISYO.L")))
 )

(setq skk-check-okurigana-on-touroku 'auto)

(setq skk-check-okurigana-on-touroku t)

(setq skk-jisyo-code 'utf-8-unix)

(add-hook 'skk-mode-hook
          (lambda ()
            (and (skk-in-minibuffer-p)
                 (skk-mode-exit))))
(setq skk-isearch-start-mode 'latin)
