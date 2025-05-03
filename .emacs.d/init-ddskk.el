(eval-when-compile (require 'skk))

(setq skk-show-candidates-always-pop-to-buffer nil)

(setq skk-henkan-number-to-display-candidates 5)

(setq skk-japanese-message-and-error nil)

(setq default-input-method "japanese-skk")

(setq skk-show-japanese-menu nil)

(setq skk-show-annotation nil)

;; (setq skk-latin-mode-string "[_A]")
;; (setq skk-hiragana-mode-string "[あ]")
;; (setq skk-katakana-mode-string "[ア]")
;; (setq skk-jisx0208-latin-mode-string "[Ａ]")
;; (setq skk-jisx0201-mode-string "[_ｱ]")
;; (setq skk-abbrev-mode-string "[aA]")
;; (setq skk-indicator-use-cursor-color nil)

(setq skk-status-indicator 'minor-mode)

(setq skk-use-color-cursor t)
(setq skk-cursor-hiragana-color "orange")
(setq skk-cursor-katakana-color "green")
(setq skk-cursor-latin-color "cyan")
(setq skk-cursor-jisx0208-latin-color "yellow")
(setq skk-cursor-jisx0201-color "purple")

(setq skk-use-jisx0201-input-method t)

(setq skk-egg-like-newline t)

(setq skk-auto-insert-paren t)

(setq skk-auto-paren-string-alist
      '(
        ("「" . "」")
        ("『" . "』")
        ("（" . "）")
        ("｛" . "｝")
        ("〈" . "〉")
        ("《" . "》")
        ("［" . "］")
        ("〔" . "〕")
        ("【" . "】")))

(setq skk-kuten-touten-alist
      '(
        (jp    . ("。" . "、"))
        (jp-en . ("。" . ", "))
        (en-jp . ("．" . "，"))
        (en    . (". " . ", "))
        ))
(setq-default skk-kutouten-type 'jp)

(setq skk-rom-kana-rule-list
      (append skk-rom-kana-rule-list
              '((";" nil ";")
                (":" nil ":")
                ("!" nil "!")
                ("?" nil "?")
                ("@" nil "@")
                ("$" nil "$")
                ("~" nil "~")
                ("-" nil "ー")
                ("(" nil nil)
                (")" nil ")")
                ("[" nil nil)
                ("]" nil "]")
                ("z;" nil "；")
                ("z:" nil "：")
                ("z!" nil "！")
                ("z?" nil "？")
                ("z~" nil "～")
                ("z-" nil "-")
                ("z(" nil "（")
                ("z)" nil "）")
                ("z[" nil "「")
                ("z]" nil "」")
                ("z{" nil "【")
                ("z}" nil "】")
                ("z<" nil "＜")
                ("z>" nil "＞")
                ("z " nil "　")
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

(setq skk-inhibit-ja-dic-search t)

(setq skk-check-okurigana-on-touroku 'auto)

(setq skk-check-okurigana-on-touroku t)

(setq skk-jisyo-code 'utf-8-unix)

(setq skk-isearch-start-mode 'latin)

;;; Isearch setting.
(defun skk-isearch-setup-maybe ()
  (require 'skk-vars)
  (when (or (eq skk-isearch-mode-enable 'always)
            (and (boundp 'skk-mode)
                 skk-mode
                 skk-isearch-mode-enable))
    (skk-isearch-mode-setup)))

(defun skk-isearch-cleanup-maybe ()
  (require 'skk-vars)
  (when (and (featurep 'skk-isearch)
             skk-isearch-mode-enable)
    (skk-isearch-mode-cleanup)))

(add-hook 'isearch-mode-hook #'skk-isearch-setup-maybe)
(add-hook 'isearch-mode-end-hook #'skk-isearch-cleanup-maybe)
