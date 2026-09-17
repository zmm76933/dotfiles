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

;; かなモードの入力で (モード変更を行なわずに) 長音(ー)を
;; ASCII 数字の直後では `-' に、全角数字の直後では `−' にしたい。
(setq skk-rom-kana-rule-list
      (cons '("-" nil my:skk-hyphen)
            skk-rom-kana-rule-list))
(defun my:skk-hyphen (arg)
  (let ((c (char-before (point))))
    (cond ((null c) "ー")
          ((and (<= ?0 c) (>= ?9 c)) "-")
          ((and (<= ?０ c) (>= ?９ c)) "−")
          (t "ー"))))

;; かなモードの入力でモード変更を行わずに、数字入力中の
;; 小数点 (.) およびカンマ (,) 入力を実現する。
;; (例) かなモードのまま 1.23 や 1,234,567 などの記述を行える。
;; period
(setq skk-rom-kana-rule-list
      (cons '("." nil my:skk-period)
            skk-rom-kana-rule-list))
(defun my:skk-period (arg)
  (let ((c (char-before (point))))
    (cond ((null c) "。")
          ((and (<= ?0 c) (>= ?9 c)) ".")
          ((and (<= ?０ c) (>= ?９ c)) "．")
          (t "。"))))

;; comma
(setq skk-rom-kana-rule-list
      (cons '("," nil my:skk-comma)
            skk-rom-kana-rule-list))
(defun my:skk-comma (arg)
  (let ((c (char-before (point))))
    (cond ((null c) "、")
          ((and (<= ?0 c) (>= ?9 c)) ",")
          ((and (<= ?０ c) (>= ?９ c)) "，")
          (t "、"))))

(defun my:skk-today (&optional _arg)
  "今日の日付を YYYY/MM/DD 形式で返す。`skk-rom-kana-rule-list' の出力用。"
  (format-time-string "%Y/%m/%d"))

(setq skk-rom-kana-rule-list
      (append skk-rom-kana-rule-list
              '((";" nil ";")
                (":" nil ":")
                ("!" nil "!")
                ("?" nil "?")
                ("@" nil "@")
                ("$" nil "$")
                ("~" nil "~")
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
                ("/today" nil my:skk-today)
                )))

(setq skk-henkan-strict-okuri-precedence t)

(setq skk-share-private-jisyo t)

;; (setq skk-show-inline 'vertical)
(setq skk-show-inline nil)

(setq skk-get-jisyo-directory (concat my:d:tmp "skk")
      skk-large-jisyo (concat skk-get-jisyo-directory "/SKK-JISYO.L"))

(when (getenv "SKKSERVER")
  (setq skk-server-host (getenv "SKKSERVER")
        skk-server-portnum "1178")
  (add-to-list 'skk-search-prog-list '(skk-server-completion-search) t)
  (add-to-list 'skk-search-prog-list '(skk-comp-by-server-completion) t))

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

;;; ------------------------------------------------------------------
;;; macSKK との辞書共有
;;;
;;; macSKK は App Sandbox のためコンテナ外の辞書を読めない（シンボリック
;;; リンクも不可）。一方 ddskk 側には制約がないので、
;;;   - ddskk の個人辞書を macSKK の辞書フォルダ内に置く（コピー同期が不要）
;;;   - macSKK の個人辞書は ddskk から直接参照する
;;; という非対称な構成で語彙を双方向に共有する。書き込み先は互いに分離した
;;; ままにしておくこと。同一ファイルに両者が書くと、macSKK が自身のメモリ
;;; 内容で定期的に上書きするため ddskk の登録が消える。
;;;
;;; SKK-JISYO.L は yaskkserv2（SKKSERVER）を両者が参照して共有する。

(defconst my:macskk-dict-dir
  (expand-file-name
   "Library/Containers/net.mtgto.inputmethod.macSKK/Data/Documents/Dictionaries/"
   (getenv "HOME"))
  "macSKK の辞書フォルダ。")

;; ddskk の個人辞書。ファイル名に utf8 を含めると macSKK が UTF-8 と判定する。
(setq skk-jisyo (cons (expand-file-name "ddskk-jisyo.utf8" my:macskk-dict-dir)
                      'utf-8))
;; ddskk は既定では Emacs 終了時にしか個人辞書を保存しない。macSKK は辞書
;; ファイルの外部更新を自動で検知するので、保存さえすれば即座に反映される。
(run-with-idle-timer 600 t (lambda () (skk-save-jisyo 'quiet)))
