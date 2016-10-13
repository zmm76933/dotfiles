;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; yatex.el --- yatex setting file
;;
;; $Id$

;;; Commentary:
;;
;; yatex 関連

;;; Code:
;;
;;
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(when (autoload-if-found 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

  ;; platex and xdvi
  ;;(setq tex-command "ptex -kanji=utf8"
  ;;      dvi2-command "xdvi")
  ;; platex and TeXShop
  (custom-set-variables
   '(tex-command "~/library/TeXShop/bin/platex2pdf-utf8")
   '(dvi2-command "open -a Skim")
   '(dviprint-command-format "dvips %s | lpr")
   '(YaTeX-kanji-code 4) ;; (1 JIS, 2 SJIS, 3 EUC, 4 UTF-8)
   '(YaTeX-use-AMS-LaTeX t)
   '(section-name "documentclass")
   '(makeindex-command "mendex -U")
   '(bibtex-command "jbibtex -kanji=utf8")
   '(YaTeX-skip-default-reader  t)
   '(YaTeX-latex-message-code 'utf-8)
   '(YaTeX-use-font-lock t))

  (add-hook 'skk-mode-hook
            (lambda ()
              (if (eq major-mode 'yatex-mode)
                  (progn
                    (define-key skk-j-mode-map "\\" 'self-insert-command)
                    (define-key skk-j-mode-map "$" 'YaTeX-insert-dollar)
                    ))
              )))

;;; yatex.el ends here
