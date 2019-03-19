;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; paren.el --- parenthesis setting file
;;
;; $Id$

;;; Commentary:
;;
;;

;;; Code:
;;
;;
(show-paren-mode 1)
(custom-set-variables
 '(show-paren-delay 0)
 '(show-paren-style 'expression)
 '(parens-require-spaces nil))

;; smart parenthesis
(smartparens-global-mode t)
(show-smartparens-global-mode t)

(with-eval-after-load 'smartparens
  (define-key smartparens-mode-map (kbd "C-c l") 'editutil-toggle-let)
  (define-key smartparens-mode-map (kbd "C-c j") 'editutil-newline-after-sexp)
  (define-key smartparens-mode-map (kbd "DEL") 'editutil-smartparens-backward-delete))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-pair "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

;; @See https://github.com/Fuco1/smartparens/issues/286
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
  (sp-local-pair "`" nil
                 :skip-match (lambda (ms mb me)
                               (cond
                                ((equal ms "'")
                                 (or (sp--org-skip-markup ms mb me)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))

;;; paren.el ends here
