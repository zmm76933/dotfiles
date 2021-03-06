;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; html.el ---  html setting file
;;
;; $Id$

;;; Commentary:
;;
;;

;;; Code:
;;
;; web-mode
(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook 'my/web-mode-hook)

  (custom-set-variables
   '(web-mode-attr-indent-offset nil)
   '(web-mode-markup-indent-offset 2)
   '(web-mode-css-indent-offset 2)
   '(web-mode-code-indent-offset 2)
   '(web-mode-sql-indent-offset 2)
   '(indent-tabs-mode nil)
   '(tab-width 2)
   '(web-mode-auto-close-style 2))
  ;; remap key
  (define-key web-mode-map (kbd "C-c b b") 'web-mode-block-beginning)
  (define-key web-mode-map (kbd "C-c b e") 'web-mode-block-end)
  (define-key web-mode-map (kbd "C-c b k") 'web-mode-block-kill)
  (define-key web-mode-map (kbd "C-c b n") 'web-mode-block-next)
  (define-key web-mode-map (kbd "C-c b p") 'web-mode-block-previous)
  (define-key web-mode-map (kbd "C-c b s") 'web-mode-block-select)

  (define-key web-mode-map (kbd "C-c e b") 'web-mode-element-beginning)
  (define-key web-mode-map (kbd "C-c e c") 'web-mode-element-clone)
  (define-key web-mode-map (kbd "C-c e d") 'web-mode-element-child)
  (define-key web-mode-map (kbd "C-c e e") 'web-mode-element-end)
  (define-key web-mode-map (kbd "C-c e i") 'web-mode-element-content-select)
  (define-key web-mode-map (kbd "C-c e k") 'web-mode-element-kill)
  (define-key web-mode-map (kbd "C-c e n") 'web-mode-element-next)
  (define-key web-mode-map (kbd "C-c e p") 'web-mode-element-previous)
  (define-key web-mode-map (kbd "C-c e r") 'web-mode-element-rename)
  (define-key web-mode-map (kbd "C-c e s") 'web-mode-element-select)
  (define-key web-mode-map (kbd "C-c e t") 'web-mode-element-traverse)
  (define-key web-mode-map (kbd "C-c e u") 'web-mode-element-parent)

  (define-key web-mode-map (kbd "C-c t b") 'web-mode-tag-beginning)
  (define-key web-mode-map (kbd "C-c t e") 'web-mode-tag-end)
  (define-key web-mode-map (kbd "C-c t m") 'web-mode-tag-match)
  (define-key web-mode-map (kbd "C-c t n") 'web-mode-tag-next)
  (define-key web-mode-map (kbd "C-c t p") 'web-mode-tag-previous)
  (define-key web-mode-map (kbd "C-c t s") 'web-mode-tag-select))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hook for `web-mode'."
    (set (make-local-variable 'company-backends)
         '(company-tern company-web-html company-yasnippet company-files)))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Enable JavaScript completion between <script>...</script> etc.
(advice-add 'company-tern :before
            #'(lambda (&rest _)
                (if (equal major-mode 'web-mode)
                    (let ((web-mode-cur-language
                           (web-mode-language-at-pos)))
                      (if (or (string= web-mode-cur-language "javascript")
                              (string= web-mode-cur-language "jsx"))
                          (unless tern-mode (tern-mode))
                        (if tern-mode (tern-mode -1)))))))

(defun my/web-mode-hook ()
  (local-unset-key (kbd "C-c C-b"))
  (local-unset-key (kbd "C-c C-e"))
  (local-unset-key (kbd "C-c C-t")))

;; html-mode
(defun html-mode-insert-br ()
  (interactive)
  (insert "<br />"))

(with-eval-after-load 'sgml-mode
  (define-key html-mode-map (kbd "C-c b") 'html-mode-insert-br))

;; emmet-coding
(dolist (hook '(sgml-mode-hook html-mode-hook web-mode-hook))
  (add-hook 'hook 'emmet-mode))

;; Preview is disable as default
(custom-set-variables
 '(emmet-preview-default nil)
 '(emmet-indentation 2))

;;; html.el ends here
