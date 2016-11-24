;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; view_mode.el --- view mode setting
;;
;; $Id$


;;; Commentary:
;;
;; view-mode
(custom-set-variables
 '(view-read-only t))

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
  (define-key view-mode-map (kbd "?") 'View-search-regexp-backward)
  (define-key view-mode-map (kbd "g") 'View-goto-line)
  (define-key view-mode-map (kbd "w") 'forward-word)
  (define-key view-mode-map (kbd "W") 'forward-symbol)
  (define-key view-mode-map (kbd "b") 'backward-word)
  (define-key view-mode-map (kbd "h") 'backward-char)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map (kbd "l") 'forward-char)
  (define-key view-mode-map (kbd "[") 'backward-paragraph)
  (define-key view-mode-map (kbd "]") 'forward-paragraph)
  (define-key view-mode-map (kbd "C-f") 'View-scroll-page-forward)
  (define-key view-mode-map (kbd "C-b") 'View-scroll-page-backward)
  (define-key view-mode-map (kbd "C-d") 'View-scroll-half-page-forward)
  (define-key view-mode-map (kbd "C-u") 'View-scroll-half-page-backward))

(with-eval-after-load 'doc-view
  (define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page))

;;; view_mode.el ends here
