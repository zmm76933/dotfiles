;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; dired.el --- dired setting file
;;
;; $Id$

;;; Commentary:
;;
;; dired 関連

;;; Code:
;;
;;
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'dired-filter-mode)
  ;; Not create new buffer, if you chenge directory in dired
  (put 'dired-find-alternate-file 'disabled nil)

  (when (executable-find "gls")
    (setq insert-directory-program "gls"))

  (load-library "ls-lisp")

  (defun kill-current-buffer-and/or-dired-open-file ()
    "In Dired, dired-open-file for a file. For a directory, dired-find-file and kill previously selected buffer."
    (interactive)
    (if (file-directory-p (dired-get-file-for-visit))
        (dired-find-alternate-file)
      (dired-open-file)))

  (defun kill-current-buffer-and-dired-up-directory (&optional other-window)
    "In Dired, dired-up-directory and kill previously selected buffer."
    (interactive "P")
    (let ((b (current-buffer)))
      (dired-up-directory other-window)
      (kill-buffer b)))

  ;; binding
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "h") 'kill-current-buffer-and-dired-up-directory)
  (define-key dired-mode-map (kbd "l") 'kill-current-buffer-and/or-dired-open-file)
  (define-key dired-mode-map (kbd "K") 'dired-k2)
  (define-key dired-mode-map (kbd "/") 'dired-filter-map)
  (define-key dired-mode-map " " 'quick-preview-at-point)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

(custom-set-variables
 '(ls-lisp-dirs-first t)
 '(dired-listing-switches "-alh")
 '(dired-dwim-target t)
 '(dired-auto-revert-buffer t)
 '(dired-isearch-filenames 'dwim)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always))

(autoload 'dired-jump "dired-x" nil t)
(global-set-key (kbd "C-x C-j") #'dired-jump)

;;; dired.el ends here
