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

  ;; binding
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "h") 'dired-up-directory)
  (define-key dired-mode-map (kbd "l") 'dired-open-file)
  (define-key dired-mode-map (kbd "K") 'dired-k2)
  (define-key dired-mode-map (kbd "/") 'dired-filter-map)
  (define-key dired-mode-map " " 'quick-preview-at-point)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

(defun return-current-working-directory-to-shell ()
  (expand-file-name
   (with-current-buffer
       (if (featurep 'elscreen)
           (let* ((frame-confs (elscreen-get-frame-confs (selected-frame)))
                  (num (nth 1 (assoc 'screen-history frame-confs)))
                  (cur-window-conf
                   (assoc 'window-configuration
                          (assoc num (assoc 'screen-property frame-confs))))
                  (marker (nth 2 cur-window-conf)))
             (marker-buffer marker))
         (nth 1
              (assoc 'buffer-list
                     (nth 1 (nth 1 (current-frame-configuration))))))
     default-directory)))

(custom-set-variables
 '(ls-lisp-dirs-first t)
 '(dired-listing-switches "-alh")
 '(dired-dwim-target t)
 '(dired-auto-revert-buffer t)
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-isearch-filenames 'dwim)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always))

(autoload 'dired-jump "dired-x" nil t)
(global-set-key (kbd "C-x C-j") #'dired-jump)

;;; dired.el ends here
