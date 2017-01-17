;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; helm.el --- helm setting file
;;
;; $Id$

;;; Commentary:
;;
;;

;;; Code:
;;
;;
(custom-set-variables
 '(helm-input-idle-delay 0)
 '(helm-exit-idle-delay 0)
 '(helm-candidate-number-limit 500)
 '(helm-ag-insert-at-point 'symbol)
 '(helm-find-files-doc-header "")
 '(helm-command-prefix-key nil))

(with-eval-after-load 'helm
  (helm-descbinds-mode +1)
  (helm-migemo-mode +1)

  (define-key helm-map (kbd "C-p")   #'helm-previous-line)
  (define-key helm-map (kbd "C-n")   #'helm-next-line)
  (define-key helm-map (kbd "C-z")   #'helm-previous-page)
  (define-key helm-map (kbd "C-M-z") #'helm-scroll-other-window-down)
  (define-key helm-map (kbd "C-M-n") #'helm-next-source)
  (define-key helm-map (kbd "C-M-p") #'helm-previous-source))

(with-eval-after-load 'helm-files
  (remove-hook 'post-self-insert-hook 'helm-find-files--reset-level-tree)

  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "C-w") 'editutil-backward-delete-word)
  (define-key helm-find-files-map (kbd "C-M-u") #'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "C-M-d") #'helm-find-files-down-last-level)
  (define-key helm-find-files-map (kbd "C-c C-o") #'helm-ff-run-switch-other-window))

;;; helm.el ends here
