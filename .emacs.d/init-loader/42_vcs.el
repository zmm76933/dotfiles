;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; vcs.el --- version control system setting file
;;
;; $Id$

;;; Commentary:
;;
;; git関連

;;; Code:
;;
;;;; Common VCS setting
(custom-set-variables
 '(auto-revert-check-vc-info t)
 '(auto-revert-mode-text ""))
(global-auto-revert-mode 1)

;; disable vc-mode
(custom-set-variables
 '(vc-handled-backends '(Git))
 '(vc-follow-symlinks t))
(with-eval-after-load 'vc
  '(remove-hook 'find-file-hooks #'vc-find-file-hook))

;; vc
(global-set-key (kbd "C-x v d") #'vc-diff)

;; git-gutter
(global-git-gutter2-mode +1)
(global-set-key (kbd "C-x v u") #'git-gutter2-update)
(global-set-key (kbd "C-x v U") #'git-gutter2-cached)
(global-set-key (kbd "C-x v p") #'git-gutter2-stage-hunk)
(global-set-key (kbd "C-x v =") #'git-gutter2-popup-hunk)
(global-set-key (kbd "C-x v r") #'git-gutter2-revert-hunk)
(global-set-key (kbd "C-x v e") #'git-gutter2-end-of-hunk)

(custom-set-variables
 '(git-gutter2:verbosity 4)
 '(git-gutter2:modified-sign " ")
 '(git-gutter2:deleted-sign " "))

(add-hook 'focus-in-hook #'git-gutter2-update-all-windows)

(smartrep-define-key
    global-map  "C-x" '(("p" . 'git-gutter2:previous-hunk)
                        ("n" . 'git-gutter2:next-hunk)))

;; magit
 (custom-set-variables
  '(git-commit-fill-column 80)
  '(git-commit-summary-max-length 72)
  '(magit-auto-revert-mode-lighter "")
  '(magit-push-always-verify nil))

  (defun my/magit-status-around (orig-fn &rest args)
   (window-configuration-to-register :magit-fullscreen)
   (apply orig-fn args)
   (delete-other-windows))

  (global-set-key (kbd "M-g M-g") 'magit-status)

  (with-eval-after-load 'magit
   (global-git-commit-mode +1)
   (advice-add 'magit-status :around 'my/magit-status-around)

    (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session))

  (defun my/magit-quit-session ()
   (interactive)
   (kill-buffer)
   (jump-to-register :magit-fullscreen)
   (git-gutter:update-all-windows))

  (defun my/git-commit-commit-after (_unused)
   (delete-window))

  (defun my/git-commit-mode-hook ()
   (setq-local company-backends '(company-ispell company-files company-dabbrev))
   (flyspell-mode +1))

  (with-eval-after-load 'git-commit
   (add-hook 'git-commit-mode-hook 'my/git-commit-mode-hook)
   (advice-add 'git-commit-commit :after 'my/git-commit-commit-after))

;; git-messenger
(global-set-key (kbd "C-x v m") #'git-messenger2:popup-message)

;;; vcs.el ends here
