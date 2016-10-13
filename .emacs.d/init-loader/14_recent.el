;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; recent.el --- recentf setting file
;;
;; %Id$

;;; Commentary:
;;
;;

;;; Code:
;;
;; @see http://masutaka.net/chalow/2011-10-30-2.html
(defvar my-recentf-list-prev nil)

;; 保存時にメッセージの出力を抑止
(defadvice recentf-save-list
    (around no-message activate)
  "If `recentf-list' and previous recentf-list are equal,
do nothing. And suppress the output from `message' and
`write-file' to minibuffer."
  (unless (equal recentf-list my-recentf-list-prev)
    (cl-flet ((message (format-string &rest args)
                       (eval `(format ,format-string ,@args)))
              (write-file (file &optional confirm)
                          (let ((str (buffer-string)))
                            (with-temp-file file
                              (insert str)))))
      ad-do-it
      (setq my-recentf-list-prev recentf-list))))

;; クリーンアップ時のメッセージ出力を抑止
(defadvice recentf-cleanup
    (around no-message activate)
  "suppress the output from `message' to minibuffer"
  (cl-flet ((message (format-string &rest args)
                     (eval `(format ,format-string ,@args))))
    ad-do-it))

(custom-set-variables
 '(recentf-max-saved-items 2000)
 '(recentf-auto-cleanup 600)
 '(recentf-exclude '(".recentf" "/elpa/" "/elisps/" "/site-lisp/" "\\`/tmp/" "/\\.git/" "/\\.cask/"
                     "/tmp/gomi/" "/el-get/" ".loaddefs.el" "/\\.cpanm/"
                     "\\.mime-example" "\\.ido.last" "woman_cache.el"
                     "\\`/proc/" "\\`/sys/"
                     "CMakeCache.txt" "/bookmarks" "\\.gz$"
                     "COMMIT_EDITMSG" "MERGE_MSG" "git-rebase-todo"))
 `(recentf-save-file ,(concat user-emacs-directory "var/recentf/recentf.cache")))

(run-at-time t 600 #'editutil-recentf-save-list)
(recentf-mode 1)

;;; recent.el ends here
