;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; navi2ch.el --- navi2ch setting file
;;
;; $Id$

;;; Commentary:
;;
;; 2ちゃんねる廃人への道
;; navi2ch
;; @see http://navi2ch.sourceforge.net/

;;; Code:
;;
;;
(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)
(autoload 'navi2ch-localfile "navi2ch-localfile" nil t)

(custom-set-variables
 ;;'(navi2ch-list-bbstable-url "http://www.2ch.net/bbsmenu.html")
 ;;'(navi2ch-list-bbstable-url "http://www6.ocn.ne.jp/~mirv/2chmenu.html")
 ;;'(navi2ch-list-bbstable-url "http://azlucky.s25.xrea.com/2chboard/bbsmenu.html")
 '(navi2ch-list-bbstable-url "http://menu.2ch.net/bbsmenu.html")
 '(navi2ch-list-stay-list-window t)
 '(navi2ch-article-new-message-range '(1000 . 1))
 '(navi2ch-article-exist-message-range '(1 . 1000))
 '(navi2ch-article-auto-range nil)
 '(navi2ch-list-stay-list-window t)
 '(navi2ch-ask-when-exit nil)
 '(navi2ch-message-user-name "")
 '(navi2ch-message-ask-before-send nil)
 '(navi2ch-message-ask-before-kill nil)
 '(navi2ch-article-max-buffers 5)
 '(navi2ch-article-auto-expunge t)
 '(navi2ch-board-insert-subject-with-diff t)
 '(navi2ch-board-insert-subject-with-unread t)
 '(navi2ch-mona-enable t)
 '(navi2ch-mona-on-message-mode t)
 '(navi2ch-mona-use-ipa-mona t)
 '(navi2ch-mona-ipa-mona-font-family-name "IPA モナー Pゴシック"))

;;(require 'navi2ch-mona)
;;(add-hook 'navi2ch-article-arrange-message-hook
;;         'navi2ch-mona-arrange-message)
;;(setq navi2ch-mona-enable t)

;; bookmark で未読レスがあるスレッドを上に
;; bookmark 表示と同時 -> 重い
;; (defadvice navi2ch-bookmark (after fetch-and-sort)
;;   (navi2ch-bm-fetch-maybe-new-articles))
;; (ad-activate 'navi2ch-bookmark)
;; タイミングを明示
(defadvice navi2ch-bm-fetch-maybe-new-articles
  (after my-navi2ch-sort)
  (navi2ch-bm-sort-by-state))
(ad-activate 'navi2ch-bm-fetch-maybe-new-articles)

;; 連続改行を消す
;; http://web.archive.org/web/20041212084758/http://reed1200.at.infoseek.co.jp/navi2ch/
(defun navi2ch-article-shrink-newlines ()
  (save-excursion
    (while (re-search-forward "^[ 　]*\n[ 　]*\n[ 　]*\n\\([ 　]*\n\\)+" nil t)
      (let ((len (count-lines (match-beginning 0) (match-end 0))))
        (when (>= len 8)
          (replace-match (format "\n[[ %d newlines ]]\n\n" len)))))))

(add-hook 'navi2ch-article-arrange-message-hook
          'navi2ch-article-shrink-newlines)

;; 勢いソート
;; http://my.opera.com/hirohiso/blog/2010/11/27/navi2ch
(defadvice navi2ch-bm-sort (around my-navi2ch-bm-sort)
  (interactive "P")
  (let ((ch (navi2ch-read-char-with-retry
             "Sort by n)umber s)tate t)itle o)ther d)ate? i)kioi?"
             nil '(?n ?s ?t ?o ?d ?i))))
    (message "Sorting...")
    (funcall
     (cond ((eq ch ?n) 'navi2ch-bm-sort-by-number)
           ((eq ch ?s) 'navi2ch-bm-sort-by-state)
           ((eq ch ?t) 'navi2ch-bm-sort-by-subject)
           ((eq ch ?o) 'navi2ch-bm-sort-by-other)
           ((eq ch ?d) 'navi2ch-bm-sort-by-date)
           ((eq ch ?i) 'navi2ch-bm-sort-by-ikioi))
     arg)
    (message "Sorting...done")))
(ad-activate 'navi2ch-bm-sort)

(defun navi2ch-bm-sort-by-ikioi (&optional rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   (not rev)
   (lambda ()
     (let* ((curtime (current-time))
            (unixtime (+ (* (car curtime) 65536.0) (cadr curtime)))
            (createtime
             (string-to-number
              (cdr
               (assq 'artid (navi2ch-bm-get-article-internal
                             (navi2ch-bm-get-property-internal (point)))))))
            (exittime (- unixtime createtime))
            (resnum (string-to-number
                     (cdr (assq 'response
                                (navi2ch-bm-get-article-internal
                                 (navi2ch-bm-get-property-internal (point))))))))
       (setq resnum (* resnum resnum))
       (/ (* resnum 100) exittime))) nil))

;; startup
(add-hook 'navi2ch-after-startup-hook
          (lambda ()
            (navi2ch-three-pane)))

;;; navi2ch.el ends here
