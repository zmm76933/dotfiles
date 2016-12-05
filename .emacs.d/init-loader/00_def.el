;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; def.el --- define emacs variables, functions and macros setting file
;;
;; $Id$

;;; Commentary:
;;
;; 編集、関数、マクロの設定

;;; Code:
;;
;;
;; OS type
(defvar darwin-p (eq system-type 'darwin))
(defvar freebsd-p (eq system-type 'gnu/kfreebsd))
(defvar linux-p (eq system-type 'gnu/linux))
(defvar nt-p (eq system-type 'windows-nt))

;; custome path
(defvar my/history-dir (concat user-emacs-directory "var/hist/"))
(defvar my/backup-dir (concat user-emacs-directory "var/backup/"))
(if (not (file-directory-p my/history-dir)) (make-directory my/history-dir))
(if (not (file-directory-p my/backup-dir))  (make-directory my/backup-dir))

;; Ctrl-q map
(defvar my/ctrl-q-map (make-sparse-keymap)
  "My original keymap binded to C-q.")
(defalias 'my/ctrl-q-prefix my/ctrl-q-map)

;; smartparens
(defun editutil-smartparens-backward-delete ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'sp-backward-delete-char)))

;; @see https://github.com/hottestseason/dotemacs/blob/master/init.d/init-programming.el
(defun newline-and-insert-newline-and-indent-after-brace ()
  (interactive)
  (if (and (eq (char-before) ?{) (eq (following-char) ?}))
      (progn
        (newline-and-indent)
        (save-excursion
          (insert "\n")
          (indent-according-to-mode))
        (indent-according-to-mode))
    (newline-and-indent)))

;; @see http://www.sodan.org/~knagano/emacs/dotemacs.html
;; @see http://e-arrows.sakura.ne.jp/2010/03/macros-in-emacs-el.html
(defmacro defun-add-hook (hookname &rest sexplist)
  "add-hook のエイリアス。引数を関数にパックして hook に追加する。"
  `(add-hook ,hookname (function (lambda () ,@sexplist))))

(defmacro req (lib &rest body)
  `(when (locate-library ,(symbol-name lib))
     (require ',lib) ,@body))

(defmacro eval-safe (&rest body)
  "安全な評価。評価に失敗してもそこで止まらない。"
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))

(defun load-safe (loadlib)
  "安全な load。読み込みに失敗してもそこで止まらない。"
  ;; missing-ok で読んでみて、ダメならこっそり message でも出しておく
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))

(defun autoload-if-found (functions file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (if (not (listp functions))
      (setq functions (list functions)))
  (and (locate-library file)
       (progn
         (dolist (function functions)
           (autoload function file docstring interactive type))
         t )))

;; window の移動
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

;; fullscreen を toggle する
(defun toggle-fullscreen ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)
    ))

;; find-file-sudo
(defun file-root-p (filename)
  "Return t if file FILENAME created by root."
  (eq 0 (nth 2 (file-attributes filename))))

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (file-root-p (ad-get-arg 0))
           (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;; linum-off
(defcustom linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode doc-view-mode image-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum
  )

(defadvice linum-on(around linum-off activate)
  (unless (or (minibufferp)
              (member major-mode linum-disabled-modes-list)
              (string-match "*" (buffer-name))
              (> (buffer-size) 3000000))
    (linum-mode 1)))

(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

;;; def.el ends here
