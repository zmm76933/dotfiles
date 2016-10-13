;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; init.el -- Emacs init setting elisp file
;;
;; $Id$

;;; Commentary:
;;
;; emacs初期設定

;;; Code:
;;
;; for lancher

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; for lancher (package-initialize)
(unless load-file-name
    (cd (getenv "HOME")))

(require 'cl-lib)

(when load-file-name
    (setq-default user-emacs-directory (file-name-directory load-file-name)))

(load (concat user-emacs-directory "init-el-get.el"))

;; load environment variables
(custom-set-variables
   '(exec-path-from-shell-check-startup-files nil))
(exec-path-from-shell-copy-envs '("PATH" "VIRTUAL_ENV" "GOROOT" "GOPATH" "EIJIRO_DIR"))

;;;; setup theme
(load-theme 'syohex t t)
(enable-theme 'syohex)

;; init-loader
(custom-set-variables
   '(init-loader-show-log-after-init 'error-only))
(init-loader-load (concat user-emacs-directory "init-loader"))

;; init.el ends here
