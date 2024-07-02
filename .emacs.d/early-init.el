;; -*- lexical-binding: t -*-
;; (require 'profiler)    ;; 必要に応じて有効化
;; (profiler-start 'cpu)

(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil
      package-quickstart nil)
;;
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(menu-bar-lines       . nil) default-frame-alist)
(push '(tool-bar-lines       . nil) default-frame-alist)
(push '(scroll-bar-mode      . nil) default-frame-alist)
(push '(blink-cursor-mode    . nil) default-frame-alist)
(push '(column-number-mode   . nil) default-frame-alist)
;;
(setq load-prefer-newer noninteractive)
;;
(setq frame-inhibit-implied-resize t)
;;
(setq site-run-file nil)
;; (setq file-name-handler-alist nil) ;; 🤔
(setq use-file-dialog nil)
;;
;; (setq native-comp-speed 2
;;       native-compile-target-directory (expand-file-name "eln-cache" user-emacs-directory))
(provide 'early-init)
;;; early-init.el ends here
