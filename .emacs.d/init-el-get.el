;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; init.el -- Emacs init setting elisp file
;;
;; $Id$

;;; Commentary:
;;
;; emacs package設定

;;; Code:
;;
;;
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(custom-set-variables
 '(el-get-verbose t))

;; setup
(el-get-bundle emacs-jp/init-loader)
(el-get-bundle purcell/exec-path-from-shell)

;; My Utilities
(el-get-bundle syohex/emacs-editutil :name editutil)
;;(el-get-bundle syohex/emacs-progutil :name progutil)
(el-get-bundle syohex/emacs-zoom-window2 :name zoom-window2)

;; Theme
(el-get-bundle syohex/emacs-syohex-theme :name syohex-theme
  (add-to-list 'custom-theme-load-path default-directory))

;; undo
(el-get-bundle undo-tree)
(el-get-bundle undohist)

;; save-kill
(el-get-bundle savekill)

;; highlighting
(el-get-bundle vline)
(el-get-bundle col-highlight)

;; Search
(el-get-bundle syohex/emacs-anzu2 :name anzu2)
(el-get-bundle emacs-jp/migemo)

;; moving cursor
(el-get-bundle goto-chg)

;; region
(el-get-bundle expand-region)
(el-get-bundle mark-multiple)
(el-get-bundle multiple-cursors)

;; Buffer
(el-get-bundle emacs-jp/elscreen)
(el-get-bundle popwin)
(el-get-bundle lukhas/buffer-move)
(el-get-bundle syohex/emacs-zoom-window :name zoom-window)

;; Pair
(el-get-bundle Fuco1/smartparens)

;; Directory
(el-get-bundle direx)
(el-get-bundle syohex/emacs-dired-k2 :name dired-k2)

;; auto-complete
(el-get-bundle auto-complete/popup-el :name popup)

;; company
(el-get-bundle company-mode/company-mode :name company-mode)

;; helm
(el-get-bundle helm)

;; key utility
(el-get-bundle key-chord)

;; Repeat utility
(el-get-bundle myuhe/smartrep.el :name smartrep)

;; snippet
(el-get-bundle yasnippet)

;; C/C++
(el-get-bundle clang-format
  :type http
  :url "https://github.com/llvm-mirror/clang/blob/master/tools/clang-format/clang-format.el")

;; Go
(el-get-bundle go-mode)
(el-get-bundle syohex/emacs-go-eldoc :name go-eldoc)
(el-get-bundle golint
  :type http
  :url "https://raw.githubusercontent.com/golang/lint/master/misc/emacs/golint.el")
(el-get-bundle go-guru
  :type http
  :url "https://raw.githubusercontent.com/dominikh/go-mode.el/master/go-guru.el")
;;(el-get-bundle nsf/gocode :load-path ("emacs") :name go-autocomplete)
(el-get-bundle nsf/gocode
  :load-path ("emacs-company") :name company-go
  :depends (company-mode))
(el-get-bundle syohex/emacs-go-impl :name go-impl)
(el-get-bundle syohex/emacs-go-add-tags :name go-add-tags)

;; Emacs Lisp
(el-get-bundle purcell/elisp-slime-nav :name elisp-slime-nav)

;; Rust
(el-get-bundle rust-mode)

;; Build tool
(el-get-bundle cmake-mode)

;; Markup language
(el-get-bundle markdown-mode)
(el-get-bundle yoshiki/yaml-mode)

;; session
(el-get-bundle recentf-ext)

;; skk
(el-get-bundle ddskk)

;; yatex
(el-get-bundle yatex)

;; shell
(el-get-bundle syohex/emacs-quickrun2 :name quickrun2)

;; VCS
(el-get-bundle magit)
(el-get-bundle with-editor)
(el-get-bundle syohex/emacs-git-gutter2 :name git-gutter2)
(el-get-bundle syohex/emacs-git-messenger2 :name git-messenger2)

;; key
(el-get-bundle which-key)

;; Helm plugins
(el-get-bundle emacs-helm/helm-descbinds)
(el-get-bundle syohex/emacs-helm-gtags2 :name helm-gtags2)
(el-get-bundle syohex/emacs-helm-ag2 :name helm-ag2)
(el-get-bundle syohex/emacs-helm-godoc :name helm-godoc)
(el-get-bundle ShingoFukuyama/helm-swoop :name helm-swoop)

;; init-el-get.el ends here
