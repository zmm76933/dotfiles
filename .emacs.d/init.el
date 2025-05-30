;; -*- lexical-binding: nil -*-
;; (eval-when-compile
;;   (require 'profiler)
;;   (profiler-start 'cpu)
;;   )

(eval-and-compile
  (when load-file-name
    (setq user-emacs-directory
          (expand-file-name (file-name-directory load-file-name))))
  (defconst my:d:share
    (expand-file-name "share/" user-emacs-directory))
  (defconst my:d:tmp
    (expand-file-name ".cache/emacs/" (getenv "HOME")))
  (defconst my:d:backup
    (expand-file-name "backup-file" my:d:tmp))
  (defconst my:d:auto-save
    (expand-file-name "auto-save-list" my:d:tmp))
  (defconst my:d:org
    (expand-file-name "~/Dropbox/org")))

(eval-when-compile
  (require 'cl-lib nil t))

(eval-and-compile
  ;; (setq byte-compile-warnings t)
  (setq byte-compile-warnings '(cl-functions free-vars docstrings unresolved))
  )

(eval-and-compile
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (setq native-comp-speed  2
            native-comp-async-report-warnings-errors nil ;; 'silent
            native-compile-target-directory (expand-file-name "eln-cache" user-emacs-directory)
            native-comp-jit-compilation-deny-list '(".*-\\(loaddefs\\|autoloads\\)\\.\\(el\\.gz\\|el\\)")
            ))
  (setq debug-on-error t))

(setq gc-cons-threshold most-positive-fixnum)
;; Run GC every 60 seconds if emacs is idle.
(run-with-idle-timer 60.0 t #'garbage-collect)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; recover default value
            (setq gc-cons-threshold 800000)))

(defconst my:saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my:saved-file-name-handler-alist)))

(defvar network-drives '(("/Volumes" . "//nas01")
                         ("/Volumes" . "/nas01")
                         ("/Volumes" . "//192.168.32.11")
                         ("/Volumes" . "/192.168.32.11")))

;; elpa/gnutls workaround
(eval-and-compile
;;  (when (version<=  emacs-version "26.2")  ;; => for syntax hightlight
;;    (customize-set-variable 'gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
  (setq package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                           ("melpa"  . "https://melpa.org/packages/")
                           ("org"    . "https://orgmode.org/elpa/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           )
        package-gnupghome-dir (expand-file-name ".gnupg" (getenv "HOME"))
        package-quickstart nil
        ;; package-quickstart-file (expand-file-name "package-quickstart.el" my:d:tmp)
        )
  ;; (add-hook 'kill-emacs-hook 'package-quickstart-refresh)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (setq package-native-compile t))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf t)
    )
  )

(leaf leaf-convert
  :ensure t)

(leaf leaf-tree
  :ensure t
  :custom ((imenu-list-size . 30)
           (imenu-list-position . 'left)))

(leaf leaf-keywords
  :doc "Use leaf as a package manager"
  :url "https://github.com/conao3/leaf.el"
  :ensure t
  :init
  (leaf blackout :ensure t)
  (leaf hydra :ensure t)
  (leaf el-get
    :ensure t
    :init (setq el-get-git-shallow-clone t))
  :config
  (leaf-keywords-init)
  )

;;;###autoload
(defun my:put-current-path-to-clipboard ()
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (defun convert-path-as-windows(path)
      (let ((a path)
            (number 0))
        (while (< number (length network-drives))
          (when (string-match (car (nth number network-drives)) a)
            (setq a (s-replace (car (nth number network-drives)) (cdr (nth number network-drives)) a))
            (setq number (length network-drives))
            )
          (setq number (1+ number)))
        (s-replace "/" "\\" a)))
    (cond (file-path
           (kill-new (convert-path-as-windows file-path))
           (message "(%s) --> clipboard" (convert-path-as-windows file-path)))
          (dir-path
           (kill-new (convert-path-as-windows dir-path))
           (message "(%s) --> clipboard" (convert-path-as-windows dir-path)))
          (t
           (error-message-string "Fail to get path name.")))))

;;;###autoload
(defun my:shorten-file-path (fpath max-length)
  "Show up to `max-length' characters of a directory name `fpath' like zsh"
  (let* ((path (reverse (split-string (abbreviate-file-name fpath) "/")))
         (output "")
         (top (mapconcat 'identity (reverse (last path 3)) "/"))
         (vmax (- max-length 4 (length top)))
         (path (butlast path 3))
         )
    (while (and path
                (and (< (length output) vmax) ;; > (for syntax)
                     (< (length (concat "/" (car path) output)) vmax))) ;; > (for syntax)
      (setq output (concat "/" (car path) output))
      (setq path (cdr path)))
    ;; 省略
    (when path
      (setq output (concat "/..." output)))
    (format "%s%s" top output)))

;;;###autoload
(defun my:delete-file-if-no-contents ()
  (when (and (buffer-file-name (current-buffer))
             (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))
(add-hook 'after-save-hook 'my:delete-file-if-no-contents)

;;;###autoload
(defun my:make-scratch (&optional arg)
  " *scratch* を作成して buffer-list に放り込む."
  (interactive)
  (progn
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg
        (progn
          (setq arg 0)
          (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

;;;###autoload
(defun my:buffer-name-list ()
  "buffer 一覧の取得"
  (mapcar (function buffer-name) (buffer-list)))
;;
(add-hook 'kill-buffer-query-functions
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my:make-scratch 0) nil)
              t)))
(add-hook 'after-save-hook
          (lambda ()
            (unless (member "*scratch*" (my:buffer-name-list))
              (my:make-scratch 1))))

(defvar my:delete-trailing-whitespace-exclude-suffix
  (list "\\.rd$" "\\.md$" "\\.rbt$" "\\.rab$"))

;;;###autoload
(defun my:delete-trailing-whitespace ()
  (interactive)
  (eval-when-compile (require 'cl-lib nil t))
  (cond
   ((equal nil
           (cl-loop for pattern in my:delete-trailing-whitespace-exclude-suffix
                    thereis (string-match pattern buffer-file-name)))
    (delete-trailing-whitespace))))
(add-hook 'before-save-hook 'my:delete-trailing-whitespace)

;;;###autoload
; cargo cult adaptation of event-apply-control-modifier
(defun my:event-apply-control-meta-modifiers (ignore-prompt)
  (ignore ignore-prompt)
  (vector
   (event-apply-modifier (event-apply-modifier (read-event)
                                               'control 26 "C-")
                         'meta 27 "M-")))
(define-key function-key-map (kbd "C-x @") 'my:event-apply-control-meta-modifiers)

(leaf exec-path-from-shell
  :ensure t
  :custom
  `((exec-path-from-shell-variables
     . '("GPG_KEY_ID"
         "GPG_TTY"
         "PASSWORD_STORE_DIR"
         "PATH"
         "MANPATH"
         "LANG"
         "LC_COLLATE"
         "LC_CTYPE"
         "LC_MONETARY"
         "LC_NUMERIC"
         "SHELL"
         "SKKSERVER"
         "XAPIAN_CJK_NGRAM"
         "VIRTUAL_ENV"
         "WSL_DISTRO_NAME"))
    (exec-path-from-shell-arguments . nil))
  :config
  (exec-path-from-shell-initialize)
  (defconst my:d:password-store
    (if (getenv "PASSWORD_STORE_DIR")
        (expand-file-name (concat "Emacs/" (system-name))
                          (getenv "PASSWORD_STORE_DIR")) nil))
  )

(leaf cp5022x
  :ensure t
  :require t
  :config
  (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                        'katakana-jisx0201 'iso-8859-1 'unicode)
  (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)
  )

(leaf locale-eaw-emoji
  :el-get uwabami/locale-eaw-emoji
  :hook (emacs-startup-hook . eaw-and-emoji-fullwidth)
  )

(leaf editutil
  :el-get zmm76933/emacs-editutil
  :hook (emacs-startup-hook . editutil-default-setup)
  )

(leaf cus-edit
  :preface
  (setq custom-file (expand-file-name "custom.el" my:d:tmp))
  :custom
  `((custom-file . ,(expand-file-name "custom.el" my:d:tmp)))
  :hook
  `((kill-emacs-hook . (lambda ()
                         (if (file-exists-p custom-file)
                             (delete-file custom-file)))))
  )

(leaf cus-start
  :custom
  `(
    ;; 表示
    (ring-bell-function     . 'ignore)   ; ベル無効化
    ;; 編集
    (tab-width              . 4)    ;; tab 幅 4
    (indent-tabs-mode       . nil)  ;; tab ではインデントしない
    (fill-column            . 72)   ;; RFC2822 風味
    (truncate-lines         . t)    ;; 折り返し
    (truncate-partial-width-windows . t)
    (paragraph-start        . '"^\\([ 　・○<\t\n\f]\\|(?[0-9a-zA-Z]+)\\)")
    (auto-fill-mode         . nil)
    (next-line-add-newlines . nil)  ;; バッファ終端で newline を入れない
    (read-file-name-completion-ignore-case . t)  ; 大文字小文字区別無し
    (save-abbrevs           . 'silent)
    ;; backup
    (auto-save-list-file-prefix . ,(expand-file-name "saves-" my:d:auto-save))
    (auto-save-default       . t)
    (auto-save-timeout       . 60)
    (auto-save-interval      . 360)
    (make-backup-files       . t)
    (backup-by-copying       . t)  ;; symlink は使わない
    (backup-directory-alist  . '((".*" . ,my:d:backup)))
    (auto-save-file-name-transforms . '((".*" ,my:d:auto-save t)))
    (version-control         . nil)
    (kept-new-versions       . 2)
    (kept-old-versions       . 2)
    (delete-old-versions     . t)
    (delete-auto-save-files  . t)
    ;; undo/redo - 数字に根拠無し
    (undo-limit              . 200000)
    (undo-strong-limit       . 260000)
    (history-length          . t)  ;; 無制限(の筈)
    ;; (save-silently           . t)
    (use-short-answers       . t)
    (split-height-threshold  . nil)
    (epg-pinentry-mode       . 'loopback)
    (scroll-step             . 1)
    (scroll-preserve-screen-position . 'always)
    ;;
    (safe-local-variable-values
     . '((org-link-file-path-type . absolute)))
    (mac-pass-command-to-system . nil)
    (mac-pass-control-to-system . nil)
    )
  :config
  (when (boundp 'load-prefer-newer)
    (setq load-prefer-newer t))
  ;; yes or no を y or n に
  (when (< emacs-major-version 28)
    (fset 'yes-or-no-p 'y-or-n-p))
  )

(leaf startup
  :custom
  ((inhibit-startup-screen            . t)
   (inhibit-startup-message           . t)
   (inhibit-startup-echo-area-message . t)
   (initial-scratch-message           . nil)
   )
  )

(leaf hl-line
  :hook
  (emacs-startup-hook . global-hl-line-mode)
  )

(leaf simple
  :hook
  (emacs-startup-hook . transient-mark-mode)
  )

(leaf delsel
  :hook
  (emacs-startup-hook . delete-selection-mode)
  )

(leaf paren
  :custom
  ((show-paren-style  . 'mixed))
  :hook
  (emacs-startup-hook . show-paren-mode)
  )

(leaf line-number-mode
  :custom
  ((linum-format     . "%5d ")
   (line-number-mode . nil))
  )

(leaf minibuffer
  :custom
  (inhibit-message-regexps . '("^Saving file" "^Wrote" "^Cleaning up"))
  (set-message-functions . '(inhibit-message))
  :hook
  (minibuffer-inactive-mode-hook . (lambda ()
                                     (let ((clipbord (gui-selection-value)))
                                       (unless (null clipbord)
                                         (setq kill-ring (cons clipbord kill-ring))
                                         (setq kill-ring-yank-pointer kill-ring)))
                                     (unless (null kill-ring)
                                       (unless (null kill-ring-yank-pointer)
                                         (let ((a (s-replace "\\" "/" (car kill-ring)))
                                               (number 0))
                                           (while (< number (length network-drives))
                                             (case (string-match (cdr (nth number network-drives)) a)
                                               (0
                                                (setq kill-ring (cons (s-replace (cdr (nth number network-drives)) (car (nth number network-drives)) a) kill-ring ))
                                                (setq number (length network-drives)))
                                               (t
                                                (setq kill-ring (cons a kill-ring))
                                                (setq number (1+ number))
                                                ))
                                             (setq kill-ring-yank-pointer kill-ring)
                                             ))))))
  )

(leaf autorevert
  :custom
  ((auto-revert-interval . 0.1))
  :hook
  (find-file-hook . global-auto-revert-mode)
  )

(leaf savehist
  :custom
  `((savehist-file
     . ,(expand-file-name "history" my:d:tmp)))
  :hook
  ((emacs-startup-hook . savehist-mode))
  )

(leaf *change-default-file-location
  :custom
  `(;; url
    (url-configuration-directory
     . ,(expand-file-name "url" my:d:tmp))
    ;; nsm
    (nsm-settings-file
     . ,(expand-file-name "nsm.data" my:d:tmp))
    ;; bookmark
    (bookmark-default-file
     . ,(expand-file-name "bookmarks" my:d:tmp))
    ;; eshell
    (eshell-directory-name
     . ,(expand-file-name "eshell" my:d:tmp))
    )
  )

(leaf eldoc
  :hook (emacs-lisp-mode-hook . turn-on-eldoc-mode)
  :blackout t
  :custom
  `((eldoc-echo-area-prefer-doc-buffer . nil)
    (eldoc-print-after-edit            . t)
    (eldoc-echo-area-use-multiline-p   . nil))
  :preface
  (defun my:shutup-eldoc-message (f &optional string)
    (unless (active-minibuffer-window)
      (funcall f string)))
  :advice
  (:around eldoc-message
           my:shutup-eldoc-message)
  )

(leaf midnight
  :custom
  ((clean-buffer-list-delay-general . 1))
  :hook
  (emacs-startup-hook . midnight-mode))

(leaf uniquify
  :custom
  ((uniquify-buffer-name-style . 'post-forward-angle-brackets)
   (uniquify-min-dir-content   . 1))
  )

(leaf whitespace
  ;; :blackout ((global-whitespace-mode . "")
  ;;            (whitespace-mode        . ""))
  :hook (emacs-startup-hook . global-whitespace-mode)
  :custom
  ((whitespace-line-column      . 72)
   (whitespace-style
    . '(face        ; faceを使う. *-mark に必要
        trailing    ; 行末の空白を対象.
        tabs        ; tab
        spaces      ; space
        empty       ; 前後の空行
        space-mark  ; 可視化の際の置き換えを有効化
        tab-mark    ; 可視化の際の置き換えを有効化
        ))
   (whitespace-display-mappings . '((space-mark ?\u3000 [?\□])
                                    (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
   (whitespace-space-regexp     . "\\(\u3000+\\)")
   (whitespace-trailing-regexp  . "\\([ \u00A0]+\\)$")
   (whitespace-global-modes     . '(not eww-mode
                                        term-mode
                                        eshell-mode
                                        org-agenda-mode
                                        calendar-mode))
   )
  )

(leaf save-place
  :custom
  `((save-place . t)
    (save-place-file . ,(expand-file-name "emacs-places"  my:d:tmp))
    ;; add tramp-file-name-regexp
    (save-place-ignore-files-regexp
     . "\\(\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$\\)\\|\\(\\`/[^/:]+:[^/:]*:\\)")
    )
  :hook (emacs-startup-hook . save-place-mode)
  )

(leaf time-stamp
  :hook (before-save-hook . time-stamp)
  :custom
  ((time-stamp-active     . t)
   (time-stamp-line-limit . 10)
   (time-stamp-start      . "$Lastupdate: 2")
   (time-stamp-end        . "\\$")
   (time-stamp-format     . "%Y-%02m-%02d %02H:%02M:%02S")
   )
  )

(leaf tramp
  :preface
  (defvar tramp-persistency-file-name (expand-file-name "tramp" my:d:tmp))
  :custom
  `((tramp-persistency-file-name . ,(expand-file-name "tramp" my:d:tmp))
    (tramp-completion-reread-directory-timeout . nil)
    (remote-file-name-inhibit-cache . nil)
    (vc-ignore-dir-regexp
     . ,(format "\\(%s\\)\\|\\(%s\\)"
                locate-dominating-stop-dir-regexp
                tramp-file-name-regexp))
    )
  :hook
  (kill-emacs-hook . (lambda ()
                       (if (file-exists-p tramp-persistency-file-name)
                           (delete-file tramp-persistency-file-name))))
  )

(leaf browse-url
  ;; :require t
  :bind ("C-c C-j" . browse-url-at-point)
  :defer-config
  (cond ((executable-find "xdg-open")
         (setq browse-url-browser-function 'browse-url-xdg-open
               browse-url-secondary-browser-function 'browse-url-xdg-open))
        ((eq system-type 'darwin)
         (setq browse-url-browser-function 'browse-url-default-macosx-browser
               browse-url-secondary-browser-function 'browse-url-default-macosx-browser))
        (t
         ;; (setq browse-url-browser-function 'w3m-browse-url)
         (setq browse-url-browser-function 'eww-browse-url)
         ))
  )

(leaf server
  :commands (server-running-p)
  :init
  (defun my:new-client-frame ()
    "Create new GUI emacsclient"
    (interactive)
    (make-frame-on-display (getenv "DISPLAY")))
  :hook
  (emacs-startup-hook . (lambda ()
                          (unless (server-running-p)
                            (server-start))))
  )

(leaf ps-mule
  :if (executable-find "lpr")
  :custom
  ((ps-multibyte-buffer       . 'nil ) ;;non-latin-printer
   (ps-printer-name           . "PDF")
   (ps-paper-size             . 'a4)
   ;; (ps-n-up-printing          .  2)
   (ps-print-header           .  t)
   (ps-print-footer           .  nil)
   (ps-font-family            . 'Courier)
   (ps-font-size              . '(9 . 10))
   (ps-header-font-family     . 'Helvetica)
   (ps-header-font-size       . '(10 . 12))
   (ps-header-title-font-size . '(12 . 14))
   (ps-line-number            . nil)
   ;; (ps-line-number-font   . "Times-Italic")
   ;; (ps-line-number-font-size . 6)
   ;; (ps-line-number-start   . 1)
   ;; (ps-line-number-step    . 1)
   )
  :hook
  (defalias 'ps-mule-header-string-charset 'ignore)
  :config
  ;; (setq ps-mule-font-info-database-default
  ;;       '((iso-8859-1
  ;;          (normal nil nil))
  ;;         (katakana-jisx0201
  ;;          (normal builtin "Ryumin-Light-Katakana")
  ;;          (bold builtin "GothicBBB-Medium-Katakana"))
  ;;         (latin-jisx0201
  ;;          (normal builtin "Ryumin-Light-Hankaku")
  ;;          (bold builtin "GothicBBB-Medium-Hankaku"))
  ;;         (japanese-jisx0208
  ;;          (normal builtin "Ryumin-Light-Ext-H")
  ;;          (bold builtin "GothicBBB-Medium-Ext-H"))
  ;;         (japanese-jisx0213-2
  ;;          (normal builtin "Ryumin-Light-Ext-H")
  ;;          (bold builtin "GothicBBB-Medium-Ext-H"))
  ;;         (japanese-jisx0213.2004-1
  ;;          (normal builtin "Ryumin-Light-2004-H")
  ;;          (bold builtin "GothicBBB-Medium-H"))
  ;;         (unicode-bmp
  ;;          (normal builtin "Ryumin-Light-Ext-H")
  ;;          (bold builtin "GothicBBB-Medium-Ext-H"))
  ;;         )
  ;;       )
  )

(leaf *authentication
  :if (and (getenv "GPG_KEY_ID")
           my:d:password-store)
  :init
  (setq leaf-default-plstore
     (plstore-open
         (expand-file-name "plstore.plist" my:d:password-store)))
  (add-to-list 'vc-directory-exclusion-list
               (expand-file-name my:d:password-store))
  (leaf auth-source
    :init
    (setq auth-source-gpg-encrypt-to '(getenv "GPG_KEY_ID")))
  (leaf password-store :ensure t)
  (leaf auth-source-pass :ensure t)
  (leaf plstore
    :custom
    `((plstore-secret-keys . 'silent)
      (plstore-encrypt-to  . ,(getenv "GPG_KEY_ID")))
    )
  )

(leaf skk
  :commands skk-make-indicator-alist
  :bind (("C-\\" . skk-mode))
  :preface
  (defvar skk-user-directory (concat my:d:tmp "skk"))
  (unless (file-directory-p skk-user-directory)
    (make-directory skk-user-directory))
  (unless (locate-library "skk")
    (package-install 'ddskk t))
  :init
  (setq skk-preload t)
  (setq skk-init-file (concat user-emacs-directory "init-ddskk")
        default-input-method "japanese-skk")
  :hook
  (find-file-hooks . (lambda () (skk-mode) (skk-latin-mode-on)))
  (minibuffer-setup-hook . (lambda () (skk-mode) (skk-latin-mode-on)))
  (mu4e-compose-mode-hook . (lambda () (skk-mode) (skk-latin-mode-on)))
  (evil-normal-state-entry-hook . (lambda () (skk-mode) (skk-latin-mode-on)))
  )

(leaf undo-tree
  :ensure t
  :init
  (let ((undo-tree-history-directory (expand-file-name "undo-tree" my:d:tmp)))
    (setq undo-tree-history-directory-alist `(("." . ,undo-tree-history-directory))))
  :hook
  (emacs-startup-hook . global-undo-tree-mode)
  :bind
  ("C-M-/" . undo-tree-redo)
  )

(leaf expand-region
  :ensure t
  :bind
  (("C-."     . er/expand-region)
   ("C-,"     . er/contract-region))
  )

(leaf all-the-icons
  :require t
  :ensure t
  :custom
  (all-the-icons-scale-factor . 1.0))

(leaf dired
  :if (executable-find "gls")
  :preface
  (defun my:dired-mode-open-with()
    (interactive)
    (let ((fn (dired-get-file-for-visit)))
      (start-process "default-app" nil "open" fn)))
  (defun my:dired-mode-open-finder()
    (interactive)
    (shell-command "open ."))
  :custom
  ((insert-directory-program . "gls")
   (ls-lisp-dirs-first . t)
   (dired-listing-switches . "-AlhL --group-directories-first")
   (dired-dwim-target . t)
   (dired-auto-revert-buffer . t)
   (dired-kill-when-opening-new-dired-buffer . t)
   (dired-isearch-filenames . 'dwim)
   (dired-recursive-copies . 'always)
   (dired-recursive-deletes . 'always))
  :bind
  (:dired-mode-map
   ("z" . my:dired-mode-open-with)
   ("f" . my:dired-mode-open-finder))
  :config
  (leaf all-the-icons-dired
    :ensure t
    :hook
    (dired-mode-hook . all-the-icons-dired-mode))
  )

(leaf dired-subtree
  :ensure t
  :after dired
  :require t
  :bind
  (:dired-mode-map ("<tab>" . dired-subtree-toggle))
  )

(leaf quick-preview
  :ensure t
  :after dired
  :require t
  :bind
  (:dired-mode-map ("SPC" . quick-preview-at-point))
)

(leaf recentf
  :defun
  (recentf-save-list recentf-cleanup)
  :preface
  (defun my:recentf-track-visited-file (_prev _curr)
    (and buffer-file-name
         (recentf-add-file buffer-file-name)))
  :init
  (leaf recentf-ext :ensure t)
  :hook
  (emacs-startup-hook . recentf-mode)
  :custom
  `((recentf-save-file       . ,(expand-file-name "recentf" my:d:tmp))
    (recentf-max-saved-items . 500)
    (recentf-auto-cleanup    . 'mode)
    (recentf-exclude         . '(".recentf"
                                 "^/tmp\\.*"
                                 "^/private\\.*"
                                 "^/var/folders\\.*"
                                 "^/ssh:"
                                 "/TAGS$"
                                 "^#\\.*"
                                 "^/[^/:]+:"
                                 "bookmarks"
                                 "\\.*COMMIT_EDITMSG$"
                                 "^/home/zmm76933/.cache/\\.*"
                                 "^/Users/zmm76933/.cache/\\.*"
                                 ))
    )
  )

(leaf calendar
  :defvar calendar-holidays japanese-holidays
  :custom
  (;; 月と曜日の表示調整
   ;; (calendar-month-name-array . ["01" "02" "03" "04" "05" "06"
   ;;                               "07" "08" "09" "10" "11" "12" ])
   ;; (calendar-day-name-array   . ["日" "月" "火" "水" "木" "金" "土"])
   ;; (calendar-day-header-array . ["日" "月" "火" "水" "木" "金" "土"])
   ;; 日曜開始
   (calendar-week-start-day   . 0)
   ;; 祝日をカレンダーに表示
   (calendar-mark-holidays-flag . t)
   )
  :config
  (with-eval-after-load 'japanese-holidays
    (setq calendar-holidays (append japanese-holidays holiday-local-holidays)))
  )

(leaf japanese-holidays
  :ensure t
  :after calendar
  :require t
  :custom
  `((japanese-holiday-weekend         . '(0 6))
    (japanese-holiday-weekend-marker
     . '(holiday  ;; 日
         nil      ;; 月
         nil      ;; 火
         nil      ;; 水
         nil      ;; 木
         nil      ;; 金
         japanese-holiday-saturday))
    )
  :config
  ;; autoload
  (defun my:japanese-holiday-show (&rest _args)
    (let* ((date (calendar-cursor-to-date t))
           ;; (calendar-date-display-form '((format "%s年 %s月 %s日（%s）" year month day dayname)))
           (date-string (calendar-date-string date))
           (holiday-list (calendar-check-holidays date)))
      (when holiday-list
        (message "%s: %s" date-string (mapconcat #'identity holiday-list "; ")))))
  :hook
  ((calendar-move-hook            . my:japanese-holiday-show)
   (calendar-today-visible-hook   . japanese-holiday-mark-weekend)
   (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
   (calendar-today-visible-hook   . calendar-mark-today))
  )

(leaf key-settings
  :config
  (keyboard-translate ?\C-h ?\C-?)
  (leaf-keys (("C-z"       . scroll-down)
              ("C-M-z"     . scroll-other-window-down)
              ("C-/"       . undo)
              ("C-x K"     . kill-buffer)
              ("C-c M-r"   . replace-regexp)
              ("C-c M-l"   . toggle-truncate-lines)
              ("C-x RET r" . revert-buffer)
              ([wheel-down]        . '(lambda () "" (interactive) (scroll-up 1)))
              ([wheel-up]          . '(lambda () "" (interactive) (scroll-down 1)))
              ([double-wheel-down] . '(lambda () "" (interactive) (scroll-up 1)))
              ([double-wheel-up]   . '(lambda () "" (interactive) (scroll-down 1)))
              ([triple-wheel-down] . '(lambda () "" (interactive) (scroll-up 2)))
              ([triple-wheel-up]   . '(lambda () "" (interactive) (scroll-down 2)))))
  )

(leaf ediff
  :custom
  (ediff-split-window-function . 'split-window-horizontally)
  (ediff-window-setup-function . 'ediff-setup-windows-plain)
  (ediff-diff-options          . "-twB"))

(leaf migemo
  :if (executable-find "cmigemo")
  :ensure t
  :custom
  '((migemo-user-dictionary  . nil)
    (migemo-regex-dictionary . nil)
    (migemo-options          . '("-q" "--emacs"))
    (migemo-command          . "cmigemo")
    (migemo-coding-system    . 'utf-8-unix))
  :init
  (cond
   ((and (eq system-type 'darwin)
         (file-directory-p "/opt/homebrew/share/migemo/utf-8/"))
    (setq migemo-dictionary "/opt/homebrew/share/migemo/utf-8/migemo-dict"))
   (t
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))
  :hook
  (emacs-startup-hook . migemo-init)
  )

(leaf eww
  :preface
  (unless (file-directory-p (expand-file-name "eww" my:d:tmp))
    (make-directory (expand-file-name "eww" my:d:tmp)))
  :init
  (leaf ace-link :ensure t)
  (leaf shr
    :custom
    ((shr-use-colors    . nil)
     (shr-use-fonts     . nil)
     (shr-image-animate . nil)
     (shr-width         . 72))
    )
  :bind
  ("<f2>" . eww)
  :custom
  `((eww-bookmarks-directory . ,(expand-file-name "eww" my:d:tmp))
    (eww-search-prefix
     . "https://www.google.com/search?&gws_rd=cr&complete=0&pws=0&tbs=li:1&q=")
    )
  ;; :advice (:around eww-colorize-region
  ;;                  my:shr-colorize-region--disable)
  :config
  (ace-link-setup-default)
  )

(leaf emacs-w3m
  :if (and (executable-find "w3m")
           (file-directory-p "/opt/homebrew/share/emacs/site-lisp/w3m/"))
  :preface
  (defun my:w3m-open-current-page-in-firefox ()
    "Open the current URL in Mozilla Firefox."
    (interactive)
    (browse-url-firefox w3m-current-url))

  (defun my:w3m-open-link-or-image-in-firefox ()
    "Open the current link or image in Firefox."
    (interactive)
    (browse-url-firefox (or (w3m-anchor)
                            (w3m-image))))
  :bind
  ((:w3m-mode-map
    ("f" . my:w3m-open-current-page-in-firefox)
    ("F" . my:w3m-open-link-or-image-in-firefox))
   )
  :custom
  `((w3m-fill-column . 72))
  )

(leaf ibuffer
  :defun (ibuffer-current-buffer)
  :defvar (ibuffer-formats)
  :preface
  (defun my:ibuffer-find-file ()
    "Like `find-file', but default to the directory of the buffer at point."
    (interactive)
    (let ((default-directory
            (let ((buf (ibuffer-current-buffer)))
              (if (buffer-live-p buf)
                  (with-current-buffer buf
                    default-directory)
                default-directory))))
      (find-file default-directory)))
  ;;
  :bind (("C-x C-b" . ibuffer)
         (:ibuffer-mode-map
          ("C-x C-f" . my:ibuffer-find-file))
         )
  )

(leaf xclip
  :if (and (executable-find "xclip")
           (eq system-type 'gnu/linux))
  :ensure t
  :hook (emacs-startup-hook
         . (lambda () (xclip-mode +1)))
  )

(leaf *macOSclipborad
  :disabled t
  :if (eq system-type 'darwin)
  :preface
  (defun my:copy-from-osx ()
    "Get string via pbpaste"
    (shell-command-to-string "pbpaste"))
  (defun my:paste-to-osx (text &optional push)
    "put `TEXT' via pbcopy with `PUSH' mode"
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  :config
  (setq interprogram-cut-function   'my:paste-to-osx
        interprogram-paste-function 'my:copy-from-osx)
  )

(leaf *completion
  :init
  ;; 補完で無視する拡張子の追加．そのうち増える．
  (cl-loop for ext in
           '(;; TeX
             ".dvi"
             ".fdb_latexmk"
             ".fls"
             ".ilg"
             ".jqz"
             ".nav"
             ".out"
             ".snm"
             ".synctex\\.gz"
             ".vrb"
             ;; fortran >= 90
             ".mod"
             ;; zsh
             ".zwc"
             ;; libtool
             ".in"
             ".libs/"
             ;; fxxkin Apple
             ".DS_Store"
             "._DS_Store"
             ;; "org-id-locations"
             )
           do (add-to-list 'completion-ignored-extensions ext))
  )

(leaf vertico
  :ensure t
  :custom-face
  `((vertico-current
     . '((t (:inherit hl-line :background unspecified)))))
  :custom
  `((vertico-count . 9)
    (vertico-cycle . t)
    )
  :hook
  (emacs-startup-hook . vertico-mode)
  :config
  (leaf vertico-directory
    :ensure nil
    :require t
    :after vertico
    :bind
    (:vertico-map
     ("C-l" . vertico-directory-up)
     ("<backspace>" . vertico-directory-delete-char))
    )
  )

(leaf embark
  :ensure t
  :bind
  (("M-g e" . embark-act)
   ("C-'"   . embark-dwim)
   ("M-g b" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(leaf embark-consult
  :ensure t
  :after embark consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(leaf consult
  :ensure t
  :bind* (("C-;"   . consult-buffer)
          ("M-g ," . consult-find)
          ("M-g ." . consult-ripgrep)
          ("C-c o" . consult-outline)
          )
  :bind (("M-s"     . consult-line)
         ("C-x C-r" . consult-recent-file))
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (setq my:consult--source-project-buffer
        (plist-put consult--source-project-buffer :hidden nil))
  (setq my:consult--source-project-file
        (plist-put consult--source-project-recent-file :hidden nil))
  (defun my:consult-project ()
    "my `consult' command for project only"
    (interactive)
    (when-let (buffer (consult--multi '(my:consult--source-project-buffer
                                        my:consult--source-project-file)
                                      :require-match
                                      t ;(confirm-nonexistent-file-or-buffer)
                                      :prompt "(in proj) Switch to: "
                                      :history nil
                                      :sort nil))
      (unless (cdr buffer)
        (consult--buffer-action (car buffer)))))
  )

(leaf consult-dir
  :ensure t
  :bind
  (("C-x C-d" . consult-dir)))

(leaf consult-ls-git
  :ensure t
  :bind
  ("C-x C-a" . consult-ls-git)
  )

(leaf orderless
  :require t
  :ensure t
  :init
  (defun orderless-migemo (component)
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))
  :config
  (orderless-define-completion-style orderless-default-style
    (orderless-matching-styles '(orderless-literal
                                 orderless-regexp)))
  (orderless-define-completion-style orderless-migemo-style
    (orderless-matching-styles '(orderless-literal
                                 orderless-regexp
                                 orderless-migemo)))
  (orderless-define-completion-style orderless-initialism-style
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal)))
  (setq completion-category-overrides
        '((command (styles orderless-initialism-style))
          (file (styles orderless-migemo-style))
          (buffer (styles orderless-migemo-style))
          (symbol (styles orderless-default-style))
          (consult-location (styles orderless-migemo-style))
          (consult-multi (styles orderless-migemo-style))
          (org-roam-node (styles orderless-migemo-style))
          (unicode-name (styles orderless-migemo-style))
          (variable (styles orderless-default-style))))
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-migemo))
  :custom
  (completion-styles . '(orderless))
  )

(leaf marginalia
  :ensure t
  :init
  ;; 補完でも icon 表示
  (leaf all-the-icons-completion
    :ensure t
    :hook
    (emacs-startup-hook . all-the-icons-completion-mode)
    )
  :bind (("M-A" . marginalia-cycle)
         (:minibuffer-local-map
          ("M-A" . marginalia-cycle)))
  :config
  (add-to-list 'marginalia-prompt-categories '("\\<Heading\\>" . file))
  (add-to-list 'marginalia-prompt-categories '("\\<Node\\>" . file))
  (add-to-list 'marginalia-prompt-categories
               '("\\<Fuzzy grep in.*\\>" . file))
  :custom
  `((marginalia-annotators
     . '(marginalia-annotators-light marginalia-annotators-heavy nil))
    (marginalia-align . 'right)
    (marginalia-align-offset .  -2))
  :hook
  ((emacs-startup-hook . marginalia-mode)
   (marginalia-mode-hook . all-the-icons-completion-marginalia-setup))
  )

(leaf corfu
  :disabled t
  :ensure t
  :init
  (leaf corfu-terminal
    :ensure t
    :custom (corfu-terminal-disable-on-gui . nil)
    )
  :bind
  (:corfu-map
   ("C-n" . corfu-next)
   ("C-p" . corfu-previous)
   ("C-y" . corfu-complete)
   ("C-e" . corfu-quit)
   ("<return>" . corfu-complete)
   ("RET" . corfu-complete))
  :custom
  `((completion-cycle-threshold . 4)
    (tab-always-indent          . 'complete)
    (corfu-cycle                . t)    ;; Enable cycling for `corfu-next/previous'
    (corfu-auto                 . t)    ;; Enable auto completion
    (corfu-preselect-first      . nil)  ;; Disable candidate preselection
    ;; (corfu-separator          . ?\s)  ;; Orderless field separator
    ;; (corfu-quit-at-boundary   . nil)  ;; Never quit at completion boundary
    ;; (corfu-quit-no-match      . nil)  ;; Never quit, even if there is no match
    ;; (corfu-preview-current    . nil)  ;; Disable current candidate preview
    ;; (corfu-on-exact-match     . nil)  ;; Configure handling of exact matches
    ;; (corfu-echo-documentation . nil)  ;; Disable documentation in the echo area
    ;; (corfu-scroll-margin      . 5)    ;; Use scroll margin
    )
  :hook (emacs-startup-hook
         . (lambda ()
             (corfu-terminal-mode +1)
             (global-corfu-mode)))
  )

;; (leaf cape :ensure t)
  ;; ;; 補完候補を出すときの文脈を特定

  ;; (defvar my-capf-context nil)

  ;; (defun my-capf--corfu--auto-complete (old-fun &rest args)
  ;;   ;; my-capf-context = 'in-corfu--auto-complete にして補完候補を出す。
  ;;   (let ((my-capf-context 'in-corfu--auto-complete))
  ;;     (apply old-fun args)))
  ;; (advice-add 'corfu--auto-complete :around #'my-capf--corfu--auto-complete)

  ;; ;; 追加の補完関数

  ;; (defun my-capf-additional ()
  ;;   (pcase my-capf-context
  ;;     ('in-corfu--auto-complete
  ;;      ;; 自動補完の場合は確度の高い候補しか出さない。
  ;;      nil)
  ;;     (_
  ;;      ;; 手動補完の場合は積極的にいろんな候補を出す。
  ;;      (my-capf-manual))))
  ;; (add-hook 'completion-at-point-functions #'my-capf-additional 100)

  ;; ;; 手動補完時の補完関数

  ;; (defvar my-capf-manual nil)
  ;; (defun my-capf-manual ()
  ;;   ;; capeパッケージの読み込みを遅延させる。
  ;;   (unless my-capf-manual
  ;;     (setq my-capf-manual
  ;;           ;; いろんな補完候補を合成する。
  ;;           (cape-super-capf
  ;;            #'cape-file #'cape-dabbrev #'cape-abbrev #'cape-line)))
  ;;     (funcall my-capf-manual))

(leaf mu4e
  :if (executable-find "mu")
  :bind
  ("<f3>" . mu4e)
  :init
  (require 'mu4e)
  ;; use mu4e for e-mail in emacs
  (setq mail-user-agent 'mu4e-user-agent)

  ;; the next are relative to the root maildir
  ;; (see `mu info`).
  ;; instead of strings, they can be functions too, see
  ;; their docstring or the chapter 'Dynamic folders'
  (setq mu4e-maildir (expand-file-name "mu4e" my:d:tmp)
        mu4e-sent-folder   "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder  "/Trash"
        mu4e-refile-folder "/Archive")

  ;; the maildirs you use frequently; access them with 'J' ('jump')
  (setq mu4e-maildir-shortcuts
        '(
          (:maildir "/Inbox"   :key ?i)
          (:maildir "/Sent"    :key ?s)
          (:maildir "/Drafts"  :key ?d)
          (:maildir "/Archive" :key ?a)
          (:maildir "/Trash"   :key ?t)))
  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-date-format "%F %T")
  (setq mu4e-headers-fields
        '( (:human-date    .  20)    ;; alternatively, use :human-date
           (:flags         .   6)
           (:tags          .  10)
           (:mailing-list  .  20)
           (:from          .  35)
           (:subject       .  nil))) ;; alternatively, use :thread-subject

  ;; Marks
  ;; See: https://www.djcbsoftware.nl/code/mu/mu4e/Adding-a-new-kind-of-mark.html
  (add-to-list 'mu4e-marks
               '(tag
                 :char       "g"
                 :prompt     "gtag"
                 :ask-target (lambda () (read-string "What tag do you want to add?"))
                 :action     (lambda (docid msg target)
                               (mu4e-action-retag-message msg (concat "+" target)))))
  (mu4e~headers-defun-mark-for tag)
  (add-to-list 'mu4e-headers-actions
               '("retag message" . mu4e-action-retag-message) t)
  (add-to-list 'mu4e-view-actions
               '("retag message" . mu4e-action-retag-message) t)

  ;; 添付ファイルの保存先を"~/Downloads"に変更
  (setq mu4e-attachment-dir "~/Downloads")

  ;; use 'fancy' non-ascii characters in various places in mu4e
  (setq mu4e-use-fancy-chars nil)
  ;; Header Viewの行数を10 -> 20に変更（バッファ内だとC-+, C-- で増減可能）
  (setq mu4e-headers-visible-lines 20)
  ;; 重複するメッセージは表示しない（バッファ内だと V で切替可能）
  (setq mu4e-headers-skip-duplicates t)
  ;; Header Viewのメーッセージ数
  (setq mu4e-headers-results-limit 1000)
  ;; attempt to show images when viewing messages
  (setq mu4e-view-show-images t)
  ;; Update process warnings
  (setq mu4e-index-update-error-warning nil)

  ;; ;; メールアカウント毎の設定
  (setq mu4e-contexts
    `( ,(make-mu4e-context
          :name "work"
          :enter-func (lambda () (mu4e-message "Entering work context"))
          :leave-func (lambda () (mu4e-message "Leaving work context"))
          ;; メールの連絡先でマッチしてcontextを切り替え
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg
                            :to "katayama@web-tips.co.jp")
                            ))
          :vars '( ( user-mail-address      . "katayama@web-tips.co.jp")
                   ( user-full-name         . "片山 雅仁")
                   ( mu4e-compose-signature . (concat
                                               "━━━━━━━━━━━━━━━━━━━━━━━━━\n"
                                               "株式会社ウェブチップス\n"
                                               "片山 雅仁 (かたやま まさと)\n"
                                               "Email: katayama@web-tips.co.jp\n"
                                               "…………………………………………………………………\n"
                                               "〒770-0865 徳島市南末広町4番53号 エコービル4階\n"
                                               "TEL: 088-678-6619\n"
                                               "URL: https://www.web-tips.co.jp/\n"
                                               "━━━━━━━━━━━━━━━━━━━━━━━━━\n"
                                                ))))))

  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.
  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  (setq mu4e-compose-context-policy nil)

  ;; If you get your mail without an explicit command,
  ;; use "true" for the command (this is the default)
  (setq mu4e-get-mail-command "offlineimap")
  ;; update every 5 minutes
  (setq mu4e-update-interval 300)
  ;; use msmtp
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)
  )

(leaf evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
        evil-want-minibuffer nil
        evil-mode-line-format 'after
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol nil
        evil-backspace-join-lines nil
        evil-undo-system 'undo-tree
        evil-want-fine-undo t
        evil-move-cursor-back t
        evil-show-paren-range 1
        evil-echo-state nil
        evil-respect-visual-line-mode t
        evil-disable-insert-state-bindings t
        evil-want-abbrev-expand-on-insert-exit nil)
  :hook
  (emacs-startup-hook . evil-mode)
  :config
  (evil-add-command-properties #'isearch-forward :jump t)
  (evil-add-command-properties #'consult-line :jump t)
  (evil-add-command-properties #'consult-org-heading :jump t)
  (evil-add-command-properties #'consult-org-agenda :jump t)
  (evil-set-command-properties #'org-open-at-point :jump t)
  (evil-set-command-properties #'org-backward-heading-same-level :jump t)
  (evil-set-command-properties #'org-forward-heading-same-level :jump t)
  (evil-set-command-properties #'org-previous-visible-heading :jump t)
  (evil-set-command-properties #'org-next-visible-heading :jump t)
  (leaf evil-collection
    :ensure t
    :config
    (evil-collection-init '(ediff
                            elfeed
                            eww
                            calendar
                            info
                            ibuffer
                            org
                            org-roam
                            magit
                            mu4e
                            dired
                            vterm)))
  (leaf evil-org
    :ensure t
    :hook
    (org-mode-hook . evil-org-mode)
    (org-agenda-mode-hook . evil-org-mode)
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))
  (leaf evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))
  (leaf evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))
  (leaf key-bindings
    :config
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] #'evil-normal-state)
    (evil-define-key 'normal dired-mode-map
      " " 'quick-preview-at-point
      "ga" 'my:dired-mode-open-with
      "go" 'my:dired-mode-open-finder)
    (evil-define-key 'normal calendar-mode-map
      "C" 'my:org-archive-find-date))
  )

(leaf general
  :after evil
  :ensure t
  :config
  (general-evil-setup t)
  (general-nmap
    :prefix "SPC"
    "o" 'evil-open-below
    "O" 'evil-open-above
    "gj" 'git-gutter:next-hunk
    "gk" 'git-gutter:previous-hunk
    "gs" 'git-gutter:stage-hunk
    "gr" 'git-gutter:revert-hunk
    "gp" 'git-gutter:popup-hunk)
  (general-def
   :states '(insert emacs normal visual motion)
   :keymaps 'override
   :prefix "C-t"
   "c"    'tab-new
   "C-c"  'tab-new
   "k"    'tab-close
   "C-k"  'tab-close
   "n"    'tab-next
   "C-n"  'tab-next
   "p"    'tab-previous
   "C-p"  'tab-previous
   "t"    'tab-last
   "C-t"  'tab-last
   "x"    'tab-move
   "C-x"  'tab-move)
  )

(leaf tab-bar-mode
  :after general
  :custom
  ((tab-bar-close-button-show      . nil)
   (tab-bar-close-last-tab-choice  . nil)
   (tab-bar-close-tab-select       . 'left)
   (tab-bar-history-mode           . nil)
   (tab-bar-new-tab-choice         . "*scratch*")
   (tab-bar-new-button-show        . nil)
   )
  :hook
  (emacs-startup-hook . tab-bar-mode)
  )

(leaf elfeed
  :ensure t
  :preface
  (defun my:elfeed-mark-as-read ()
    "Mark all items in the elfeed buffer as read."
    (interactive)
    (my:mark-line 0)
    (elfeed-search-untag-all-unread))

  (defun my:elfeed-mark-all-as-read ()
    "Mark all items in the elfeed buffer as read."
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (defun my:elfeed-mark-current-as-read ()
    (interactive)
    "Mark current entry as read."
    (let ((current (elfeed-search-selected :ignore-region)))
      (elfeed-untag current 'unread)
      (elfeed-search-update-entry current)
      (elfeed-db-save-safe)))

  (defun my:elfeed-open-in-eww (entry)
    "Display the currently selected item in eww."
    (interactive (list (elfeed-search-selected :ignore-region)))
    (require 'elfeed-show)
    (my:elfeed-mark-current-as-read)
    (when (elfeed-entry-p entry)
      (let ((link (elfeed-entry-link entry)))
        (eww link)
        (rename-buffer (format "*elfeed eww %s*" link)))))

  (defun my:elfeed-show-in-eww ()
    "Display the currently shown item in eww."
    (interactive)
    (require 'elfeed-show)
    (when (elfeed-entry-p elfeed-show-entry)
      (let ((link (elfeed-entry-link elfeed-show-entry)))
        (eww link)
        (rename-buffer (format "*elfeed eww %s*" link)))))
  :bind
  (("<f4>" . elfeed))
  ;; :init
  ;; (setq elfeed-feeds
  ;;     '("http://nullprogram.com/feed/"
  ;;       "https://planet.emacslife.com/atom.xml"))
  :custom
  `((elfeed-db-directory           . ,(expand-file-name "elfeed" my:d:tmp))
    (elfeed-enclosure-default-dir  . "~/Downloads/"))
  :config
  (leaf elfeed-goodies
    :ensure t
    :custom
    `((elfeed-goodies/entry-pane-position  . 'bottom))
    :config
    (elfeed-goodies/setup)
    )
  ;;
  (leaf elfeed-org
    :ensure t
    :init
    (setq rmh-elfeed-org-files (list (format "%s/index/elfeed.org" my:d:org)))
    ; elfeed dynamic tagging rules

    (with-eval-after-load 'elfeed
      ;; for shizuoka-shinbun
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-url (rx "www.at-s.com")
                                    :entry-link
                                    (rx (or "news/article/shizuoka"
                                            "news/article/social/shizuoka"
                                            "news/article/politics/shizuoka"
                                            "news/article/topics/shizuoka"
                                            "news/article/culture/shizuoka"
                                            "news/article/local/west"
                                            "news/article/local/central"
                                            "news/article/local/east"))
                                    :add 'regional))
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-url (rx "www.at-s.com")
                                    :entry-link
                                    (rx (or "sports/article/shizuoka"
                                            "sports/article/national"
                                            "sports/article/soccer/national"))
                                    :add 'sports))
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-url (rx "www.at-s.com")
                                    :entry-link
                                    (rx "news/article/science")
                                    :add 'science))
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-url (rx "www.at-s.com")
                                    :entry-link
                                    (rx (or "news/article/topics/national"
                                            "news/article/culture/national"
                                            "news/article/politics/national"
                                            "news/article/social/national"
                                            "news/article/economy/national"
                                            "news/article/health/national"
                                            "news/article/international"
                                            "sports/article/sumo"
                                            "sports/article/golf"
                                            "sports/article/baseball"))
                                    :remove 'unread))
      ;; workaround for bug
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-url (rx "www.city.shimada.shizuoka.jp")
                                    :remove 'checked))
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-url (rx "www.city.shimada.shizuoka.jp")
                                    :add 'unread))
      ;; for favorite entries
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-url (rx "pc.watch.impress.co.jp")
                                    :entry-title
                                    (rx "【山田祥平のRe:config.sys】")
                                    :add 'prime))
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-title (rx "デモクラシータイムス.")
                                    :entry-title
                                    (rx "WeN")
                                    :add 'prime))
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-title (rx "Jリーグ ニュース")
                                    :entry-title
                                    (rx (or "入籍" "子が誕生"))
                                    :remove 'unread))
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-title (rx "BSAsahi")
                                    :entry-title
                                    (rx "町山智浩のアメリカの今を知るTV")
                                    :add 'unread)
                20)
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-title (rx "BSAsahi")
                                    :remove 'unread)
                10)
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-title (rx "DAZN Japan")
                                    :entry-title
                                    (rx (and "ハイライト"
                                             (0+ anychar)
                                             "明治安田生命J1リーグ"))
                                    :add 'unread)
                20)
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-title (rx "DAZN Japan")
                                    :remove 'unread)
                10)
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-title (rx "Jリーグ公式チャンネル")
                                    :entry-title
                                    (rx (and "ハイライト"
                                             (0+ anychar)
                                             "ジュビロ磐田"))
                                    :add 'unread)
                20)
      (add-hook 'elfeed-new-entry-hook
                (elfeed-make-tagger :feed-title (rx "Jリーグ公式チャンネル")
                                    :remove 'unread)
                10))
    :config
    (elfeed-org))
  )

(leaf *deepl-translate
  :commands my:deepl-translate
  :bind (("C-x T" . my:deepl-translate))
  :preface
  (require 'url-util)
  (defun my:deepl-translate (&optional string)
    (interactive)
    (setq string
          (cond ((stringp string) string)
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (save-excursion
                   (let (s)
                     (forward-char 1)
                     (backward-sentence)
                     (setq s (point))
                     (forward-sentence)
                     (buffer-substring s (point)))))))
    (run-at-time 0.1 nil 'deactivate-mark)
    (browse-url
     (concat
      "https://www.deepl.com/translator#en/ja/"
      (url-hexify-string string)
      )))
  )

(leaf google-translate
  :ensure t
  :defvar google-translate-backend-method
  :init
  (defvar google-translate-translation-directions-alist '(("en" . "ja") ("ja" . "en")))
  (leaf popup :ensure t)
  (defun my:google-translate--search-tkk ()
    "Search TKK. @see https://github.com/atykhonov/google-translate/issues/52"
    (list 430675 2721866130))
  :bind
  ("C-x t" . google-translate-smooth-translate)
  :advice (:override google-translate--search-tkk
                     my:google-translate--search-tkk)
  :config
  (setq google-translate-translation-directions-alist '(("en" . "ja") ("ja" . "en"))
        google-translate-backend-method 'curl)
  )

(leaf flyspell
  :if (executable-find "aspell")
  :blackout (flyspell-mode . "F")
  :custom
  `((ispell-program-name   . "aspell")
    (ispell-check-comments . nil)
    (ispell-skip-html      . t)
    (ispell-silently-savep . t)
    )
  :bind (:flyspell-mode-map
         ("C-." . nil)
         ("C-," . nil))
  :defer-config
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  )

(leaf lookup
  :if (and (file-exists-p "/etc/emacs/site-start.d/50lookup-el.el")
           (file-exists-p "/usr/local/share/dict/lookup-enabled"))
  :commands (lookup lookup-region lookup-pattern)
  :bind (("C-c w" . lookup-pattern)
         ("C-c W" . lookup-word))
  :custom
  (lookup-search-agents
   . '((ndeb "/usr/local/share/dict/eijiro" :alias "英辞郎")
       (ndeb "/usr/local/share/dict/waeijiro" :alias "和英辞郎")
       (ndeb "/usr/local/share/dict/rikagaku5" :alias "理化学辞典 第5版")
       (ndeb "/usr/local/share/dict/koujien4" :alias "広辞苑 第4版")
       (ndeb "/usr/local/share/dict/wadai5" :alias "研究社 和英大辞典 第5版")
       ;; (ndeb "/usr/local/share/dict/eidai6" :alias "研究社 英和大辞典 第6版")
       ;; (ndeb "/usr/local/share/dict/colloc" :alias "研究社 英和活用大辞典 ")
       ))
  )

(leaf text-adjust :vc (:url "https://github.com/uwabami/text-adjust.el"))

(leaf flycheck
  :ensure t
  :disabled t
  ;; :hook (prog-mode-hook . flycheck-mode)
  :custom ((flycheck-display-errors-delay . 0.3))
  :config
  (leaf flycheck-popup-tip
    :ensure t
    :hook (flycheck-mode-hook . flycheck-popup-tip-mode))
  ;; (leaf flycheck-inline
  ;;   :ensure t
  ;;   :hook (flycheck-mode-hook . flycheck-inline-mode))
  ;;
  (flycheck-define-checker textlint
    "A linter for text."
    :command ("textlint-wrapper.sh" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode wl-draft-mode draft-mode))
  :hook
  ((text-mode-hook         . flycheck-mode)
   (markdown-mode-hook     . flycheck-mode)
   (gfm-mode-hook          . flycheck-mode)
   (org-mode-hook          . flycheck-mode)
   (mu4e-compose-mode-hook . flycheck-mode))
  )

(leaf smartparens
  :ensure t
  :blackout t
  :defun (sp-pair)
  :hook (emacs-startup-hook . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  )

(leaf rainbow-mode
  :ensure t
  ;; :blackout `((rainbow-mode . ,(format " %s" "\x1F308")))
  )

(leaf project
  :ensure t
  :custom
  `((project-list-file . ,(expand-file-name "projects" my:d:tmp))
    (project-switch-use-entire-map . t)
    )
  )

(leaf git
  :config
  (leaf consult-ghq
    :vc (:url "https://github.com/uwabami/consult-ghq")
    :bind (("C-x g f" . consult-ghq-open))
    :custom
    `((consult-ghq-short-list . t)))

  (leaf magit
    :bind
    (("C-x g s" . magit-status))
    :ensure t
    :custom
    `((magit-completing-read-function . 'magit-builtin-completing-read)
      (magit-refs-show-commit-count   . 'all)
      (magit-log-buffer-file-locked   . t)
      (magit-revision-show-gravatars  . nil)))

  (leaf transient
    :custom
    `((transient-history-file
       . ,(expand-file-name "transient-history.el" my:d:tmp))
      (transient-levels-file
       . ,(expand-file-name "transient-levels.el" my:d:tmp))
      (transient-values-file
       . ,(expand-file-name "transient-values.el" my:d:tmp))
      (transient-force-fixed-pitch . t)
      (transient-default-level . 5)))

  (leaf orgit
    :ensure t)

  (leaf browse-at-remote
    :ensure t
    :bind
    (("C-x g g" . browse-at-remote)))

  (leaf git-gutter-fringe
    :ensure t
    :custom
    `((git-gutter:update-interval . 0.02)
      (git-gutter:update-hooks . '(after-save-hook after-revert-hook)))
    :hook
    (emacs-startup-hook . global-git-gutter-mode))
  )

(leaf lua-mode :ensure t)
(leaf ssh-config-mode :ensure t)
(leaf fish-mode :ensure t :mode "\\.fish\\'")
(leaf rd-mode
  :mode "\\.rd\\'"
  :hook
  (rd-mode-hook . rd-show-other-block-all))
(leaf scss-mode
  :ensure t
  :init
  (leaf css-mode :custom (css-indent-offset . 2))
  :mode "\\.sass\\'" "\\.scss\\'")
(leaf generic-x)
(leaf systemd :ensure t)
;; (leaf *misc-mode
;;   :init
;;   (leaf systemd :ensure t)
;;   (leaf debian-el
;;     :custom
;;     `((debian-bug-download-directory . "~/Downloads"))
;;     )
(leaf yaml-mode :ensure t :mode "\\.yml\\'" "\\.yaml\\'")
;;   (leaf textile-mode :ensure t)
;;   (leaf dpkg-dev-el)
;;   (leaf sh-mode
;;     :custom ((system-uses-terminfo . nil))
;;     )
;;   (leaf apt-sources-list
;;     :custom
;;     ((apt-sources-list-suites
;;       . '("stable" "stable-backports"
;;           "testing" "testing-backports"
;;           "unstable" "experimental"
;;           "jessie" "jessie-backports"
;;           "stretch" "stretch-backports"
;;           "buster" "buster-backports"
;;           "bullseye" "bullseye-backports"
;;           "sid")))
;;     )
;;   (leaf info-colors
;;     :ensure t
;;     :hook
;;     (Info-selection #'info-colors-fontify-node))
;;   )

(leaf markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-indent-on-enter 'indent-and-new-item)
  )

(leaf org
  :ensure t
  :blackout `((org-mode . ,(all-the-icons-icon-for-mode 'org-mode)))
  :preface
  (require 'cl)
  (defconst org-relate-property "PARENT"
    "Property name for child nodes to look up parent node.")
  (defcustom org-relate-parent-tag-list
    '("Office" "refile")
    "A list of tags all parent nodes must have one of them.")
  (defcustom org-relate-target-files
    (file-expand-wildcards (concat my:d:org "/**/*.org"))
    "A list of target files to search relation nodes.")

  (defun org-relate-search ()
    "Search for entries related to the entry at point.

User who would like to search for parent and children of that, namely siblings,
 must press 'p'.
User who would like to search for children of this entry must press 'c'.
User who would like to search for friends either pointing to this entry
 or pointed from this entry must be press 'f'.
User who would like to search for all listed above must press 'a'."
    (interactive)
    (let* ((org-super-agenda-properties-inherit nil)
           (char (read-char-from-minibuffer "Target: [a]ll [p]arent [c]hild [f]riend"))
           (queries (seq-remove 'null
                                (cond
                                 ((equal char ?a) (list (org-relate--search-parent-org-ql-query)
                                                        (org-relate--search-children-org-ql-query)
                                                        (org-relate--search-friends-refer-org-ql-query)
                                                        (org-relate--search-friends-referred-org-ql-query)
                                                        (org-relate--search-siblings-org-ql-query)))
                                 ((equal char ?p) (list (org-relate--search-parent-org-ql-query)
                                                        (org-relate--search-siblings-org-ql-query)))
                                 ((equal char ?c) (list (org-relate--search-children-org-ql-query)))
                                 ((equal char ?f) (list (org-relate--search-friends-refer-org-ql-query)
                                                        (org-relate--search-friends-referred-org-ql-query)))))))
      (if queries
          (org-ql-search org-relate-target-files
                         (seq-reduce (lambda (accum elem)
                                       (push elem (cdr (last accum)))
                                       accum)
                                     (mapcar 'car queries)
                                     '(or))
                         :super-groups (mapcar 'cdr queries))
        (message "No query found"))))

  (defun org-relate--search-children-org-ql-query ()
    (when-let* ((id (org-id-get))
                (rel-prop-link (format "[[id:%s]]" id)))
      `((or (property ,org-relate-property ,id)
            (property ,org-relate-property ,rel-prop-link)) .
            (:name "Children" :property (,org-relate-property ,id)))))

  (defun org-relate--search-parent-org-ql-query ()
    (if-let* ((prop-ref (org-entry-get (point) org-relate-property))
              (index (string-match (format "\\(%s\\)" thing-at-point-uuid-regexp) prop-ref))
              (parent-id (match-string 0 prop-ref)))
        `((property "ID" ,parent-id) .
          (:name "Parent" :property ("ID" ,parent-id)))))

  (defun org-relate--search-siblings-org-ql-query ()
    (if-let* ((prop-ref (org-entry-get (point) org-relate-property))
              (index (string-match (format "\\(%s\\)" thing-at-point-uuid-regexp) prop-ref))
              (parent-id (match-string 0 prop-ref))
              (parent-id-link (format "[[id:%s]]" parent-id)))
        `((or (property ,org-relate-property ,parent-id)
              (property ,org-relate-property ,parent-id-link)) .
              (:name "Siblings" :property (,org-relate-property ,parent-id)))))

  (defun org-relate--search-friends-refer-org-ql-query ()
    (when-let* ((id (org-id-get))
                (ref-re (format "\\[\\[id:%s\\].*\\]" id)))
      `((regexp ,ref-re) .
        (:name "Friends pointing to this entry" :regexp ,ref-re))))

  (defun org-relate--search-friends-referred-org-ql-query ()
    (let* ((id-link-re (format "\\[\\[id:\\(%s\\)\\].*\\]" thing-at-point-uuid-regexp))
           (end (or (save-excursion
                      (outline-next-heading))
                    (point-max))))
      (when (save-excursion
              (re-search-forward id-link-re end t))
        `((property "ID" ,(match-string 1)) .
          (:name "Friends to whom this entry is pointing" :property ("ID" ,(match-string 1)))))))

  (defun org-relate-interrelate ()
    "Make parent-child relationship by inserting property.

This command must be called in parent node which should have one of `org-relate-parent-tag-list'."
    (interactive)
    (save-excursion
      (org-back-to-heading)
      (if (some (lambda (parent-tag)
                  (member parent-tag (org-get-tags)))
                org-relate-parent-tag-list)
          (let ((ref-id (org-id-get-create)))
            (and (org-goto-first-child)
                 (cl-labels ((set-ref-id-to-siblings
                              (ref-link)
                              (org-set-property org-relate-property ref-link)
                              (and (org-goto-sibling)
                                   (set-ref-id-to-siblings ref-link))))
                   (set-ref-id-to-siblings ref-id)))
            (message "Interrelated has done."))
        (message "This entry does not compliant with 'org-relate-parent-tag-list"))))

  (defun org-agenda-relation-interrelate ()
    "Invoke `org-relate-interrelate' in agenda view."
    (interactive)
    (org-agenda-check-no-diary)
    (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error)))
           (buffer (marker-buffer hdmarker))
           (pos (marker-position hdmarker))
           (inhibit-read-only t)
           newhead)
      (org-with-remote-undo buffer
        (with-current-buffer buffer
          (widen)
          (goto-char pos)
          (org-show-context 'agenda)
          (call-interactively 'org-relate-interrelate)
          (end-of-line 1)
          (setq newhead (org-get-heading))))))
  :bind
  (("M-p"       . org-shiftup)
   ("M-n"       . org-shiftdown)
   ("C-c l"     . org-store-link)
   ("C-c c"     . org-capture)
   ("C-c a"     . org-agenda)
   ("C-c C-x %" . #'org-relate-search)
   ("C-c C-x &" . #'org-relate-interrelate)
   ("C-c C-x $" . my:org-archive-subtree)
   (:org-mode-map
   ("C-c j h"   . consult-org-heading)
   ("C-c j a"   . consult-org-agenda)
   ("C-'"       . embark-dwim)))
  :advice
  (:before org-calendar-holiday
           (lambda () (require 'japanese-holidays nil 'noerror)))
  :custom
  `(;; Dropbox に保存する
    (org-directory              . ,(expand-file-name my:d:org))
    ;; インデントする
    (org-adapt-indentation      . nil)
    ;; 折り返し無し
    (org-startup-truncated      . t)
    ;; DONEの時刻を記録
    (org-startup-folded         . 'show2levels)
    ;; 見出しの初期状態(見出しだけ表示)
    (org-log-done               . 'time)
    ;; link handler → xdg-open 任せ
    (org-file-apps              . '((auto-mode . emacs)
                                   (directory . emacs)
                                   ("\\.mm\\'" . default)
                                   ("\\.x?html?\\'" . "open %s")
                                   ("\\.pdf\\'" . "open %s")))
    (org-file-apps-macos        . '((remote . emacs)
                                    (system . "open %s")
                                    ("ps.gz" . "gv %s")
                                    ("eps.gz" . "gv %s")
                                    ("dvi" . "xdvi %s")
                                    ("fig" . "xfig %s")
                                    ("\\.docx?\\'" . "open %s")
                                    ("\\.xlsx?\\'" . "open %s")
                                    ("\\.pptx?\\'" . "open %s")
                                    (t . "open %s")))
    (org-file-apps-gnu          . '((remote . emacs)
                                    (system . "xdg-open %s")
                                    (t      . "xdg-open %s")))
    ;; Archive.org の位置指定
    (org-archive-location       . ,(expand-file-name "Archive.org::" my:d:org))

    (org-todo-keywords          . '((sequence "Open(o)" "In Progress(p!/@)" "|" "Resolved(r!)" "Closed(c@)")))
    (org-todo-keyword-faces     . '(("Open"        . (:foreground "#ff4500" :weight bold))
                                    ("In Progress" . (:foreground "#4169e1" :weight bold))
                                    ("Resolved"    . (:foreground "#008000" :weight bold))
                                    ("Closed"      . (:foreground "#9acd32" :weight bold))))
    ;; GTD: タグの追加
    (org-tag-alist              . '(("Meeting"     . ?m)
                                    ("Document"    . ?d)
                                    ("Kitting"     . ?k)
                                    ("Survey"      . ?s)
                                    ("Office"      . ?o)))
    (org-refile-targets         .  `((org-agenda-files :tag . "Office")
                                     (,(file-expand-wildcards (concat my:d:org "/**/*.org")) :tag . "refile")))
    (org-global-properties      . '(("Effort_ALL". "0:00 0:10 0:20 0:30 1:00 1:30 2:00 3:00 4:00 5:00 6:00 8:00")))
    (org-highest-priority       . ?A)
    (org-lowest-priority        . ?Z)
    (org-default-priority       . ?E))
  )

(leaf org-id
  :commands
  (my:org-id-add-custom-id
   my:org-id-get-custom-id
   my:org-custom-id-get
   my:org-id-add-to-headlines-in-file
   my:org-id-delete-all-id-in-file
   )
  :init
  (leaf org-macs :commands org-with-point-at)
  :custom
  `((org-id-locations-file
     . ,(expand-file-name "org-id-locations" my:d:tmp))
    (org-id-link-to-org-use-id . 'create-if-interactive-and-no-custom-id)
    )
  :config
  (defun my:org-id-add-custom-id ()
    "Add \"CUSTOM_ID\" to the current tree if not assigned yet."
    (interactive)
    (my:org-custom-id-get nil t))
  ;;
  (defun my:org-id-get-custom-id ()
    "Return a part of UUID with an \"org\" prefix.
e.g. \"org3ca6ef0c\"."
    (let* ((id (org-id-new "")))
      (when (org-uuidgen-p id)
        (downcase (concat "org"  (substring (org-id-new "") 0 8))))))
  ;;
  (defun my:org-custom-id-get (&optional pom create)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
See https://writequit.org/articles/emacs-org-mode-generate-ids.html"
    (interactive)
    (eval-when-compile (require 'org-macs))
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (my:org-id-get-custom-id))
          (unless id
            (error "Invalid ID"))
          (org-entry-put pom "CUSTOM_ID" id)
          (message "--- CUSTOM_ID assigned: %s" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))
  ;;
  (defun my:org-id-delete-all-id-in-file ()
    (interactive)
    (goto-char 1)
    (while (not (eq (point) (point-max)))
      (org-next-visible-heading 1)
      (let ((id (org-entry-get (point) "ID")))
        (when id
          (message "ID: %s" id)
          (org-delete-property "ID"))))
    (message "--- done."))
  ;;
  (defun my:org-id-add-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current file.
See https://writequit.org/articles/emacs-org-mode-generate-ids.html"
    (interactive)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+options:.*auto-id:t" (point-max) t)
        (org-map-entries
         (lambda () (my:org-custom-id-get (point) 'create))))))
  ;;
  :hook (before-save-hook
         . (lambda ()
             (when (and (eq major-mode 'org-mode)
                        (eq buffer-read-only nil))
               (my:org-id-add-to-headlines-in-file))))
  )

(leaf ob-yaml
  :el-get llhotka/ob-yaml
  )

(leaf org-babel
  :blackout `((org-src-mode . ,(format " %s" (all-the-icons-octicon "code"))))
  :custom
  `(;; font-lock
   (org-src-fontify-natively         . t)
   ;; TAB の挙動
   (org-src-tab-acts-natively        . t)
   ;; インデント
   (org-edit-src-content-indentation . 0)
   ;; load languages
   (org-babel-load-languages
    . '((emacs-lisp . t)
        (shell . t)
        (python . t)
        (ruby . t)
        (yaml . t)
        (org . t))))
  )

(leaf org-roam
  :if (and (file-directory-p my:d:org)
           (and (executable-find "rg")
                (executable-find "sqlite3")))
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :commands (org-roam-db-query)
  :bind
  (("C-c n a" . org-roam-alias-add)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n o" . org-id-get-create)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n l" . org-roam-buffer-toggle)
   (:org-mode-map
    ("C-M-i"   . completion-at-point)))
  :custom
  `((org-roam-db-location . ,(expand-file-name "org-roam.db" my:d:tmp))
    (org-roam-directory   . ,(expand-file-name "roam" my:d:org))
    ;; (org-roam-mode-section-functions
    ;;  . (list #'org-roam-backlinks-section
    ;;          #'org-roam-reflinks-section
    ;;         ;; #'org-roam-unlinked-references-section
    ;;         ))
    ;; (org-roam-db-update-on-save . t)
    )
  :defer-config
  (org-roam-db-autosync-enable)
  )
;;
(leaf org-roam-dailies
  :commands (org-roam-dailies--capture)
  :preface
  (defun my:org-roam-dailies-capture-without-templates ()
    (interactive)
    (org-roam-dailies--capture (current-time) nil "m"))
  :pl-setq
  (my:org:calendar1 my:org:calendar2 my:org:calendar3)   ;; 名前がイケてないっ!
  :bind
  `(("C-c n d" . org-roam-dailies-map)
    ("C-x M"   . org-roam-dailies-capture-today)
    ("C-x m"   . my:org-roam-dailies-capture-without-templates)
   (:org-roam-dailies-map
    ("Y" . org-roam-dailies-capture-yesterday)
    ("T" . org-roam-dailies-capture-tomorrow))
   )
  :custom
  `(
    (org-roam-dailies-directory . "") ;; relative path→flatten!
    (org-roam-dailies-capture-templates
     . '(
         ("m" "memo" entry "* MEMO <%<%Y-%m-%d %H:%M>> %^{title}\n  %?"
          :target (file+head "%<%Y-%m-%d>.org"
                             "#+title: %<%Y年%m月%d日(%a)>")
          :prepend nil
          :kill-buffer t
          )
         ("t" "ToDo" entry "* TODO <%<%Y-%m-%d %H:%M>> %^{title}\n  %?"
          :target (file+head "%<%Y-%m-%d>.org"
                             "#+title: %<%Y年%m月%d日(%a)>")
          :prepend nil
          :kill-buffer t
          )
         ("s" "Schedule: 個人スケジュール" entry
          "* %^{title}\n  :PROPERTIES:\n  :calendar-id: %(format \"%s\" my:org:calendar1)\n  :org-gcal-managed: org\n  :END:\n  :org-gcal:\n%?\n%i\n  :END:"
          :target (file ,(expand-file-name "Schedule.org" my:d:org))
          :prepend t   ;; prepend nil としたいが, 末尾まで行ってくれないので諦める.
          :kill-buffer t
          )
         ("u" "Univ: 大学関連 スケジュール" entry
          "* %^{title}\n  :PROPERTIES:\n  :calendar-id: %(format \"%s\" my:org:calendar2)\n  :org-gcal-managed: org\n  :END:\n  :org-gcal:\n%?\n%i\n  :END:"
          :target (file ,(expand-file-name "Univ.org" my:d:org))
          :prepend t   ;; prepend nil としたいが, 末尾まで行ってくれないので諦める.
          :kill-buffer t
          )
         ("g" "GFD 関連 スケジュール" entry
          "* %^{title}\n  :PROPERTIES:\n  :calendar-id: %(format \"%s\" my:org:calendar3)\n  :org-gcal-managed: org\n  :END:\n  :org-gcal:\n%?\n%i\n  :END:"
          :target (file ,(expand-file-name "GFD.org" my:d:org))
          :prepend t   ;; prepend nil としたいが, 末尾まで行ってくれないので諦める.
          :kill-buffer t
          )
         ))
    )
  )

(leaf vulpea
  :if (and (file-directory-p my:d:org)
           (and (executable-find "rg")
                (executable-find "sqlite3")))
  :doc "https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html"
  :ensure t
  :commands
  (vulpea-buffer-prop-get
   vulpea-buffer-tags-get
   my:vulpea-project-p
   my:vulpea-project-update-tag
   my:vulpea-buffer-p
   my:vulpea-project-files
   my:vulpea-agenda-files-update
   )
  :preface
  (defun my:vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (org-element-map
        (org-element-parse-buffer 'headline)
        'headline
      (lambda (h)
        (eq (org-element-property :todo-type h)
            'todo))
      nil 'first-match))

  (defun my:vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (my:vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (my:vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun my:vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun my:vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"project\"%"))]))))

  (defun my:vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (append my:org-agenda-files (my:vulpea-project-files))))
  :hook
  ((find-file-hook . my:vulpea-project-update-tag)
   (before-save-hook . my:vulpea-project-update-tag)
   (org-roam-db-autosync-mode . vulpea-db-autosync-enable))
  )

(leaf org-agenda
  :if (file-directory-p my:d:org)
  :defer-config
  (defvar org-agenda-files-default
    (file-expand-wildcards (concat my:d:org "/agenda/*.org"))
    "Default org-agenda-files.")
  (setq org-agenda-files (append org-agenda-files-default))
  :bind
  (:org-agenda-mode-map
   ("&" . org-agenda-relation-interrelate)
   ("$" . my:org-agenda-archive-subtree)
   ("C" . my:org-archive-find-date))
  )

(leaf org-ql
  :ensure t
  :preface
;; TODO: This function can be rewritten with org-ml
;;;###autoload
  (defun org-timestamps-in-entry ()
    "Return timestamp objects for all Org timestamps in entry."
    (interactive (list current-prefix-arg))
    (save-excursion
      (goto-char (org-entry-beginning-position))
      (org-show-entry)
      (org-narrow-to-element)
      (let* ((parsetree (org-element-parse-buffer))
             (ts-list nil))
        (org-element-map parsetree '(planning clock timestamp)
          (lambda (elm)
            (case (org-element-type elm)
              ('planning
               (add-to-list 'ts-list (ts-parse-org-element (or (org-element-property :closed elm)
                                                               (org-element-property :scheduled elm)
                                                               (org-element-property :deadline elm)))
                            t))
              ('clock
               (add-to-list 'ts-list (ts-parse-org-element (org-element-property :value elm)) t))
              ('timestamp
               (add-to-list 'ts-list (ts-parse-org-element elm) t)))))
        (widen)
        ts-list)))
  :preface
  (require 'ts)
  (require 'org)
  (require 'org-ql-search)
  ;;; my:org-archived
  (defvar org-agenda-files-default
    (file-expand-wildcards (concat my:d:org "/*.org")))
  (defun my:org-archive-file (&optional year)
    "Return a path of archive f

If optional argument `YEAR' is passed that year's file is returned instead of current year's."
    (let* ((record-year (or year (ts-year (ts-now))))
           (record-file-cand (format "%s/archive/archive_%s.org" my:d:org record-year))
           (record-file
            (if (file-exists-p record-file-cand)
                record-file-cand
              (expand-file-name "archive/archive_0000.org" my:d:org))))
      (if (or (file-exists-p record-file)
              (file-symlink-p record-file))
          record-file
        nil)))

  (defvar my:org-archive-file (my:org-archive-file))

  (defun my:org-archive-files ()
    "Return list of archive files."
    (append (sort (file-expand-wildcards (format "%s/archive/archive_*.org" my:d:org)) 'string<)
            org-agenda-files-default))

  (defvar my:org-archive-files (my:org-archive-files))

  (defun my:org-archive-find-date (date)
    "Find and visit the location of DATE in archivee file.

DATE must be a string representing the date to find and parsable with `format-time-string'.

If called interactively, it prompt the user to select the date to find."
    (interactive
     (cond
      ((eq major-mode 'calendar-mode)
       (list (calendar-date-string (calendar-cursor-to-date))))
      ((eq major-mode 'org-agenda-mode)
       (let* ((day (or (get-text-property (min (1- (point-max)) (point)) 'day)
                       (user-error "Don't know which date to open in calendar")))
              (date (calendar-gregorian-from-absolute day)))
         (list (calendar-date-string date))))
      (t (let ((date-select (org-read-date)))
           (list date-select)))))
    (let* ((d (parse-time-string date))
           (day (decoded-time-day d))
           (month (decoded-time-month d))
           (year (decoded-time-year d)))
      (find-file (my:org-archive-file year))
      (org-datetree-find-iso-week-create `(,month ,day ,year))))

  (defun my:org-archive-subtree ()
    "Refile current subtree to archive file using latest timestamp."
    (interactive)
    (let* ((ts (car (sort (org-timestamps-in-entry) #'ts>)))
           (year (ts-year (or ts (ts-now))))
           (save-file (my:org-archive-file year)))
      (when-let* ((pos (with-current-buffer (find-file-noselect save-file)
                         (save-excursion
                           (if (and (member "Office" (org-get-local-tags))
                                    (member (org-get-todo-state) '("Resolved" "Closed")))
                               (org-find-exact-headline-in-buffer "untouched items")
                             (org-datetree-find-iso-week-create `(,(ts-month ts)
                                                                  ,(ts-day ts)
                                                                  ,(ts-year ts)))
                           (point))))))
        (org-refile nil nil (list nil save-file nil pos)))
      (org-save-all-org-buffers)
      (setq this-command 'my:org-archive-subtree)))

  (defun my:org-agenda-archive-subtree ()
    "Refile the entry or subtree belonging to the current agenda entry."
    (interactive)
    (org-agenda-archive-with 'my:org-archive-subtree))

  (defun my:org-ql-view-archive-subtree ()
    "Refile the entry or subtree belonging to the current agenda entry."
    (interactive)
    (org-agenda-archive-with 'my:org-archive-subtree)
    (org-ql-view-refresh))

  (defun my:org-archive-search (query)
    "Search org entries matched QUERY in archive files using `org-ql-search'."
    (interactive (list (read-string "Query: ")))
    (let ((files (my:org-archive-files)))
      (org-ql-search files query)))

  (push '("Archive entries" . my:org-archive-search)
        org-ql-views)

  (defun my:lookup-org-archive (word &optional arg)
    "Look up WORD in my archive files using `org-ql-search'."
    (interactive (list (if (use-region-p)
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Word: "))
                       current-prefix-arg))
    (org-ql-search
      my:org-archive-files
      (cond
       ((equal arg '(4)) `(heading ,word))
       (t `(regexp ,word)))))

  :bind
  (:org-ql-view-map
   ("$" . my:org-ql-view-archive-subtree))
  :custom
  `(;; 再帰的に検索
    (org-ql-search-directories-files-recursive  . t))
  )

(leaf *org-clock
  :after org
  :preface
  (defun org-clock-sum-all ()
    "Sum the times for all agenda files."
    (interactive)
    (save-excursion
      (mapc (lambda (file)
              (with-current-buffer (or (org-find-base-buffer-visiting file)
                                       (find-file-noselect file))
                (org-clock-sum)
                (org-clock-sum-today)))
            (org-agenda-files))))
  :hook
  ((org-clock-out org-clock-cancel) .
   (lambda () (and (boundp 'org-timer-countdown-timer)
                   org-timer-countdown-timer
                   (org-timer-stop))))
  ((org-clock-in org-clock-out org-clock-cancel) . save-buffer)
  (org-clock-in . (lambda ()
                    (let* ((opt '(4))
                           (attention-span (org-entry-get (point) "ATTENTION_SPAN" 'selective))
                           (effort (org-entry-get (point) "Effort" 'selective))
                           (org-timer-default-timer
                            (if (and (stringp attention-span)
                                     (< 0 (length attention-span)))
                                (progn
                                  (setq opt '(64))
                                  attention-span)
                              (default-value 'org-timer-default-timer)))
                           (time-default (decode-time (current-time))))
                      (when (or
                             ;; if "Effort" is less than 1:40 it's useless as timer value
                             (and (stringp effort)
                                  (apply #'time-less-p
                                         (mapcar (lambda (time)
                                                   (apply 'encode-time (mapcar* (lambda (x y) (or x y))
                                                                                (parse-time-string time)
                                                                                time-default)))
                                                 `(,effort "1:40"))))
                             attention-span)
                        (org-timer-set-timer opt)))))
  :custom
  (org-clock-out-when-done . t)
  (org-clock-persist . t)
  (org-clock-persist-query-resume . nil)
  (org-clock-string-limit . 20)
  ;(org-clock-continuously . t)
  (org-clock-ask-before-exiting . nil)
  (org-clock-mode-line-total . 'today)
  :bind
  `(("C-c C-x ="   . org-clock-sum-all)
    ("C-c C-x C-j" . org-clock-goto))
  :defer-config
  (defconst org-clock-ts-line-re
    (concat "^[ \t]*" org-clock-string "[ \t]*" org-tsr-regexp-both)
    "Matches a line with clock time stamp.")
  :config
  (org-clock-persistence-insinuate)
  )

(leaf org-life
  :after org org-ql
  :config
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-include-diary t)
  (setq org-agenda-custom-commands
        `(("n" "Agenda and all TODOs"
           ((agenda #1="")
            (alltodo #1#)))
          ("l" "Log entries in a week"
           agenda ""
           ((org-agenda-span (if (equal current-prefix-arg '(4))
                                 'day 'week))
            (org-agenda-start-with-log-mode t)
            (org-agenda-include-inactive-timestamps nil)
            (org-agenda-include-diary t)
            (org-agenda-sorting-strategy
             '(time-up
               deadline-up
               todo-state-up
               priority-down))))
          ("L" "Log entry timeline on today with default org-agenda-prefix-format"
           agenda ""
           ((org-agenda-prefix-format (eval (car (get 'org-agenda-prefix-format 'standard-value))))
            (org-agenda-span (if (equal current-prefix-arg '(4))
                                 'day 'week))
            (org-agenda-start-with-log-mode t)
            (org-agenda-include-inactive-timestamps nil)
            (org-agenda-include-diary t)
            (org-agenda-sorting-strategy
             '(time-up
               deadline-up
               todo-state-up
               priority-down))))
          ;; KEEP IN MIND
          ;; invoking `org-clock-sum-all' is required before showing effort table
          ("k" . "Effort table")
           ("kt" "today"
            ((org-ql-search-block `(or (todo ,"Open")
                                       (todo ,"In Progress")
                                       (and (clocked :on today)
                                            (or (todo) (done))
                                            (not (habit))
                                            (not (tags "web"))))
                                  ((org-ql-block-header "Today's task"))))
            ((org-agenda-overriding-header "Today's Task")
             (org-overriding-columns-format "%46ITEM(Task) %Effort(Effort){:} %CLOCKSUM_T(Today){:} %CLOCKSUM(Total)")
             (org-agenda-view-columns-initially t)
             (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
           ("kw" "this week"
            ((org-ql-search-block `(or (todo ,"Open")
                                       (todo ,"In Progress"))
                                  ((org-ql-block-header "This Week's task"))))
            ((org-agenda-overriding-header "This Week's Task")
             (org-overriding-columns-format "%46ITEM(Task) %Effort(Effort){:} %CLOCKSUM_T(Today){:} %CLOCKSUM(Total)")
             (org-agenda-view-columns-initially t)
             (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))
           ("kd" "done task"
            ((org-ql-search-block `(or (todo ,"Resolved")
                                       (todo ,"Closed"))
                                  ((org-ql-block-header "Done task"))))
            ((org-agenda-overriding-header "Done Task")
             (org-overriding-columns-format "%46ITEM(Task) %Effort(Effort){:} %CLOCKSUM(Total){:}")
             (org-agenda-view-columns-initially t)
             (org-agenda-sorting-strategy '(todo-state-up priority-down deadline-up))))))

  (defvar org-capture-todo-file (concat my:d:org "/priv_a/life.org"))

  (defun org-goto-clocking-or-today ()
    "Go to currently clocking entry.

If no entry is clocked or CATEGORY on clocking entry is \"Cyclic\",
go to today's entry in record file."
    (if (and (org-clocking-p)
             (save-excursion
               (with-current-buffer (org-clocking-buffer)
                 (org-clock-jump-to-current-clock)
                 (org-back-to-heading)
                 (not (string=
                       (org-entry-get (point) "CATEGORY" t)
                       "Cyclic")))))
        (org-clock-goto)
      (let* ((now (decode-time (current-time)))
             (day (nth 3 now))
             (month (nth 4 now))
             (year (nth 5 now))
             (org-refile-targets
              `((,my:org-archive-file :regexp . ,(format "%04d-%02d-%02d" year month day)))))
        (find-file my/org-archive-file)
        (org-datetree-find-iso-week-create `(,month ,day ,year) nil))))

  (leaf org-capture
    :init
    (setq org-capture-templates
          `(("p" "Project"
             entry (id "0A6350F7-CD44-4B14-B8CB-E01EE068D242")
             "* %? [/] :project:\n %U\n  - [ ] insert ID property if necessary"
             :prepend t :jump-to-captured t)
            ("D" "Drill")
            ("Dd" "Drill entry in currently clocking or today's entry."
             entry (function org-goto-clocking-or-today)
             "* %i :drill:\n[%?]")
            ("De" "English drill entry in currently clocking or today's entry."
             entry (function org-goto-clocking-or-today)
             "* %i :drill:fd_en:\n[%^C%?]\n- %a")))
    :custom
    (org-capture-bookmark . nil))
  )

;;;###autoload
(defun my:load-window-config ()
  "load window-system specific settings"
  (interactive)
  (progn
    (set-face-attribute 'default nil :family "HackGen Console NF" :height 130)
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208
                      (font-spec :family "HackGen Console NF")))
  )
(leaf *gui
  :if window-system
  :config
  (cond
   ;; 4K display
   ((= (display-pixel-height) 1692)
    (setq default-frame-alist
          (append (list
                   '(width  . 200)
                   '(height . 106)
                   '(top    . 72)
                   '(left   . 10)
                 )
                default-frame-alist)))
   ;; HD display
   ((= (display-pixel-height) 1080)
    (setq default-frame-alist
          (append (list
                   '(width  . 180)
                   '(height . 68)
                   '(top    . 22)
                   '(left   . 0)
                   )
                  default-frame-alist)))
   ;; MacBook Pro 14inch ディスプレイ
   ((= (display-pixel-height) 900)
    (setq default-frame-alist
          (append (list
                   '(width  . 140)
                   '(height . 56)
                   '(top    . 22)
                   '(left   . 0)
                   )
                  default-frame-alist)))
   ;; とりあえずその他 完全に未確認で分岐できる事を確認するためのコード
   (t
    (setq default-frame-alist
          (append (list
                   '(width  . 140)
                   '(height . 50)
                   '(top    . 22)
                   '(left   . 0)
                   )
                  default-frame-alist))))
  (set-frame-parameter nil 'alpha 100)
  (setq use-default-font-for-symbols nil)
  (scroll-bar-mode -1)
  (my:load-window-config)
  )

(leaf modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t)
  )

(leaf powerline
  :ensure t
  :init
  (defun my:major-mode-icon (mode)
    "Update file icon in mode-line, just display major-mode icon. not filename."
    (let* ((icon (all-the-icons-icon-for-mode mode)))
      (if (symbolp icon)
          (all-the-icons-faicon "file-code-o"
                                :face 'all-the-icons-dsilver
                                :height 1.0)
        icon)))
  :hook (emacs-startup-hook . my:powerline-theme)
  :config
;;;###autoload
  (defun my:powerline-theme ()
    "Setup the default mode-line."
    (interactive)
    (setq-default
     mode-line-format
     '("%e"
       (:eval
        (let* ((active (powerline-selected-window-active))
               (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
               (mode-line (if active 'mode-line 'mode-line-inactive))
               (face0 (if active 'powerline-active0 'powerline-inactive0))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (separator-left (intern (format "powerline-%s-%s"
                                               (powerline-current-separator)
                                               (car powerline-default-separator-dir))))
               (separator-right (intern (format "powerline-%s-%s"
                                                (powerline-current-separator)
                                                (cdr powerline-default-separator-dir))))
               (lhs (list (powerline-raw "%*" mode-line 'l)
                          (powerline-raw mode-line-mule-info mode-line 'l)
                          (powerline-raw (my:major-mode-icon major-mode) mode-line 'l)
                          (powerline-buffer-id mode-line-buffer-id 'l)
                          (powerline-raw " ")
                          ))
               (rhs (list (powerline-raw global-mode-string face1 'r)
                          (powerline-vc face1 'r)
                          (powerline-raw " ")
                          (powerline-raw "%4l" face1 'l)
                          (powerline-raw ":" face1 'l)
                          (powerline-raw "%3c" face1 'r)
                          (powerline-raw " " face1)
                          (powerline-raw "%6p" mode-line 'r)
                          )))
          (concat (powerline-render lhs)
                  (powerline-fill face2 (powerline-width rhs))
                  (powerline-render rhs)))))))
  )

(defun my:debug-on-quit-if-scratch (&rest _args)
  (setq debug-on-quit (string= (buffer-name) "*scratch*")))
(add-hook 'window-selection-change-functions 'my:debug-on-quit-if-scratch)

(leaf which-key
  :ensure t
  :custom
  `((which-key-show-early-on-C-h    . t)
    (which-key-idle-delay           . 10000)
    (which-key-idle-secondary-delay . 0.05))
  :hook (emacs-startup-hook . which-key-mode)
  )
;; (leaf tree-sitter :ensure t)
;; (leaf tree-sitter-langs :ensure t)
(leaf rg
  :if (executable-find "rg")
  :ensure t
  )

(leaf vterm
  :ensure t
  :custom
  `((vterm-always-compile-module . t))
  :hook
  (vterm-mode-hook
   . (lambda () (setq-local global-hl-line-mode nil)))
  )

(leaf nginx-mode :ensure t)
(leaf apache-mode :ensure t)
(leaf keg :ensure t)
(leaf keg-mode :ensure t)
(leaf esup
  :ensure t
  :custom
  ((esup-insignificant-time . 0.01)
   (esup-depth              . 0))
  )

(leaf popper
  :ensure t
  :bind (("C-`" . popper-toggle-latest)
         ("C-<tab>" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  :hook (emacs-startup-hook
         . (lambda ()
             (popper-mode)
             (popper-echo-mode)))
  )

(leaf *show-startup-time
  :hook
  (emacs-startup-hook
   . (lambda ()
       (message "init time: %.3f sec"
                (float-time (time-subtract after-init-time before-init-time)))))
  )

(setq debug-on-error nil)
;; (profiler-report)
;; (profiler-stop)

(provide 'init)
;; Local Variables:
;; byte-compile-warnings: (not obsolete cl-functions)
;; End:
