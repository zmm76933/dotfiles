;; -*- lexical-binding: nil -*-
;; (require 'profiler)
;; (profiler-start 'cpu)

(when load-file-name
  (setq user-emacs-directory
        (expand-file-name (file-name-directory load-file-name))))
(eval-and-compile
  (defconst my:d:share
    (expand-file-name "share/" user-emacs-directory))
  (defconst my:d:tmp
    (expand-file-name ".cache/emacs/" (getenv "HOME"))))

(eval-when-compile (require 'cl-lib nil t))

(eval-and-compile
  (setq byte-compile-warnings '(not cl-functions obsolete free-vars))
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (setq native-comp-speed  2
            native-comp-async-report-warnings-errors 'silent))
  (setq debug-on-error  t))

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

; cargo cult adaptation of event-apply-control-modifier
(defun my:event-apply-control-meta-modifiers (ignore-prompt)
  (vector
   (event-apply-modifier (event-apply-modifier (read-event)
                                               'control 26 "C-")
                         'meta 27 "M-")))
(define-key function-key-map (kbd "C-x @") 'my:event-apply-control-meta-modifiers)

;; elpa/gnutls workaround
(eval-and-compile
  (when (version<=  emacs-version "26.2")  ;; => for syntax hightlight
    (customize-set-variable 'gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
  (setq package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                           ("melpa"  . "https://melpa.org/packages/")
                           ("org"    . "https://orgmode.org/elpa/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
        package-gnupghome-dir (expand-file-name ".gnupg" (getenv "HOME")))
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (setq package-native-compile t)))
(eval-when-compile
  (unless (file-exists-p (expand-file-name "bootstrap-stamp" my:d:tmp))
    (package-refresh-contents)
    (with-temp-buffer
      (write-file (expand-file-name "bootstrap-stamp" my:d:tmp)))
    ))
(eval-and-compile (package-initialize))

(eval-and-compile
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf t))
  (leaf leaf-keywords
    :ensure t
    :init
    (leaf blackout :ensure t)
    (leaf el-get
      :ensure t
      :init (setq el-get-git-shallow-clone t)
      )
    :config
    (leaf-keywords-init)
    )
  )

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-arguments nil
        exec-path-from-shell-variables '("GPG_KEY_ID"
                                         "PASSWORD_STORE_DIR"
                                         "PATH"
                                         "SHELL"
                                         "SKKSERVER"
					                     "VIRTUAL_ENV"
                                         "WSL_DISTRO_NAME"))
  (when (window-system) (exec-path-from-shell-initialize))
  (defconst my:d:password-store
    (if (getenv "PASSWORD_STORE_DIR")
        (expand-file-name (concat "Emacs/" (system-name))
                          (getenv "PASSWORD_STORE_DIR")) nil))
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
    :init
    (setq plstore-secret-keys 'silent
          plstore-encrypt-to  (getenv "GPG_KEY_ID")))
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
    (auto-save-list-file-prefix . ,(expand-file-name ".saves-" my:d:tmp))
    (auto-save-default       . t)
    (auto-save-timeout       . 15)
    (auto-save-interval      . 60)
    (make-backup-files       . t)
    (backup-by-copying       . t)  ;; symlink は使わない
    (backup-directory-alist  . '(("." . ,my:d:tmp)))
    (auto-save-file-name-transforms . '((".*" ,my:d:tmp t)))
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
    (scroll-step             . 1)
    (scroll-preserve-screen-position . 'always)
    ;;
    (safe-local-variable-values
     . '((org-link-file-path-type . absolute)))
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
  :global-minor-mode delete-selection-mode)

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
  :blackout ((global-whitespace-mode . "")
             (whitespace-mode        . ""))
  :hook (emacs-startup-hook . global-whitespace-mode)
  :custom
  ((whitespace-line-column      . 72)
   (whitespace-style
    . '(face        ; faceを使う
        trailing    ; 行末の空白を対象.
        tabs        ; tab
        spaces      ; space
        ))
   (whitespace-display-mappings . '((space-mark ?\u3000 [?\□])
                                    (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
   (whitespace-space-regexp     . "\\(\u3000+\\)")
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
  (setq tramp-persistency-file-name (expand-file-name "tramp" my:d:tmp))
  :custom
  `((tramp-persistency-file-name
     . ,(expand-file-name "tramp" my:d:tmp))
    (tramp-completion-reread-directory-timeout . nil)
    (remote-file-name-inhibit-cache . nil)
    (vc-ignore-dir-regexp
     . ,(format "\\(%s\\)\\|\\(%s\\)"
                locate-dominating-stop-dir-regexp
                tramp-file-name-regexp))
    )
  :hook
  (kill-emacs-hook
   . (lambda ()
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
  (setq ps-mule-font-info-database-default
        '((iso-8859-1
           (normal nil nil))
          (katakana-jisx0201
           (normal builtin "Ryumin-Light-Katakana")
           (bold builtin "GothicBBB-Medium-Katakana"))
          (latin-jisx0201
           (normal builtin "Ryumin-Light-Hankaku")
           (bold builtin "GothicBBB-Medium-Hankaku"))
          (japanese-jisx0208
           (normal builtin "Ryumin-Light-Ext-H")
           (bold builtin "GothicBBB-Medium-Ext-H"))
          (japanese-jisx0213-2
           (normal builtin "Ryumin-Light-Ext-H")
           (bold builtin "GothicBBB-Medium-Ext-H"))
          (japanese-jisx0213.2004-1
           (normal builtin "Ryumin-Light-2004-H")
           (bold builtin "GothicBBB-Medium-H"))
          (unicode-bmp
           (normal builtin "Ryumin-Light-Ext-H")
           (bold builtin "GothicBBB-Medium-Ext-H"))
          )
        )
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

(leaf tab-bar-mode
  :init
  (defvar my:ctrl-t-map (make-sparse-keymap)
    "My original keymap binded to C-t.")
  (defalias 'my:ctrl-t-prefix my:ctrl-t-map)
  (define-key global-map (kbd "C-t") 'my:ctrl-t-prefix)
  (define-key my:ctrl-t-map (kbd "c")   'tab-new)
  (define-key my:ctrl-t-map (kbd "C-c") 'tab-new)
  (define-key my:ctrl-t-map (kbd "k")   'tab-close)
  (define-key my:ctrl-t-map (kbd "C-k") 'tab-close)
  (define-key my:ctrl-t-map (kbd "n")   'tab-next)
  (define-key my:ctrl-t-map (kbd "C-n") 'tab-next)
  (define-key my:ctrl-t-map (kbd "p")   'tab-previous)
  (define-key my:ctrl-t-map (kbd "C-p") 'tab-previous)
;;;###autoload
(defun my:tab-bar-tab-name-truncated ()
  "Custom: Generate tab name from the buffer of the selected window."
  (let ((tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
        (ellipsis (cond
                   (tab-bar-tab-name-ellipsis)
                   ((char-displayable-p ?…) "…")
                   ("..."))))
    (if (< (length tab-name) tab-bar-tab-name-truncated-max) ;; >
        (format "%-12s" tab-name)
      (propertize (truncate-string-to-width
                   tab-name tab-bar-tab-name-truncated-max nil nil
                   ellipsis)
                  'help-echo tab-name))))
  :custom
  ((tab-bar-close-button-show      . nil)
   (tab-bar-close-last-tab-choice  . nil)
   (tab-bar-close-tab-select       . 'left)
   (tab-bar-history-mode           . nil)
   (tab-bar-new-tab-choice         . "*scratch*")
   (tab-bar-new-button-show        . nil)
   (tab-bar-tab-name-function      . 'my:tab-bar-tab-name-truncated)
   (tab-bar-tab-name-truncated-max . 12)
   (tab-bar-separator              . "|")
   )
  :hook
  (emacs-startup-hook . tab-bar-mode)
  )

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
   (dired-listing-switches . "-alh")
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
                                 "^/home/zmm76933/.emacs.d/tmp/\\.*"
                                 "^/home/zmm76933/.dotfiles/Emacs/tmp/\\.*"
                                 "^/Users/zmm76933/.emacs.d/tmp/\\.*"
                                 "^/Users/zmm76933/.dotfiles/Emacs/tmp/\\.*"
                                 "^/[^/:]+:"
                                 "bookmarks"
                                 "\\.*COMMIT_EDITMSG$"
                                 ".*-autoloads.el$"
                                 "^/home/zmm76933/.emacs.d/pkg/\\.*"
                                 "^/home/zmm76933/.cache/\\.*"
                                 "^/Users/zmm76933/.emacs.d/pkg/\\.*"
                                 "^/Users/zmm76933/.cache/\\.*"
                                 )))
  )

(leaf calendar
  :custom
  (;; 月と曜日の表示調整
   (calendar-month-name-array . ["01" "02" "03" "04" "05" "06"
                                 "07" "08" "09" "10" "11" "12" ])
   (calendar-day-name-array   . ["日" "月" "火" "水" "木" "金" "土"])
   (calendar-day-header-array . ["日" "月" "火" "水" "木" "金" "土"])
   ;; 日曜開始
   (calendar-week-start-day   . 0)
   ;; 祝日をカレンダーに表示
   (calendar-mark-holidays-flag . t)
   )
  :config
  (leaf japanese-holidays
    :ensure t
    :require t
    :after calendar
    :custom
    ((japanese-holiday-weekend         . '(0 6))
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
    (setq calendar-holidays ; 他の国の祝日も表示させたい場合は適当に調整
          (append japanese-holidays holiday-local-holidays))
    ;;
    (defun my:japanese-holiday-show (&rest _args)
      (let* ((date (calendar-cursor-to-date t))
             (calendar-date-display-form '((format "%s年 %s月 %s日（%s）" year month day dayname)))
             (date-string (calendar-date-string date))
             (holiday-list (calendar-check-holidays date)))
        (when holiday-list
          (message "%s: %s" date-string (mapconcat #'identity holiday-list "; ")))))
    ;;
    :hook
    ((calendar-move-hook . my:japanese-holiday-show)
     (calendar-today-visible-hook . japanese-holiday-mark-weekend)
     (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
     (calendar-today-visible-hook . calendar-mark-today))
    )
  )

(leaf key-settings
  :config
  (keyboard-translate ?\C-h ?\C-?)
  (leaf-keys (("C-z"       . scroll-down)
              ("C-M-z"     . scroll-other-window-down)
              ("C-/"       . undo)
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

(defvar skk-user-directory (concat my:d:tmp "skk"))
(unless (file-directory-p skk-user-directory)
  (make-directory skk-user-directory))
(unless (locate-library "skk")
  (package-install 'ddskk t))
(leaf skk
  :commands skk-make-indicator-alist
  :bind (("C-\\"    . skk-mode))
  :init
  (setq skk-init-file (concat user-emacs-directory "init-ddskk")
        default-input-method "japanese-skk" )
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
     ("\d" . vertico-directory-delete-char))
    )
  )

(leaf embark
  :ensure t
  :bind
  (("C-h E" . embark-act)
   ("C-h B" . embark-bindings))
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
  :bind* (("C-x b" . consult-buffer)
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
   ("C-e" . corfu-quit))

  :custom
  `((completion-cycle-threshold . 4)
    (tab-always-indent        . 'complete)
    (corfu-cycle              . t)    ;; Enable cycling for `corfu-next/previous'
    (corfu-auto               . t)    ;; Enable auto completion
    (corfu-preselect-first    . nil)  ;; Disable candidate preselection
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

(leaf cape :ensure t)
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
        evil-undo-system 'undo-redo
        evil-want-fine-undo t
        evil-move-cursor-back t
        evil-show-paren-range 1
        evil-echo-state nil
        evil-respect-visual-line-mode t
        evil-disable-insert-state-bindings t
        evil-want-abbrev-expand-on-insert-exit nil)
  :hook
  (emacs-startup-hook . evil-mode)
  :bind
  (([escape] . evil-normal-state))
  :config
  (setcdr evil-insert-state-map nil)
  (leaf evil-collection
    :ensure t
    :after evil
    :config
    (evil-collection-init '(ediff
                            calendar
                            info
                            ibuffer
                            magit
                            dired)))
  (leaf evil-surround
    :ensure t
    :after evil
    :config
    (global-evil-surround-mode))
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
  :blackout `((rainbow-mode . ,(format " %s" "\x1F308")))
  )

(leaf project
  :ensure t
  :custom
  `((project-list-file . ,(expand-file-name "projects" my:d:tmp))
    (project-switch-use-entire-map . t)
    )
  )

(leaf consult-ghq
  :el-get uwabami/consult-ghq
  :bind (("C-x f" . consult-ghq-open))
  :custom
  `((consult-ghq-short-list . t))
  )

(leaf magit
  :bind (("C-x g" . magit-status))
  :ensure t
  :init
  (leaf transient
    :custom
    `((transient-history-file
       . ,(expand-file-name "transient-history.el" my:d:tmp))
      (transient-levels-file
       . ,(expand-file-name "transient-levels.el" my:d:tmp))
      (transient-values-file
       . ,(expand-file-name "transient-values.el" my:d:tmp))
      (transient-force-fixed-pitch . t))
    )
  :custom
  `((magit-completing-read-function . 'magit-builtin-completing-read)
    (magit-refs-show-commit-count   . 'all)
    (magit-log-buffer-file-locked   . t)
    (magit-revision-show-gravatars  . nil)
    )
  )

(leaf git-gutter-fringe
  :ensure t
  :hook
  (emacs-startup-hook . global-git-gutter-mode)
  )

(leaf lua-mode :ensure t)
(leaf ssh-config-mode :ensure t)
;; (leaf *misc-mode
;;   :init
;;   (leaf systemd :ensure t)
;;   (leaf debian-el
;;     :custom
;;     `((debian-bug-download-directory . "~/Downloads"))
;;     )
;;   (leaf rd-mode
;;     :mode "\\.rd$"
;;     :hook
;;     (rd-mode-hook . rd-show-other-block-all))
;;   (leaf yaml-mode
;;     :ensure t
;;     :mode "\\(\.yml\\|\.yaml\\)"
;;     )
;;   (leaf generic-x)
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

;;;###autoload
(defun my:load-window-config ()
  "load window-system specific settings"
  (interactive)
  (progn
    (set-face-attribute 'default nil
                        :family "monaco"
                        :height 120)
    (set-face-attribute 'fixed-pitch nil
                        :family "monaco"
                        :height 120)
    (set-face-attribute 'variable-pitch nil
                        :family "monaco"
                        :height 120)
    ;; Japanese
    (set-fontset-font nil
                      'japanese-jisx0213.2004-1
                      (font-spec :family "Hiragino Maru Gothic Pro" :height 120))
    (set-fontset-font nil
                      'japanese-jisx0213-2
                      (font-spec :family "Hiragino Maru Gothic Pro" :height 120))
    (set-fontset-font nil
                      'katakana-jisx0201
                      (font-spec :family "Hiragino Maru Gothic Pro" :height 120))
    ;; Latin with pronounciation annotations
    (set-fontset-font nil
                      '(#x0080 . #x024F)
                      (font-spec :family "AGVUDP" :height 120))
    ;; Math symbols
    (set-fontset-font nil
                      '(#x2200 . #x22FF)
                      (font-spec :family "AGVUDP" :height 120))
    ;; Greek
    (set-fontset-font nil
                      '(#x0370 . #x03FF)
                      (font-spec :family "AGVUDP" :height 120))
    ;; Some Icons
    (set-fontset-font nil
                      '(#xE0A0 . #xEEE0)
                      (font-spec :family "AGVUDP" :height 120))
    ))

;;;###autoload
(defun my:load-side-window-config ()
  "load window-system specific settings"
  (interactive)
  (progn
    (set-face-attribute 'default nil
                        :family "AGVUDP"
                        :height 180)
    (set-face-attribute 'fixed-pitch nil
                        :family "AGVUDP"
                        :height 180)
    (set-face-attribute 'variable-pitch nil
                        :family "AGVUDP"
                        :height 180)
    ;; Japanese
    (set-fontset-font nil
                      'japanese-jisx0213.2004-1
                      (font-spec :family "AGVUDP" :height 180))
    (set-fontset-font nil
                      'japanese-jisx0213-2
                      (font-spec :family "AGVUDP" :height 180))
    (set-fontset-font nil
                      'katakana-jisx0201
                      (font-spec :family "AGVUDP" :height 180))
    ;; Latin with pronounciation annotations
    (set-fontset-font nil
                      '(#x0080 . #x024F)
                      (font-spec :family "AGVUDP" :height 180))
    ;; Math symbols
    (set-fontset-font nil
                      '(#x2200 . #x22FF)
                      (font-spec :family "AGVUDP" :height 180))
    ;; Greek
    (set-fontset-font nil
                      '(#x0370 . #x03FF)
                      (font-spec :family "AGVUDP" :height 180))
    ;; Some Icons
    (set-fontset-font nil
                      '(#xE0A0 . #xEEE0)
                      (font-spec :family "AGVUDP" :height 180))
    ))
(leaf *gui
  :if window-system
  :config
  (cond
   ;; 4K display
   ((= (display-pixel-height) 2160)
    (setq default-frame-alist
          (append (list
                   '(width  . 200)
                   '(height . 80)
                   '(top    . 22)
                   '(left   . 0)
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
  (set-frame-parameter nil 'alpha 90)
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
  :defvar
  (skk-indicator-alist
   skk-hiragana-mode-string
   skk-katakana-mode-string
   skk-latin-mode-string
   skk-jisx0208-latin-mode-string
   skk-jisx0201-mode-string
   skk-abbrev-mode-string
   )
  :init
  (defun my:major-mode-icon (mode)
    "Update file icon in mode-line, just display major-mode icon. not filename."
    (let* ((icon (all-the-icons-icon-for-mode mode)))
      (if (symbolp icon)
          (all-the-icons-faicon "file-code-o"
                                :face 'all-the-icons-dsilver
                                :height 1.0)
        icon)))
  ;;
  (defun my:skk-init-modeline-input-mode ()
    "Custom skkが読み込まれていなくても skk-modeline-input-mode に値を設定"
    (cond
     ((not (boundp 'skk-modeline-input-mode))
      (setq skk-modeline-input-mode "DDSKK"))
     (t skk-modeline-input-mode)))
  ;;
  (defun my:skk-modeline-input-mode ()
    "Custom: powerline 用に skk の indicator を準備"
    (cond
     ((string-match "DDSKK" skk-modeline-input-mode) "[--]")
     ((string-match skk-hiragana-mode-string skk-modeline-input-mode) "[あ]")
     ((string-match skk-katakana-mode-string skk-modeline-input-mode) "[ア]")
     ((string-match skk-latin-mode-string skk-modeline-input-mode)    "[_A]")
     ((string-match skk-jisx0208-latin-mode-string skk-modeline-input-mode) "[Ａ]")
     ((string-match skk-jisx0201-mode-string skk-modeline-input-mode) "[_ｱ]")
     ((string-match skk-abbrev-mode-string skk-modeline-input-mode)   "[aA]")
     (t "[--]")
     )
    )
  ;;
  (defun my:skk-setup-modeline ()
    "skk-setup-modeline による modeline の更新を無効化"
    (setq skk-indicator-alist (skk-make-indicator-alist))
    (force-mode-line-update t))
  ;;
  :advice (:override skk-setup-modeline my:skk-setup-modeline)
  :custom
  `((powerline-buffer-size-suffix    . nil)
    (powerline-display-hud           . nil)
    (powerline-display-buffer-size   . nil)
    (powerline-text-scale-factor     .  1)
    (powerline-default-separator     . 'utf-8)
    (powerline-utf-8-separator-left  . #xe0b0)
    (powerline-utf-8-separator-right . #xe0b2)
    )
  :hook (emacs-startup-hook . my:powerline-theme)
  :config
;;;###autoload
  (defun my:powerline-theme ()
    "Setup the default mode-line."
    (interactive)
    (my:skk-init-modeline-input-mode)
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
               (lhs (list (powerline-raw (format "%s" (my:skk-modeline-input-mode)) mode-line 'l)
                          (powerline-raw "%*" mode-line 'l)
                          (powerline-raw mode-line-mule-info mode-line 'l)
                          (powerline-raw (my:major-mode-icon major-mode) mode-line 'l)
                          (powerline-buffer-id mode-line-buffer-id 'l)
                          (powerline-raw " ")
                          ;; (funcall separator-left face0 face1)
                          ))
               (rhs (list (powerline-raw global-mode-string face1 'r)
                          ;; (funcall separator-right face2 face1)
                          (powerline-vc face1 'r)
                          (powerline-raw mode-line-misc-info 'r)
                          (powerline-raw " ")
                          (powerline-raw "%6p" mode-line 'r)
                          )))
          (concat (powerline-render lhs)
                  (powerline-fill face2 (powerline-width rhs))
                  (powerline-render rhs))))))
    )
  ;; (my:powerline-theme)
  )

(defun my:debug-on-quit-if-scratch (&rest _args)
  (setq debug-on-quit (string= (buffer-name) "*scratch*")))
(add-hook 'window-selection-change-functions 'my:debug-on-quit-if-scratch)
(leaf which-key
  :ensure t
  :custom
  `((which-key-show-early-on-C-h    . t)
    (which-key-idle-delay           . 10000)
    (which-key-idle-secondary-delay . 0.05)
    (which-key-popup-type           . 'minibuffer)
    )
  :hook (emacs-startup-hook . which-key-mode)
  )
;; (leaf tree-sitter :ensure t)
;; (leaf tree-sitter-langs :ensure t)
(leaf rg
  :if (executable-find "rg")
  :ensure t
  )
;;(leaf emacs
;;  :preface
;;  (defun my-advice/window-width (fn &rest args)
;;    (- (apply fn args) 1))
;;  :advice (:around window-width my-advice/window-width))
;;
;; (leaf elfeed
;;   :if (file-directory-p my:d:password-store)
;;   :ensure t
;;   :custom
;;   `((elfeed-set-timeout  . 36000)
;;     (elfeed-db-directory . "~/.cache/elfeed"))
;;   :config
;;   (leaf elfeed-goodies
;;     :ensure t
;;     :config
;;     (elfeed-goodies/setup))
;;   ;;
;;   (leaf elfeed-protocol
;;     :ensure t
;;     :config
;;     (setq elfeed-feeds
;;           '(("owncloud+https://uwabami@uwabami.junkhub.org/nextcloud"
;;              :password (password-store-get "Web/uwabami.junkhub.org/nextcloud")
;;              )
;;             ))
;;     (elfeed-protocol-enable)
;;     )
;;   )
;;
(leaf vterm
  :ensure t
  :hook
  (vterm-mode-hook
   . (lambda () (setq-local global-hl-line-mode nil)))
  )
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
  :hook
  (emacs-startup-hook . popper-mode)
  (emacs-startup-hook . popper-echo-mode)
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
