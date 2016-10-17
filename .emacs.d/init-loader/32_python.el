;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; python.el --- python setting file
;;
;; $Id$

;;; Commentary:
;;
;; python-setting

;;; Code:
;;
;;
(when (memq system-type '(gnu/linux))
  (custom-set-variables
   '(python-shell-interpreter "ipython")
   '(python-shell-interpreter-args "--simple-prompt")
   '(python-shell-completion-native-disabled-interpreters '("pypy" "ipython"))
   '(python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
   '(python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
   '(python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")
   '(python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n")
   '(python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(defun my/python-mode-hook ()
  (jedi:setup)
  (setq-local company-backends '(company-jedi company-dabbrev))

  ;; flycheck
  (setq flycheck-flake8rc (expand-file-name "~/.config/flake8")))

(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'my/python-mode-hook)

  ;; binding
  (define-key python-mode-map (kbd "C-M-i") 'company-complete)
  (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)
  (define-key python-mode-map (kbd "C-c C-h") 'jedi:show-doc)
  (define-key python-mode-map (kbd "C-c C-l") 'jedi:get-in-function-call)

  (smartrep-define-key
      python-mode-map "C-c" '(("h" . 'python-indent-shift-left)
                              ("l" . 'python-indent-shift-right))))

;; jedi
(custom-set-variables
 '(jedi:tooltip-method nil))

;;; python.el ends here
