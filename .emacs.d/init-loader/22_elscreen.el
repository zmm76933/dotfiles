;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-
;;
;; elscreen.el --- elscreen setting file
;;
;; $Id$

;;; Commentary:
;;
;;

;;; Code:
;;
;; @see http://www.morishima.net/~naoto/j/software/elscreen/
(elscreen-start)
(elscreen-set-prefix-key "\C-t")
(define-key elscreen-map (kbd "C-t") 'elscreen-toggle)
(define-key elscreen-map (kbd "k") 'elscreen-kill-screen-and-buffers)
(define-key elscreen-map (kbd ",") 'elscreen-screen-nickname)
(define-key elscreen-map (kbd "C") 'elscreen-editutil-clone-only-this-window)
(define-key elscreen-map (kbd "C-l") 'helm-editutil-elscreen)
(run-with-idle-timer 20 t 'elscreen-editutil-update-frame-title)

(custom-set-variables
 '(elscreen-display-screen-number nil)
 '(elscreen-tab-display-kill-screen nil)

 '(elscreen-mode-to-nickname-alist
   '(("^dired-mode$" . (lambda () (format "Dired(%s/)" (buffer-name))))
     ("^Info-mode$" . (lambda ()
                        (format "Info(%s)" (file-name-nondirectory Info-current-file))))))

 '(elscreen-buffer-to-nickname-alist
   '(("Minibuf". ""))))

;; Don't show tab number in mode-line
(setq-default elscreen-mode-line-string nil)
(remove-hook 'elscreen-screen-update-hook 'elscreen-mode-line-update)
(add-hook 'elscreen-screen-update-hook 'elscreen-editutil-update-frame-title)
(elscreen-toggle-display-tab)

;;; elscreen.el ends here
