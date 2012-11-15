(setq inhibit-startup-screen t)
(setq initial-scratch-message "")


(add-to-list 'custom-theme-load-path "~/emacs/themes")
(load-theme 'zenburn t)


(setq x-select-enable-clipboard t)
(setq make-backup-files nil)
(display-time)
(global-font-lock-mode t)
(setq require-final-newline 't)
(defalias 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode -1)
(column-number-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c C-u c") 'uncomment-region)


;; ------------------------------------------------------------------------------------------------

(add-to-list 'load-path "~/emacs/py")
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

;; http://stackoverflow.com/questions/1259873/how-can-i-use-emacs-flymake-mode-for-python-with-pyflakes-and-pylint-checking-co
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "~/emacs/py/pycheckers"  (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'python-mode-hook 'flymake-mode)


;; ------------------------------------------------------------------------------------------------

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq auto-mode-alist (cons '("\.tex$" . LaTeX-mode) auto-mode-alist))

(setq TeX-PDF-mode t)


;; ------------------------------------------------------------------------------------------------

(add-to-list 'load-path "~/emacs/js")

(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

(require 'flymake-node-jshint)
(add-hook 'js-mode-hook (lambda () (flymake-mode 1)))


;; ------------------------------------------------------------------------------------------------

(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)


;; ------------------------------------------------------------------------------------------------

(load "haskell-mode.el" nil t t)
(load-library "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)


;; ------------------------------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("78b1c94c1298bbe80ae7f49286e720be25665dca4b89aea16c60dacccfbb0bca" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
