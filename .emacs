(setq inhibit-startup-screen t)
(setq initial-scratch-message "")


(setq make-backup-files nil)
(blink-cursor-mode -1)
(column-number-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t)
(display-time)
(global-font-lock-mode t)
(setq require-final-newline 't)
(setq default-font "Inconsolata-12")

(global-set-key (kbd "C-n") (lambda () (interactive) (next-line) (recenter)))
(global-set-key (kbd "C-p") (lambda () (interactive) (previous-line) (recenter)))

(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)


;; ------------------------------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defun install-if-not-installed (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))


;; ------------------------------------------------------------------------------------------------

(install-if-not-installed 'zenburn-theme)
(load-theme 'zenburn t)
(install-if-not-installed 'solarized-theme)


;; ------------------------------------------------------------------------------------------------

(install-if-not-installed 'python-mode)

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

(install-if-not-installed 'auctex)

(setq auto-mode-alist (cons '("\.tex$" . LaTeX-mode) auto-mode-alist))
(setq TeX-PDF-mode t)


;; ------------------------------------------------------------------------------------------------

(add-to-list 'load-path "~/emacs/js")
(require 'flymake-node-jshint)
(add-hook 'js-mode-hook (lambda () (flymake-mode 1)))

(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)


;; ------------------------------------------------------------------------------------------------

(install-if-not-installed 'clojure-mode)
(install-if-not-installed 'paredit)

(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)


;; ------------------------------------------------------------------------------------------------

(install-if-not-installed 'haskell-mode)

(load-library "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)


;; ------------------------------------------------------------------------------------------------
