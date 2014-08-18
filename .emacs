(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq initial-major-mode 'org-mode)
(setq auto-save-default nil)
(setq make-backup-files nil)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)
(put 'narrow-to-region 'disabled nil)

(column-number-mode t)
(cua-mode t)
(electric-pair-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(global-font-lock-mode t)
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 100)

(setq require-final-newline 't)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'prog-mode-hook 'subword-mode)
(set-default 'truncate-lines +1)
(global-hl-line-mode +1)

(setq gc-cons-threshold 20000000)

;; ------------------------------------------------------------------------------------------------

(add-hook 'post-command-hook 'recenter)

;; http://emacs-fu.blogspot.de/2013/03/editing-with-root-privileges-once-more.html
(defun find-file-as-root ()
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))


;; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c r")  'rename-file-and-buffer)


(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "/")


(require 'saveplace)
(setq-default save-place t)


;; ------------------------------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defun install-if-not-installed (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))


;; ------------------------------------------------------------------------------------------------

(install-if-not-installed 'zenburn-theme)
(require 'zenburn-theme)


;; ------------------------------------------------------------------------------------------------

(require 'ido)
(install-if-not-installed 'flx-ido)
(require 'flx-ido)

(setq ido-enable-flex-matching +1)
(setq ido-everywhere t)
(ido-mode +1)
(flx-ido-mode +1)
(setq ido-use-faces nil)


(install-if-not-installed 'flycheck)
(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

(set-face-attribute 'flycheck-error nil :foreground "#BC8383" :background "#8B0000" :underline t)
(set-face-attribute 'flycheck-warning nil :foreground "#DFAF8F" :background "#8B670B" :underline t)


(install-if-not-installed 'magit)
(setq magit-status-buffer-switch-function 'switch-to-buffer)
(global-set-key (kbd "C-x g") 'magit-status)

(install-if-not-installed 'git-timemachine)


(install-if-not-installed 'powerline)
(require 'powerline)
(powerline-default-theme)


(install-if-not-installed 'move-text)
(global-set-key [(control shift n)] 'move-text-down)
(global-set-key [(control shift p)] 'move-text-up)


(install-if-not-installed 'browse-kill-ring)
(browse-kill-ring-default-keybindings)


(install-if-not-installed 'projectile)
(projectile-global-mode)


(install-if-not-installed 'helm)
(helm-mode +1)
(install-if-not-installed 'helm-projectile)

(defun helm-grep-project ()
  (interactive)
  (let ((project-root (shell-command-to-string "global -p")))
    (when (not (string= (substring project-root 0 7) "global:"))
      (helm-do-grep-1 (list (car (split-string project-root "\n"))) '(4) nil '("*")))))

(global-set-key (kbd "C-c j") 'helm-grep-project)
(global-set-key (kbd "C-c h") 'helm-projectile)


;; ------------------------------------------------------------------------------------------------

(install-if-not-installed 'auctex)

(setq auto-mode-alist (cons '("\.tex$" . LaTeX-mode) auto-mode-alist))
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)


;; ------------------------------------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq js-indent-level 4)


;; ------------------------------------------------------------------------------------------------

(install-if-not-installed 'clojure-mode)
(install-if-not-installed 'paredit)
(install-if-not-installed 'cider)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq cider-popup-stacktraces nil)
(setq cider-show-error-buffer nil)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)


;; ------------------------------------------------------------------------------------------------

(install-if-not-installed 'haskell-mode)
(install-if-not-installed 'flycheck-hdevtools)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)


;; ------------------------------------------------------------------------------------------------

(install-if-not-installed 'markdown-mode)
(install-if-not-installed 'markdown-mode+)


;; ------------------------------------------------------------------------------------------------

(install-if-not-installed 'php-mode)

(add-to-list 'auto-mode-alist
             '("\\.php[345]?\\'\\|\\.phtml\\'" . php-mode))

(setq c-basic-offset 4)
(setq nxml-child-indent 4)

(install-if-not-installed 'ggtags)
(add-hook 'php-mode-hook 'ggtags-mode)


;; http://emacs-fu.blogspot.de/2009/01/navigating-through-source-code-using.html
(defun create-or-update-gtags ()
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p")))
      (let ((olddir default-directory)
            (rootdir (read-directory-name "gtags: source tree root:" default-directory)))
        (cd rootdir)
        (shell-command "gtags -q && echo 'created tagfile'")
        (cd olddir))
    (shell-command "global -q -u && echo 'updated tagfile'")))

(add-hook 'php-mode-hook (lambda () (add-hook 'after-save-hook 'create-or-update-gtags nil 'make-it-local)))
