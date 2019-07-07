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
(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 96)
(setq require-final-newline 't)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(set-default 'truncate-lines +1)
(global-hl-line-mode +1)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'subword-mode)

(add-hook 'post-command-hook 'recenter)

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "/")


(require 'saveplace)
(setq-default save-place t)


(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.milkbox.net/packages/"))

(package-initialize)

(defun install-if-not-installed (package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(install-if-not-installed 'zenburn-theme)
(require 'zenburn-theme)


(install-if-not-installed 'flycheck)
(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

(set-face-attribute 'flycheck-error nil :foreground "#BC8383" :background "#8B0000" :underline t)
(set-face-attribute 'flycheck-warning nil :foreground "#DFAF8F" :background "#8B670B" :underline t)


(install-if-not-installed 'auctex)

(setq auto-mode-alist (cons '("\.tex$" . LaTeX-mode) auto-mode-alist))
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(install-if-not-installed 'magit)
(setq magit-status-buffer-switch-function 'switch-to-buffer)
(global-set-key (kbd "C-x g") 'magit-status)
;(setq magit-last-seen-setup-instructions "1.4.0")

(install-if-not-installed 'move-text)
(global-set-key [(control shift n)] 'move-text-down)
(global-set-key [(control shift p)] 'move-text-up)

(install-if-not-installed 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(install-if-not-installed 'markdown-mode)
(install-if-not-installed 'rust-mode)
