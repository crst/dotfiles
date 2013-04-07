(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq make-backup-files nil)
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)
(put 'narrow-to-region 'disabled nil)

(column-number-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t)
(display-time)
(global-font-lock-mode t)
(setq default-font "Inconsolata")
(setq require-final-newline 't)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(set-default 'truncate-lines +1)
(global-hl-line-mode +1)


;; ------------------------------------------------------------------------------------------------

(add-hook 'post-command-hook (lambda () (recenter)))


;; http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(global-set-key [(shift return)] (lambda () (interactive) (move-end-of-line nil) (newline-and-indent)))


;; http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))


;; http://emacs-fu.blogspot.de/2013/03/editing-with-root-privileges-once-more.html
(defun find-file-as-root ()
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))


(require 'ido)
(setq ido-enable-flex-matching +1)
(setq ido-everywhere t)
(ido-mode +1)


(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)


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


;; ------------------------------------------------------------------------------------------------

(install-if-not-installed 'move-text)
(global-set-key [(control shift n)] 'move-text-down)
(global-set-key [(control shift p)] 'move-text-up)


(install-if-not-installed 'iedit)
(require 'iedit)
(global-set-key (kbd "C-;") 'iedit-mode)


(install-if-not-installed 'browse-kill-ring)
(browse-kill-ring-default-keybindings)


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
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)


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

(install-if-not-installed 'php-mode)
(add-to-list 'auto-mode-alist
             '("\\.php[345]?\\'\\|\\.phtml\\'" . php-mode))

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


;; http://sachachua.com/blog/2008/07/emacs-and-php-on-the-fly-syntax-checking-with-flymake/
(require 'flymake)
(defun flymake-php-init ()
  (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	 (local (file-relative-name temp (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local "-l"))))

(add-to-list 'flymake-err-line-patterns
  '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))

(add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))

(add-hook 'php-mode-hook (lambda () (flymake-mode +1)))
