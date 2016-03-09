;; ------ 设置个人信息 ------
(setq user-full-name "MingxunBai")
(setq user-mail-address "MingxunBai@outlook.com")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-site-lisp)
(require 'init-elpa)

(setq inhibit-startup-message t) ; 关闭启动动画

(setq kill-ring-max 500) ; 设置历史记录数量

(global-font-lock-mode t) ; 语法高亮

(setq x-select-enable-clipboard t) ; 支持和外部程序的拷贝

(setq make-backup-files nil) ; 不生成备份文件

(set-scroll-bar-mode nil)
(tool-bar-mode -1)

(require 'init-emmet-mode)
(require 'init-auto-complete)
(require-package 'diminish)
(require 'init-editing-utils)
(require 'init-css)
(require 'init-javascript)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
