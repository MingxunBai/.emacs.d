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

(set-scroll-bar-mode nil)
(tool-bar-mode -1)

(require 'init-emmet-mode)
(require 'init-auto-complete)
