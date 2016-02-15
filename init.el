;; 设置个人信息
(setq user-full-name "MingxunBai")
(setq user-mail-address "MingxunBai@outlook.com")

;; 关闭启动动画
(setq inhibit-startup-message t)

;; 显示列号
(setq column-number-mode t)
(setq line-number-mode t)

;; 语法高亮
(global-font-lock-mode t)

;; 用 y/n 代替 yes／no
(fset 'yse-or-no-p 'y-or-n-p)

;; 支持和外部程序的拷贝
(setq x-select-enable-clipboard t)

;; 匹配括号高亮
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; 直接打开／显示图片
(auto-image-file-mode t)

;; 设置回车后自动换行
(global-set-key (kbd "RET") 'newline-and-indent)

;; 去掉滚动条
(set-scroll-bar-mode nil)

;; 设置 tab
(setq c-indent-level 4)
(setq c-basic-offset 4)

;; set Emmet
(add-to-list 'load-path "~/.emacs.d/plugins/emmet-mode")
(require 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ; indent 2 space

(add-hook 'css-mode-hook 'emmet-mode) ; enable Emmet's css abbreviation
(global-set-key (kbd "C-x C-_") 'emmet-mode)

;; set auto-complete
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-quick-help nil)
(setq ac-auto-start 2)
(setq ac-auto-show-menu 0.2)
(setq ac-use-menu-map t) ; set menu hotkey
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
