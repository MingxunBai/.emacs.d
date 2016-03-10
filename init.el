;; ------ 设置个人信息 ------
(setq user-full-name "MingxunBai")
(setq user-mail-address "MingxunBai@outlook.com")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq inhibit-startup-message t) ; 关闭启动动画

(setq kill-ring-max 500) ; 设置历史记录数量

;; 语法高亮(除了 shell-mode 和 text-mode)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-global-modes '(not shell-mode text-mode))
(setq font-lock-verbose t)
(setq font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000)))

;; 显示行号
(global-linum-mode t)
(setq column-number-mode t)
(setq line-number-mode t)

(ido-mode t) ; C-x C-f 后有文件名提示
(setq ido-save-directory-list-file nil)

(show-paren-mode t)
(setq show-paren-style 'parenthesis) ; 括号匹配时可以高亮显示另外一边的括号，但光标不会跳到另一个括号处

(setq-default kill-whole-line t) ; 在行首 C-k 时，同时删除该行

(setq track-eol t) ; 当光标在行尾上下移动的时候，始终保持在行尾

(setq scroll-margin 3 scroll-conservatively 10000) ; 防止页面滚动时跳动，scroll-margin 3 可以在靠近屏幕边沿3行时就开始滚动，可以很好的看到上下文

(fset 'yes-or-no-p 'y-or-n-p) ; 使用 y/n 替代 yes/no

(setq x-select-enable-clipboard t) ; 支持和外部程序的拷贝

(setq make-backup-files nil) ; 不生成备份文件

(set-scroll-bar-mode nil) ; 隐藏滚动条
(tool-bar-mode -1) ; 隐藏工具栏

(global-set-key (kbd"RET") 'newline-and-indent) ; 回车时缩进

;; Auto-complete
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq tab-always-indent 'complete)
(setq-default ac-auto-start nil)
(setq-default ac-expand-on-auto-complete nil)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; Emmet-mode
(require 'emmet-mode)

;; Web-mode
(require 'web-mode)

;; highlight-parentheses-mode
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t))) 
(global-highlight-parentheses-mode)

;; 自加载对应模式
(setq auto-mode-alist
      (append '(("\\.css\\'" . css-mode)
		("\\.js\\'" . javascript-mode)
		("\\.phtml\\'" . web-mode)
		("\\.tpl\\.php\\'" . web-mode)
		("\\.[agj]sp\\'" . web-mode)
		("\\.as[cp]x\\'" . web-mode)
		("\\.erb\\'" . web-mode)
		("\\.mustache\\'" . web-mode)
		("\\.djhtml\\'" . web-mode)
		("\\.html?\\'" . web-mode)
		("\\.org\\'" . org-mode))
	      auto-mode-alist))
(add-hook 'web-mode-hook (lambda ()
			   (emmet-mode t)))
(add-hook 'css-mode-hook (lambda ()
			   (emmet-mode t)))

;;显示时间设置
(display-time-mode 1) ; 启用时间显示设置，在minibuffer上面的那个杠上
(setq display-time-24hr-format t) ; 时间使用24小时制
(setq display-time-day-and-date t) ; 时间显示包括日期和具体时间
