;; ------ 设置个人信息 ------
(setq user-full-name "MingxunBai")
(setq user-mail-address "MingxunBai@outlook.com")

(setq inhibit-startup-message t) ; 关闭启动动画

(setq kill-ring-max 500) ; 设置历史记录数量


(global-font-lock-mode t) ; 语法高亮

(setq x-select-enable-clipboard t) ; 支持和外部程序的拷贝

(set-scroll-bar-mode nil)
(tool-bar-mode nil)

;; set Emmet
(require-package 'emmet-mode)
(require 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ; indent 2 space
(add-hook 'css-mode-hook 'emmet-mode) ; enable Emmet's css abbreviation
(global-set-key (kbd "\C-x \C-_") 'emmet-mode)

;; set auto-complete
(require-package 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq tab-always-indent 'complete) ; indent first, then complete
(setq ac-auto-start 1)
; (setq-default ac-expand-on-auto-complete nil) ; stop auto-start
(ac-set-trigger-key "TAB") ; use tab to complete
(setq ac-auto-show-menu 0.2)
(setq ac-use-menu-map t) ; set menu hotkey
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
