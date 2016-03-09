;; ------ 设置个人信息 ------
(setq user-full-name "MingxunBai")
(setq user-mail-address "MingxunBai@outlook.com")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq inhibit-startup-message t) ; 关闭启动动画

(setq kill-ring-max 500) ; 设置历史记录数量

(global-font-lock-mode t) ; 语法高亮

(setq x-select-enable-clipboard t) ; 支持和外部程序的拷贝

(setq make-backup-files nil) ; 不生成备份文件

(set-scroll-bar-mode nil)
(tool-bar-mode -1)

;; Auto-complete
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq tab-always-indent 'complete) ; indent first, then complete                                                                                                         
(setq-default ac-auto-start nil)
(setq-default ac-expand-on-auto-complete nil)
(ac-set-trigger-key "TAB") ; use tab to complete                                                                                                                         
(setq ac-use-menu-map t) ; set menu hotkey                                                                                                                               
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; Emmet-mode
(require 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ; indent 2 space                                                                                      
(add-hook 'css-mode-hook 'emmet-mode) ; enable Emmet's css abbreviation                                                                                                  
(global-set-key (kbd "\C-x \C-e") 'emmet-mode)

;; Web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
