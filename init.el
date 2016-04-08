;; ------ 设置个人信息 ------
(setq user-full-name "MingxunBai")
(setq user-mail-address "MingxunBai@outlook.com")

(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))

(setq inhibit-startup-message t) ; 关闭启动动画

(setq kill-ring-max 500) ; 设置历史记录数量

(setq inhibit-startup-message t) ; 关闭出错提示音

(setq frame-title-format "emacs@%b") ; 标题显示 Buffer 名

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

;; Ido-mode
(ido-mode t)
(setq ido-save-directory-list-file nil)

(show-paren-mode t)
(setq show-paren-style 'parenthesis) ; 括号匹配时可以高亮显示另外一边的括号，但光标不会跳到另一个括号处

(setq-default kill-whole-line t) ; 在行首 C-k 时，同时删除该行

(setq track-eol t) ; 当光标在行尾上下移动的时候，始终保持在行尾

(setq scroll-margin 3 scroll-conservatively 10000) ; 防止页面滚动时跳动，scroll-margin 3 可以在靠近屏幕边沿3行时就开始滚动，可以很好的看到上下文

(fset 'yes-or-no-p 'y-or-n-p) ; 使用 y/n 替代 yes/no

(setq x-select-enable-clipboard t) ; 支持和外部程序的拷贝

(setq make-backup-files nil) ; 不生成备份文件
(setq auto-save-default nil) ; 不生成临时文件

(set-scroll-bar-mode nil) ; 隐藏滚动条
(tool-bar-mode -1) ; 隐藏工具栏

(global-set-key (kbd"RET") 'newline-and-indent) ; 回车时缩进
(setq-default indent-tabs-mode  nil) ; 设置缩进为空格

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
(add-hook 'web-mode-hook (lambda ()
                           (emmet-mode t)))
(add-hook 'html-mode-hook (lambda ()
                            (emmet-mode t)))
(add-hook 'css-mode-hook (lambda ()
			   (emmet-mode t)))

;; org-mode 自动换行
(add-hook 'org-mode-hook
          (lambda ()
            (setq truncate-lines nil)
            (set-fill-column 70)))

;; Web-mode
(require 'web-mode)
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; highlight-parentheses-mode
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t))) 
(global-highlight-parentheses-mode)

;; YASnippet
(require 'yasnippet)
(yas-global-mode 1)
;; Completing point by some yasnippet key
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
  (let ((original-point (point)))
    (while (and
            (not (= (point) (point-min) ))
            (not
             (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-ido-expand)

;; 自加载对应模式
(setq auto-mode-alist
      (append '(("\\.html?\\'" . web-mode)
                ("\\.js\\'" . web-mode)
                ("\\.php\\'" . web-mode)
                ("\\.phtml\\'" . web-mode)
                ("\\.djhtml\\'" . web-mode)
                ("\\.[agj]sp\\'" . web-mode)
                ("\\.as[cp]x\\'" . web-mode)
                ("\\.org\\'" . org-mode)
		("\\.md\\'" . org-mode)
		("\\.txt\\'" . org-mode)
		("\\.py\\'" . python-mode))
	      auto-mode-alist))

;; 显示时间设置
(display-time-mode 1) ; 启用时间显示设置
(setq display-time-24hr-format t) ; 时间使用24小时制
