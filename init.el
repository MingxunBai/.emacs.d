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

;; 设置默认模式为 text
(setq default-major-mode 'text-mode)

;; 匹配括号高亮
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; 直接打开／显示图片
(auto-image-file-mode t)

;; 设置回车后自动换行
(global-set-key (kbd "RET") 'newline-and-indent)

;; 设置 tab
(defun my-setup-indent (n)

  ;; java/c/c++
  (setq c-basic-offset n)

  ;; web development
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, inlatest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
)

(defun my-office-code-style ()
  (interactive)
  (message "Office code style!")
  (setq indent-tabs-mode t) ; use tab instead of space
  (my-setup-indent 4) ; indnet 4 space width
)

(defun my-personal-code-style ()
  (interactive)
  (message "My personal code style!")
  (setq indent-tabs-mode nil) ; use space instead of tab
  (my-setup-indnet 2) ; indent 2 space width
)

(defun my-setup-develop-environmnet ()
  (interactive)
  (let ((proj-dir (file-name-directory (buffer-file-name))))
    ;; if hobby project path contains string "hobby-proj1"
    (if (string-match-p "hobby-proj1" proj-dir)
	(my-personal-code-style))

    ;; if commericial project path contains string "commerical-proj"
    (if (string-match-p "commerical-proj" proj-dir)
	(my-office-code-style))
  )
)

;; prog-mode-hock requires emacs24+
(add-hook 'prog-mode-hock 'my-setup-develop-environment)
