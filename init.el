;;-------------------------------------------------
;; Basic setting
;;-------------------------------------------------

;; 设置个人信息
(setq user-full-name "MingxunBai"
      user-mail-address "mingxunbai@outlook.com")

;; 定义常量
(defconst *WINDOWS* (eq system-type 'windows-nt))
(defconst *PATH* (expand-file-name "plugins" user-emacs-directory))

;; 路径配置
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path *PATH*)

(setq default-directory
      (if *WINDOWS*
          (format "C:/Users/%s/Documents" user-full-name)
        "~/Documents"))

;; 配置五笔输入法
(require 'chinese-wbim-extra)

(autoload 'chinese-wbim-use-package "chinese-wbim" "Another emacs input method")
(register-input-method "chinese-wbim" "euc-cn" 'chinese-wbim-use-package "五笔" "汉字五笔输入法" "wb.txt")
(setq chinese-wbim-use-tooltip nil)

;; 启动五笔输入法
(set-input-method 'chinese-wbim)
(toggle-input-method)

;;-------------------------------------------------
;; 编码环境
;;-------------------------------------------------

(setq current-language-environment "utf-8"
      default-buffer-file-coding-system 'utf-8
      locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "\\.[bt][ax]t\\'" 'chinese-iso-8bit)

;; use gbk for cmd
(when *WINDOWS*
  (set-default 'process-coding-system-alist
               '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
                 ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos))))

;;-------------------------------------------------
;; 显示 & 行为
;;-------------------------------------------------

(defalias 'alr 'align-regexp)           ; 设置对齐别名

(setq-default indent-tabs-mode nil      ; 使用空格缩进
              cursor-type 'box)         ; 设置光标样式

(setq inhibit-startup-message t         ; 关闭启动动画
      initial-scratch-message nil       ; 移除草稿文本

      visible-bell t                    ;;
      ring-bell-function 'ignore        ; 关闭错误提示音
      save-abbrevs nil                  ;;

      mode-require-final-newline nil    ; 禁止在文件尾创建新行

      default-major-mode 'text-mode     ; 设置默认主模式为 text-mode

      split-height-threshold nil        ;;
      split-width-threshold 0           ; 垂直分屏

      scroll-margin 3                   ;;
      scroll-conservatively 10000       ; 靠近屏幕边沿3行时就开始滚动

      font-lock-maximum-decoration t    ;;
      font-lock-verbose t               ; 渲染当前 buffer 语法高亮
      font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000))

      kill-ring-max 500                 ; 设置历史记录数量

      inhibit-startup-message t         ; 关闭出错提示音

      kill-whole-line t                 ; 在行首 C-k 时，同时删除该行

      track-eol t                       ; 换行时，光标始终保持在行首尾

      x-select-enable-clipboard t       ; 支持和外部程序的拷贝

      auto-save-default nil             ; 不生成临时文件
      make-backup-files nil             ; 不生成备份文件

      c-basic-offset 4                  ; C 语言缩进为 4
      default-tab-width 4               ; 默认缩进为 4

      max-lisp-eval-depth 10000         ; 设置函数嵌套深度

      linum-format 'my-linum-format     ;;
      column-number-mode                ; 显示行号列号
      line-number-mode                  ;;

      show-paren-style 'parenthesis     ; 光标不会跳到另一个括号处

      display-time-day-and-date t       ;;
      display-time-24hr-format t        ; 显示时间日期
      display-time-default-load-average nil

      frame-title-format                ; Title 显示完整路径
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

      eshell-prompt-function            ; Eshell 提示符
      (lambda ()
        (concat
         (propertize (format-time-string "[%Y-%m-%d %H:%M] " (current-time)) 'face `(:background "#FFFFFF" :foreground "Blue"))
         (propertize (eshell/pwd) 'face `(:background "#FFFFFF" :foreground "#888"))
         (if (= (user-uid) 0) " # " " $ "))))

(fset 'yes-or-no-p 'y-or-n-p)           ; 使用 y/n 替代 yes/no

;; 设置字体
;; (set-default-font "Source Code Pro-12")
(when *WINDOWS*                         ; 设置中文字体为 "明兰黑"
  (set-fontset-font t 'han (font-spec :family "Minglan_Code")))

;; 自动匹配
(setq skeleton-pair-alist
      '((?\" _ "\"" >)
        (?\' _ "\'" >)
        (?\( _ ")"  >)
        (?\[ _ "]"  >)
        (?\{ _ "}"  >)
        (?\< _ ">"  >))
      skeleton-pair t)

;;-------------------------------------------------
;; Internal mode
;;-------------------------------------------------

(global-auto-revert-mode)               ; Auto revert

(display-time)                          ; 显示时间

(scroll-bar-mode -1)                    ; 隐藏滚动条
(tool-bar-mode -1)                      ; 隐藏工具栏
(menu-bar-mode -1)                      ; 隐藏菜单栏

(show-paren-mode)                       ; 高亮匹配括号

(global-font-lock-mode)                 ; 语法高亮

(global-linum-mode)                     ; 显示行号

;; 格式化并高亮行号
(require 'hl-line)
(global-hl-line-mode)

(defface my-linum-hl
  `((t :inherit linum :background "#E8E8FF" :foreground "#000000",(face-background 'hl-line nil t)))
  "Face for the current line number."
  :group 'linum)

(defvar my-linum-current-line-number 0)

(defun my-linum-format (line-number)
  (propertize (format "| %3d " line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'my-linum-hl
                'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))

(ad-activate 'linum-update)

;; Highlight line mode
(set-face-attribute hl-line-face nil
                    ;; :underline t
                    :background "#E8E8FF")

;; Ido mode
(ido-mode)

(setq ido-save-directory-list-file nil
      ido-enable-flex-matching t)       ; 模糊匹配

;; Winner mode
(when (fboundp 'winner-mode)
  (winner-mode))

;;-------------------------------------------------
;; Extension
;;-------------------------------------------------

;; Auto complete mode
(require 'auto-complete-config)
(ac-config-default)

(setq ac-auto-start nil
      ac-use-menu-map t)
(define-key ac-mode-map "\M-/" 'auto-complete)
(define-key ac-complete-mode-map "\M-/" 'ac-stop)

;; Batch mode
(require 'batch-mode)

;; Emmet mode
(defun enable-emmet-mode ()
  (interactive)
  (require 'emmet-mode)
  (emmet-mode))

;; Git
(defun my/git-status ()
  (interactive)
  (require 'magit)
  (magit-status))

;; Highlight indent guides
(require 'highlight-indent-guides)

(setq highlight-indent-guides-method 'character)

;; Highlight parentheses mode
(require 'highlight-parentheses)
(global-highlight-parentheses-mode)

;; History
(require 'history)

;; JavaScript IDE mode
(defun enable-js2-mode ()
  (interactive)
  (require 'js2-mode)
  (js2-mode)

  (require 'js-comint)
  (setq inferior-js-program-command "node")
  (setq inferior-js-program-arguments '("--interactive"))

  (require 'js2-highlight-vars)
  (js2-highlight-vars-mode)

  (local-set-key (kbd "C-c f")   'js-load-file-and-go)
  (local-set-key (kbd "C-c b")   'js-send-buffer)
  (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go))

;; JSON mode
(defun enable-json-mode ()
  (interactive)
  (require 'json-mode)
  (json-mode))

;; Lazy set key
(require 'lazy-set-key)
(require 'lazy-key-bind)

;; Less css mode
(defun enable-less-css-mode ()
  (interactive)
  (require 'less-css-mode)
  (less-css-mode)

  (setq less-css-compile-at-save t
        less-css-output-directory "../css"))

;; Markdown mode
(defun enable-markdown-mode ()
  (interactive)
  (require 'markdown-mode)
  (markdown-mode))

;; Multiple cursors
(require 'multiple-cursors)
(lazy-unset-key '("M-<down-mouse-1>"))

;; Origami mode
(require 'origami)
(global-origami-mode)

;; Project explorer
(require 'project-explorer)

;; SCSS mode
(defun enable-scss-mode ()
  (require 'scss-mode)
  (scss-mode)

  (setq scss-compile-at-save t))

;; Smart mode line
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(smart-mode-line-enable)

;; Vimrc mode
(defun enable-vimrc-mode ()
  (interactive)
  (require 'vimrc-mode)
  (vimrc-mode))

;; Web mode
(defun enable-web-mode ()
  (interactive)
  (require 'web-mode)
  (web-mode)

  (setq web-mode-markup-indent-offset             2
        web-mode-css-indent-offset                4
        web-mode-enable-current-element-highlight t))

;; Windows numbering
(require 'window-numbering)
(window-numbering-mode)

;; Yaml mode
(defun enable-yaml-mode ()
  (interactive)
  (require 'yaml-mode)
  (yaml-mode))

;; YASnippet
(require 'yasnippet)
(yas-global-mode)

(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))

;; 自定义功能
(require 'custom-function)

;;-------------------------------------------------
;; 主模式
;;-------------------------------------------------

(setq auto-mode-alist
      (append '(("/[^\\./]*\\'" .   conf-mode) ; File name has no dot

                ("\\.bash"      .   sh-mode)
                ("\\.css\\'"    .   (lambda () (enable-web-mode)))
                ("\\.el\\'"     .   (lambda () (emacs-lisp-mode)
                                      (setq skeleton-pair-alist
                                            '((?\' "" >)))))
                ("\\.js\\'"     .   (lambda () (enable-js2-mode)))
                ("\\.json\\'"   .   (lambda () (enable-json-mode)))
                ("\\.less\\'"   .   (lambda () (enable-less-css-mode)))
                ("\\.md\\'"     .   (lambda () (enable-markdown-mode)))
                ("\\.php\\'"    .   (lambda () (enable-web-mode)))
                ("\\.s[ac]ss"   .   (lambda () (enable-scss-mode)))
                ("\\.vimrc\\'"  .   (lambda () (enable-vimrc-mode)))
                ("\\.ya?ml\\'"  .   (lambda () (enable-yaml-mode))))
              auto-mode-alist))

;;-------------------------------------------------
;; Hook
;;-------------------------------------------------

;; Eshell mode
(add-hook 'eshell-mode-hook 'custom-eshell-mode-hook)

;; Org mode
(add-hook 'org-mode-hook 'custom-org-mode-hook)

;; Python mode
(add-hook 'python-mode-hook 'custom-python-mode-hook)

;; Web mode
(add-hook 'html-mode-hook 'enable-web-mode)
(add-hook 'nxml-mode-hook 'enable-web-mode)

(add-hook 'css-mode-hook  'custom-web-mode-hook)
(add-hook 'js2-mode-hook  'custom-web-mode-hook)
(add-hook 'json-mode-hook 'custom-web-mode-hook)
(add-hook 'web-mode-hook  'custom-web-mode-hook)

;; 保存前删除多余空格
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 最大化
(custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))