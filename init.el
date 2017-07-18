;;-------------------------------------------------
;; Basic setting
;;-------------------------------------------------

;; 设置个人信息
(setq user-full-name "MingxunBai"
      user-mail-address "mingxunbai@outlook.com")

;; 定义常量
(defconst *TERMINAL*                    ; 针对终端进行一些样式调整
  (eq window-system 'nil))
(defconst *WINDOWS* (eq system-type 'windows-nt))
(defconst *PLUGINS* (expand-file-name "plugins" user-emacs-directory))

;; 路径配置

;; Add load path recursive
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(setq default-directory
      (if *WINDOWS*
          (format "C:/Users/%s/Documents" user-full-name)
        "~/Documents"))

;;-------------------------------------------------
;; 编码环境
;;-------------------------------------------------

(setq current-language-environment "utf-8"
      default-buffer-file-coding-system 'utf-8
      locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; (modify-coding-system-alist 'file "\\.bat\\'" 'chinese-iso-8bit)

;; use gbk for Windows
(when *WINDOWS*
  (set-default 'process-coding-system-alist
               '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
                 ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos)))
  (setq locale-coding-system 'gbk))     ; 覆盖 utf-8, 确保 Windows 下 buffer-line 日期不乱码


;;-------------------------------------------------
;; 显示 & 行为
;;-------------------------------------------------

;; Alias
(defalias 'alr 'align-regexp)
(defalias 'cw  'compare-windows)
(defalias 'ev  'eval-buffer)
(defalias 'es  'custom-eshll)
(defalias 'ff  'set-buffer-file-coding-system)
(defalias 'rr  'replace-regexp)
(defalias 'rs  'replace-string)
(defalias 'x   'save-buffers-kill-emacs)

;; (set-default-font "Source Code Pro-12")
;; 设置字体
(when *WINDOWS*
  (set-fontset-font t 'han (font-spec :family "Minglan_Code")))

(global-auto-revert-mode)               ; Auto revert

(ido-mode)
(setq ido-save-directory-list-file nil
      ido-enable-flex-matching t)       ; 模糊匹配

(global-linum-mode)                     ; 显示行号

(show-paren-mode)                       ; 高亮匹配括号

(menu-bar-mode -1)                      ; 隐藏菜单栏

(setq-default indent-tabs-mode nil      ;;
              c-basic-offset 4          ; 设置缩进为 4 个空格
              tab-width 4)              ;

(setq inhibit-startup-message t         ; 关闭启动动画
      ;; initial-scratch-message nil       ; 移除草稿文本

      visible-bell t                    ;;
      ring-bell-function 'ignore        ; 关闭错误提示音
      save-abbrevs nil                  ;;

      mode-require-final-newline nil    ; 禁止在文件尾创建新行

      ;; split-height-threshold nil        ;;
      ;; split-width-threshold 0           ; 垂直分屏

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

      column-number-mode                ;;
      line-number-mode                  ; 显示行号列号
      linum-format " %2d|"              ;;

      display-time-day-and-date t       ;;
      display-time-24hr-format t        ; 时间格式
      display-time-default-load-average nil

      frame-title-format                ;;
      '("Emacs " emacs-version " - "    ; Title 显示完整路径
        (buffer-file-name "%f" (dired-directory dired-directory "%b")))

      eshell-prompt-function            ;;
      (lambda ()                        ; Eshell 提示符
        (concat
         (propertize (format-time-string "[%Y-%m-%d %H:%M:%S] " (current-time)) 'face `(:foreground "green"))
         (propertize (eshell/pwd) 'face `(:foreground "blue"))
         (if (= (user-uid) 0) " # " " $ "))))

(display-time)                          ; 显示时间

(fset 'yes-or-no-p 'y-or-n-p)           ; y / n 代替 yes/no

(advice-add 'y-or-n-p                   ; Enter 代替 y
            :around #'y-or-n-p-with-return)

(defun y-or-n-p-with-return (orig-func &rest args)
  (let ((query-replace-map (copy-keymap query-replace-map)))
    (define-key query-replace-map (kbd "RET") 'act)
    (apply orig-func args)))

(when (not *TERMINAL*)
  ;; 设置透明度
  (set-frame-parameter (selected-frame) 'alpha '(95 . 70))

  ;; 格式化并高亮行号
  (setq linum-format 'my-linum-format)

  (require 'hl-line)

  (defvar my-linum-current-line-number 0)

  (defface my-linum-hl
    `((t :inherit linum
         :background "#E8E8FF"
         :foreground "#000000",(face-background 'hl-line nil t)))
    "Face for the current line number."
    :group 'linum)

  (defun my-linum-format (line-number)
    (propertize (format " %2d\u2502" line-number) 'face
                (if (eq line-number my-linum-current-line-number)
                    'my-linum-hl
                  'linum)))

  (defadvice linum-update (around my-linum-update)
    (let ((my-linum-current-line-number (line-number-at-pos)))
      ad-do-it))

  (ad-activate 'linum-update)

  (global-hl-line-mode)

  (set-face-attribute hl-line-face nil :background "#E8E8FF")

  (set-face-background 'mode-line "#EDEDED")

  ;; 隐藏滚动条, 工具栏
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; Major Mode
(setq auto-mode-alist
      (append '(("/[^\\./]*\\'"    . conf-mode) ; File name has no dot
                ("\\.bash"         . sh-mode)
                ("\\.yasnippet\\'" . snippet-mode))
              auto-mode-alist))

;; 启用完整配置
(defun full ()
  (interactive)
  (server-start)
  (add-subdirs-to-load-path *PLUGINS*)
  (require 'extensions))

(add-hook 'after-init-hook 'full)