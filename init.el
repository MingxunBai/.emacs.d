;;-------------------------------------------------
;; Basic setting
;;-------------------------------------------------

;; 设置个人信息
(setq user-full-name "MingxunBai"
      user-mail-address "mingxunbai@outlook.com")

;; 定义常量
(defconst *TERMINAL*                   ; 针对终端进行一些样式调整
  (eq window-system 'nil))
(defconst *WINDOWS* (eq system-type 'windows-nt))
(defconst *PLUGINS* (expand-file-name "plugins" user-emacs-directory))

;; 路径配置
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path *PLUGINS*)

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

;; (set-default-font "Source Code Pro-12")
;; 设置字体
(when *WINDOWS*
  (set-fontset-font t 'han (font-spec :family "Minglan_Code"))

  ;; 设置透明度
  (set-frame-parameter (selected-frame) 'alpha '(95 . 70)))

;; Themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'monokai t)

(global-auto-revert-mode)               ; Auto revert

(global-font-lock-mode)                 ; 语法高亮

(global-linum-mode)                     ; 显示行号

(ido-mode)
(setq ido-save-directory-list-file nil
      ido-enable-flex-matching t)       ; 模糊匹配

(menu-bar-mode -1)                      ; 隐藏菜单栏

(show-paren-mode)                       ; 高亮匹配括号

(setq-default indent-tabs-mode nil      ;;
              tab-width 4)              ; 默认缩进为 4 个空格

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

      linum-format "%5d|"               ;
      column-number-mode                ; 显示行号列号
      line-number-mode                  ;;

      show-paren-style 'parenthesis     ; 光标不会跳到另一个括号处

      display-time-day-and-date t       ;;
      display-time-24hr-format t        ; 显示时间日期
      display-time-default-load-average nil

      frame-title-format                ; Title 显示完整路径
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

      eshell-prompt-function            ;;
      (lambda ()                        ; Eshell 提示符
        (concat
         (propertize (format-time-string "[%Y-%m-%d %H:%M] " (current-time)) 'face `(:foreground "#A6E22E"))
         (propertize (eshell/pwd) 'face `(:foreground "Pink"))
         (if (= (user-uid) 0) " # " " $ "))))

(display-time)                          ; 显示时间

(fset 'yes-or-no-p 'y-or-n-p)           ; y / n 代替 yes/no

(advice-add 'y-or-n-p                   ; Enter 代替 y
            :around #'y-or-n-p-with-return)

(defun y-or-n-p-with-return (orig-func &rest args)
  (let ((query-replace-map (copy-keymap query-replace-map)))
    (define-key query-replace-map (kbd "RET") 'act)
    (apply orig-func args)))

(if (not *TERMINAL*)
    (progn
      ;; 格式化并高亮行号
      (setq linum-format 'my-linum-format)

      (require 'hl-line)

      (defvar my-linum-current-line-number 0)

      (defface my-linum-hl
        `((t :inherit linum
             :background "#3C3D37"
             :foreground "#FFFFFF",(face-background 'hl-line nil t)))
        "Face for the current line number."
        :group 'linum)

      (defun my-linum-format (line-number)
        (propertize (format "%5d\u2502" line-number) 'face
                    (if (eq line-number my-linum-current-line-number)
                        'my-linum-hl
                      'linum)))

      (defadvice linum-update (around my-linum-update)
        (let ((my-linum-current-line-number (line-number-at-pos)))
          ad-do-it))

      (ad-activate 'linum-update)

      (global-hl-line-mode)

      ;; 隐藏滚动条, 工具栏
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

;;-------------------------------------------------
;; Extensions
;;-------------------------------------------------

;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'company-dict)
(setq company-idle-delay 0
      company-dict-dir (concat *PLUGINS* "/company/dict"))
(add-to-list 'company-backends 'company-dict)

;; Highlight parentheses mode
(require 'highlight-parentheses)
(global-highlight-parentheses-mode)

;; History
(require 'history)

;; Lazy set key
(require 'lazy-set-key)
(require 'lazy-init-bind)

;; Multiple cursors
(require 'multiple-cursors)

;; Paren face mode
(require 'paren-face)
(global-paren-face-mode)

;; Smart mode line
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t
      sml/theme 'respectful)
(smart-mode-line-enable)

;; Smart parens mode
(require 'smartparens-config)
(smartparens-global-mode)

;; Tab bar mode
(require 'tabbar)
(tabbar-mode)
(setq tabbar-use-images nil)

;; Windows numbering
(require 'window-numbering)
(window-numbering-mode)

;;-------------------------------------------------
;; Major Mode
;;-------------------------------------------------

(setq auto-mode-alist
      (append '(("/[^\\./]*\\'" .   conf-mode) ; File name has no dot
                ("\\.bash"      .   sh-mode))
              auto-mode-alist))

;;-------------------------------------------------
;; Custom Feature
;;-------------------------------------------------

;; Before save hook
(add-hook 'before-save-hook 'custom-before-save-hook)
(defun custom-before-save-hook ()
  (delete-trailing-whitespace)
  (custom-ff-utf-8-unix))

(defun custom-ff-utf-8-unix ()
  (if (and (not (eq major-mode 'bat-mode))
           (not (string-match "utf-8-unix" (symbol-name buffer-file-coding-system))))
      (set-buffer-file-coding-system 'utf-8-unix)
    (message "It's a unix file.")))

;; Command 分离窗口运行
(defun custom-split-window (command &optional param)
  (delete-other-windows)
  (split-window-vertically (floor (* 0.68 (window-height))))
  (other-window 1)
  (funcall command param)
  (other-window 1))

;; Lisp mode
(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "<f5>") 'eval-last-sexp)

;; Eshell
(add-hook 'eshell-exit-hook 'delete-window)
(defun custom-eshll ()                  ; 设置别名为 es
  (interactive)
  (if (not (condition-case nil
               (setq path (file-name-directory (buffer-file-name)))
             (error nil)))
      (message "Eshell need a local file buffer!")
    (progn
      (kill-new (concat "cd " path))
      (custom-split-window 'eshell)
      (other-window 1)
      (let ((pwd (eshell/pwd)))
        (if (not (string-equal pwd path))
            (eshell/cd path))))))

;; 缩进重排
(defun custom-indent-buffer ()
  (interactive)
  (let ((line (if (bolp)
                  (1+ (count-lines 1 (point)))
                (count-lines 1 (point))))
        (step (custom-remeber-point-step)))
    (indent-region (point-min) (point-max))
    (goto-line line)
    (back-to-indentation)
    (forward-char step)))

;; 自定缩进
(defun custom-resize-indentation (n)
  (interactive "nEnter indentation size:")
  (if (use-region-p)
      (let (mark (mark))
        (save-excursion
          (save-match-data
            (indent-rigidly
             (region-beginning)
             (region-end)
             n)
            (push-mark mark t t)
            (setq deactivate-mark nil))))
    (indent-rigidly
     (line-beginning-position)
     (line-end-position)
     n)))

(defun custom-resize-indentation--4 ()
  (interactive)
  (custom-resize-indentation -4))

;; 删除空白字符至上一行末尾
(defun custom-delete-whitespace-to-upline ()
  (interactive)
  (progn
    (delete-indentation)
    (indent-according-to-mode)))

;; 行号点击, 拖拽功能
(defvar *linum-mdown-line* nil)
(defun line-at-click ()
  (save-excursion
	(let ((click-y (cdr (cdr (mouse-position))))
		  (line-move-visual-store line-move-visual))
	  (setq line-move-visual t)
	  (goto-char (window-start))
	  (next-line (1- click-y))
	  (setq line-move-visual line-move-visual-store)
	  (line-number-at-pos))))

(defun custom-md-select-linum ()
  (interactive)
  (goto-line (line-at-click))
  (set-mark (point))
  (setq *linum-mdown-line*
		(line-number-at-pos)))

(defun custom-mu-select-linum ()
  (interactive)
  (when *linum-mdown-line*
	(let (mu-line)
	  ;; (goto-line (line-at-click))
	  (setq mu-line (line-at-click))
	  (goto-line (max *linum-mdown-line* mu-line))
	  (set-mark (line-end-position))
	  (goto-line (min *linum-mdown-line* mu-line))
	  (setq *linum-mdown*
			nil))))

;; 换行
(defun custom-return ()
  (interactive)
  (if (or (custom-paren-match "{" "}")
          (custom-paren-match "[" "]")
          (custom-paren-match "(" ")")
          (custom-paren-match ">" "<"))
      (custom-middle-newline)
    (newline-and-indent)))

;; 向上新建一行
(defun custom-up-newline ()
  (interactive)
  (progn
    (beginning-of-line)
    (newline-and-indent)
    (previous-line)
    (indent-according-to-mode)))

;; 向下新建一行
(defun custom-down-newline ()
  (interactive)
  (progn
    (end-of-line)
    (newline-and-indent)))

;; 标号内新建一行
(defun custom-middle-newline ()
  (interactive)
  (progn
    (newline-and-indent)
    (newline-and-indent)
    (previous-line)
    (indent-according-to-mode)))

;; 匹配括号
(defun custom-paren-match (bef end)
  (if (and (string-equal bef (string (preceding-char)))
           (string-equal end (string (following-char))))
      't))

;; 在右侧新建一个窗口
(defun custom-split-window-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

;; 在下方新建一个窗口
(defun custom-split-window-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

;; 粘贴
(defun custom-yank ()
  (interactive)
  (let ((point-before (point)))
    (yank)
    (indent-region point-before (point))))

;; 移动当前行
(defun custom-move-current-line (n)
  (progn
    (kill-new "")
    (beginning-of-line)
    (kill-whole-line)
    (forward-line n)
    (custom-yank)
    (forward-line -1)
    (back-to-indentation)
    (forward-char step)
    (indent-according-to-mode)))

(defun custom-remeber-point-step ()
  (let ((bef (point)))
    (back-to-indentation)
    (let ((step (- bef (point))))
      (if (< step 0)
          0
        step))))

;; 上移一行
(defun custom-move-up-current-line ()
  (interactive)
  (let ((step (custom-remeber-point-step)))
    (beginning-of-line)
    (if (bobp)
        (progn
          (message "Beginning of buffer!")
          (back-to-indentation)
          (forward-char step))
      (progn
        (end-of-line)
        (if (eobp)
            (progn
              (newline)
              (forward-line -1)
              (custom-move-current-line -1))
          (custom-move-current-line -1))))))

;; 下移一行
(defun custom-move-down-current-line ()
  (interactive)
  (let ((step (custom-remeber-point-step)))
    (end-of-line)
    (if (eobp)
        (progn
          (message "End of buffer!")
          (back-to-indentation)
          (forward-char step))
      (custom-move-current-line 1))))

;; 启用完整配置
(defun full ()
  (interactive)
  (require 'extensions))

;; 最大化
(custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))