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

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

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

  (set-face-attribute hl-line-face nil  ; 高亮当前行
                      :background "#E8E8FF")

  ;; mode line
  (set-face-background 'mode-line "#EDEDED")

  ;; 隐藏滚动条, 工具栏
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;;-------------------------------------------------
;; Extensions
;;-------------------------------------------------

;; History
(require 'history)

;; Lazy set key
(require 'lazy-set-key)
(require 'lazy-init-bind)

;;-------------------------------------------------
;; Major Mode
;;-------------------------------------------------

(setq auto-mode-alist
      (append '(("/[^\\./]*\\'" . conf-mode) ; File name has no dot
                ("\\.bash"      . sh-mode))
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
  (if (and (and (not (eq major-mode 'bat-mode))
                (not (eq major-mode 'java-mode)))
           (not (string-match "utf-8-unix" (symbol-name buffer-file-coding-system))))
      (set-buffer-file-coding-system 'utf-8-unix)))

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
(add-hook 'eshell-exit-hook (lambda () (if (not (eq (count-windows) 1)) (delete-window))))
(defun custom-eshll ()                  ; 设置别名为 es
  (interactive)
  (if (condition-case nil
          (setq path (file-name-directory (buffer-file-name)))
        (error nil))
      (progn
        (custom-split-window 'eshell)
        (other-window 1)
        (eshell/cd path)))
  (message "Eshell need a local file!"))

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
(defun custom-go-to-click-line ()
  (interactive)
  (save-excursion
    (let ((click-y (cdr (cdr (mouse-position)))))
      (goto-char (window-start))
      (next-line (1- click-y))
      (if (fboundp 'tabbar-mode)
          (setq line (line-number-at-pos))
        (setq line (1+ (line-number-at-pos))))))
  (goto-line line))

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

;; 换行
(defun custom-return ()
  (interactive)
  (cond ((custom-is-in-paren?)
         (custom-middle-newline))
        ((custom-is-javadoc?)
         (custom-javadoc-newline))
        (t (newline-and-indent))))

;; 匹配光标上下文
(defun custom-paren-match (bef end)
  (if (and (string-equal bef (string (preceding-char)))
           (string-equal end (string (following-char))))
      't
    nil))

(defun custom-is-in-paren? ()
  (if (or (custom-paren-match "{" "}")
          (custom-paren-match "[" "]")
          (custom-paren-match "(" ")")
          (custom-paren-match ">" "<"))
      't
    nil))

(defun custom-is-javadoc? ()
  (if (and (string-equal "*" (string (char-before)))
           (string-equal "*" (string (char-before (- (point) 1))))
           (string-equal "/" (string (char-before (- (point) 2))))
           )
      't
    nil))

;; 标点中新建一行
(defun custom-middle-newline ()
  (interactive)
  (newline-and-indent)
  (newline-and-indent)
  (previous-line)
  (indent-according-to-mode))

;; javadoc
(defun custom-javadoc-newline ()
  (interactive)
  (newline-and-indent)
  (insert "*")
  (indent-according-to-mode)
  (newline-and-indent)
  (insert "*/")
  (indent-according-to-mode)
  (previous-line))

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
  (kill-new "")
  (beginning-of-line)
  (kill-whole-line)
  (forward-line n)
  (yank)
  (forward-line -1)
  (back-to-indentation)
  (forward-char step)
  (indent-according-to-mode))

(defun custom-remeber-point-step ()
  (let ((bef (point)))
    (back-to-indentation)
    (let ((step (- bef (point))))
      (if (< step 0)
          0
        step))))

(defun custom-repos (msg)
  (message msg)
  (back-to-indentation)
  (forward-char step))

(defun custom-forward-line-end (n)
  (forward-line n)
  (end-of-line))

(defun custom-bobp? ()
  (beginning-of-line)
  (if (bobp)
      't
    nil))

(defun custom-eobp? (n)
  (custom-forward-line-end n)
  (let ((rst (eobp)))
    (custom-forward-line-end (- 0 n))
    rst))

(defun custom-next-line-empty? ()
  (forward-line 1)
  (let ((rst (eq (point-at-bol) (point-at-eol))))
    (forward-line -1)
    rst))

;; 上移一行
(defun custom-move-up-current-line ()
  (interactive)
  (let ((step (custom-remeber-point-step)))
    (cond ((custom-bobp?)
           (custom-repos "Beginning of buffer!"))
          ((custom-eobp? 0)
           (newline)
           (forward-line -1)
           (custom-move-current-line -1))
          (t (custom-move-current-line -1)))))

;; 下移一行
(defun custom-move-down-current-line ()
  (interactive)
  (let ((step (custom-remeber-point-step)))
    (cond ((custom-eobp? 0)
           (custom-repos "End of buffer!"))
          ((and (custom-next-line-empty?) (custom-eobp? 1))
           (custom-repos "End of buffer!"))
          ((and (not (custom-next-line-empty?)) (custom-eobp? 1))
           (custom-forward-line-end 1)
           (newline)
           (forward-line -2)
           (custom-move-current-line 1))
          (t (custom-move-current-line 1)))))

;; 复制当前行
(defun custom-duplicate-line ()
  (interactive)
  (let ((step (custom-remeber-point-step)))
    (kill-ring-save (point-at-bol) (point-at-eol))
    (custom-down-newline)
    (custom-yank)
    (back-to-indentation)
    (forward-char step)))

;; 启用完整配置
(defun full ()
  (interactive)
  (require 'extensions))