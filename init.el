(setq debug-on-error t)

;; Alias
(defalias 'alr 'align-regexp)
(defalias 'cw  'compare-windows)
(defalias 'ev  'eval-buffer)
(defalias 'es  'custom-eshll)
(defalias 'ff  'set-buffer-file-coding-system)
(defalias 'ha  'helm-apropos)
(defalias 'hb  'helm-buffers-list)
(defalias 'hr  'helm-recentf)
(defalias 'rr  'replace-regexp)
(defalias 'rs  'replace-string)

;; Const
(defconst *WINDOWS*  (eq system-type 'windows-nt))
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path (expand-file-name "plugins" user-emacs-directory))

;; ELPA
(package-initialize)
(require 'package)
(setq package-archives
      '(
        ("gnu"       . "http://elpa.emacs-china.org/gnu/")
        ("marmalade" . "http://elpa.emacs-china.org/marmalade/")
        ("melpa"     . "http://elpa.emacs-china.org/melpa/")
        ("org"       . "http://elpa.emacs-china.org/org/")
        ))

(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; Encoding
(setq current-language-environment "utf-8"
      default-buffer-file-coding-system 'utf-8
      locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "\\.bat\\'" 'chinese-iso-8bit)

(when *WINDOWS*
  ;; use gbk for Windows
  (set-default 'process-coding-system-alist
               '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
                 ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos)))
  (setq locale-coding-system 'gbk) ; 覆盖 utf-8, 确保 Windows 下 buffer-line 日期不乱码
  )

;; GC
(when (version< "24.5" emacs-version)
  (setq gc-cons-threshold (* 512 1024 1024)
        gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect))

;; Major Mode
(setq auto-mode-alist
      (append '(("\\.bash"   . sh-mode)
                ("\\.ahk\\'" . (lambda () (enable-ahk-mode)))
                ("\\.md\\'"  . (lambda () (enable-markdown-mode))))
              auto-mode-alist))

;; Setting
(setq-default indent-tabs-mode nil      ;;
              c-basic-offset 4          ; 设置缩进为 4 个空格
              tab-width 4)              ;;

(setq inhibit-startup-message t         ; 关闭启动动画

      visible-bell t                    ;;
      ring-bell-function 'ignore        ; 关闭错误提示音
      save-abbrevs nil                  ;;

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

      frame-title-format                ;;
      '("Emacs " emacs-version " - "    ; Title Format
        (buffer-file-name "%f" (dired-directory dired-directory "%b")))

      eshell-prompt-function            ;;
      (lambda ()                        ; Eshell Prompt
        (concat
         (propertize (format-time-string "[%Y-%m-%d %H:%M:%S] " (current-time)) 'face `(:foreground "green"))
         (propertize (eshell/pwd) 'face `(:foreground "blue"))
         (if (= (user-uid) 0) " # " " $ "))))

(fset 'yes-or-no-p 'y-or-n-p)           ; y / n 代替 yes/no

(advice-add 'y-or-n-p                   ; Enter 代替 y
            :around #'y-or-n-p-with-return)

(defun y-or-n-p-with-return (orig-func &rest args)
  (let ((query-replace-map (copy-keymap query-replace-map)))
    (define-key query-replace-map (kbd "RET") 'act)
    (apply orig-func args)))

(require 'init-gui)
(require 'init-mode)
(require 'init-keymap)
(require 'init-feature)
