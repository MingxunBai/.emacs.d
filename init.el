(setq debug-on-error t)

;; Alias
(defalias 'es  'custom-eshll)
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
;; (add-subdirs-to-load-path (expand-file-name "plugins" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))

;; ELPA
(setq package-archives
      '(("cn-gnu"       . "http://elpa.emacs-china.org/gnu/")
        ("cn-melpa"     . "http://elpa.emacs-china.org/melpa/")
        ("cn-marmalade" . "http://elpa.emacs-china.org/marmalade/")
        ("cn-org"       . "http://elpa.emacs-china.org/org/")
        ))
(require 'package)
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            (package-install package nil)
          (package-install package))
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
  (setq locale-coding-system 'gbk) ; 覆盖 utf-8, 确保 Windows 下 mode-line 日期不乱码
  )

;; GC
(when (version< "24.5" emacs-version)
  (setq gc-cons-threshold (* 512 1024 1024)
        gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect))

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

      kill-ring-max 500                 ; 设置历史记录数量

      kill-whole-line t                 ; 在行首 C-k 时，同时删除该行

      track-eol t                       ; 换行时，光标始终保持在行首尾

      x-select-enable-clipboard t       ; 支持和外部程序的拷贝

      auto-save-timeout 3               ; 自动保存等待时间
      make-backup-files nil             ; 禁用文件备份

      frame-title-format                ;;
      '("Emacs " emacs-version " - "    ; Title Format
        (buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(fset 'yes-or-no-p 'y-or-n-p)           ; y / n 代替 yes / no

;; Enter 代替 y
(defun y-or-n-p-with-return (orig-func &rest args)
  (let ((query-replace-map (copy-keymap query-replace-map)))
    (define-key query-replace-map (kbd "RET") 'act)
    (apply orig-func args)))
(advice-add 'y-or-n-p :around #'y-or-n-p-with-return)

(require 'init-gui)
(require 'init-mode)
(require 'init-keymap)
(require 'init-feature)
