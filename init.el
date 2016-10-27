;;-------------------------------------------------
;; Basic setting
;;-------------------------------------------------

;; 设置个人信息
(setq user-full-name "MingxunBai"
      user-mail-address "mingxunbai@outlook.com")

;; 检测系统
(defconst *Windows* (eq system-type 'windows-nt))

;; 路径配置
(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))

(when *Windows*
  (setq default-directory "D:/Tools/xampp/htdocs"))

;; 配置五笔输入法
(defun enable-wbpy-hook ()
  (add-to-list 'load-path (expand-file-name "plugins/input-wbpy" user-emacs-directory))

  (setq chinese-wbim-use-tooltip nil)     ; Tooltip 暂时还不好用

  (register-input-method "chinese-wbim" "euc-cn" 'chinese-wbim-use-package "五笔" "汉字五笔输入法" "wb.txt")

  ;; 用 ; 暂时输入英文
  (require 'chinese-wbim-extra)
  (global-set-key ";" 'chinese-wbim-insert-ascii)

  (autoload 'chinese-wbim-use-package "chinese-wbim" "Another emacs input method")

  ;; 设置默认输入法为五笔输入法英文状态, C-\ 切换
  (progn
    (interactive)
    (set-input-method 'chinese-wbim)
    (toggle-input-method)))

;;-------------------------------------------------
;; 编码环境
;;-------------------------------------------------

(setq current-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;-------------------------------------------------
;; 显示
;;-------------------------------------------------

(load-theme 'misterioso t)              ; 设置主题
(set-cursor-color "Wheat")              ; 设置光标颜色
(set-face-background 'region "Blue")    ; 设置选中区块背景色

(setq inhibit-startup-message t         ; 关闭启动动画

      visible-bell t                    ; 关闭错误提示音
      ring-bell-function 'ignore save-abbrevs nil

      split-height-threshold nil        ; 垂直分屏
      split-width-threshold 0

      scroll-margin 3                   ; 靠近屏幕边沿3行时就开始滚动
      scroll-conservatively 10000

      frame-title-format '("Emacs@%S" (buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; 设置字体
;; (set-default-font "Source Code Pro-12")
(when *Windows*
  (set-fontset-font t 'han (font-spec :family "Minglan_Code"))) ; 修改 windows 下的中文字体为 "明兰黑"

;; 语法高亮
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t    ; 只渲染当前 buffer 语法高亮
      font-lock-verbose t
      font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000)))

;; 显示行号
(global-linum-mode t)
(setq linum-format "%4d "
      column-number-mode t
      line-number-mode t)

(show-paren-mode t)                     ; 高亮匹配括号
(setq show-paren-style 'parenthesis)    ; 光标不会跳到另一个括号处

(scroll-bar-mode -1)                    ; 隐藏滚动条
(tool-bar-mode -1)                      ; 隐藏工具栏
(menu-bar-mode -1)                      ; 隐藏菜单栏

(setq display-time-24hr-format t)       ; 24小时制
(display-time)                          ; 启用时间显示

;;-------------------------------------------------
;; 操作
;;-------------------------------------------------

(setq default-major-mode 'text-mode     ; 设置默认主模式为 text-mode

      kill-ring-max 500                 ; 设置历史记录数量

      inhibit-startup-message t         ; 关闭出错提示音

      kill-whole-line t                 ; 在行首 C-k 时，同时删除该行

      track-eol t                       ; 换毛换行时，光标始终保持在行首尾

      x-select-enable-clipboard t       ; 支持和外部程序的拷贝

      make-backup-files nil             ; 不生成备份文件
      auto-save-default nil)            ; 不生成临时文件

(fset 'yes-or-no-p 'y-or-n-p)           ; 使用 y/n 替代 yes/no

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

;; 移动缩进
(defun resize-indentation (n)
  (interactive "nEnter indentation Size:")
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
(global-set-key (kbd "<backtab>") 'resize-indentation)

;; 设置缩进
(setq-default indent-tabs-mode nil      ; 设置缩进为空格
              default-tab-width 4       ; 设置默认缩进为 4
              c-basic-offset 4)         ; 修改 C 语言缩进为 4

;; 删除空白字符至上一行末尾
(defun delete-whitespace-to-upline ()
  (interactive)
  (progn
    (delete-indentation)
    (indent-according-to-mode)))
(global-set-key (kbd "C-c k") 'delete-whitespace-to-upline)

;; 向上新建一行
(defun up-newline ()
  (interactive)
  (progn
    (beginning-of-line)
    (newline-and-indent)
    (previous-line)
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'up-newline)

;; 向下新建一行
(defun down-newline ()
  (interactive)
  (progn
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "C-o") 'down-newline)

;; 自动匹配括号
(defun auto-pair ()
  (setq skeleton-pair-alist
        '((?\" _ "\"" >)
          (?\' _ "\'" >)
          (?\( _ ")" >)
          (?\[ _ "]" >)
          (?\{ _ "}" >)
          (?\< _ ">" >)))

  (setq skeleton-pair t)

  (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
  (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
  (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (global-set-key (kbd "<") 'skeleton-pair-insert-maybe))

;; 在右侧新建一个窗口
(defun new-right-window ()
  (interactive)
  (split-window-right)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'new-right-window)

;;-------------------------------------------------
;; Internal mode
;;-------------------------------------------------

;; Emacs lisp mode
(defun unable-quotation-hook ()
  ;; 禁止 ' 自动补齐
  (setq skeleton-pair-alist
        '((?\' "" >))))

;; HS mode
(global-set-key [f2] 'hs-toggle-hiding)

;; Ido mode
(ido-mode t)
(setq ido-save-directory-list-file nil)

;; Winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1))
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)

;;-------------------------------------------------
;; Extension
;;-------------------------------------------------

;; Auto complete mode
(defun enable-auto-complete-mode ()
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (expand-file-name "plugins/auto-complete/dict" user-emacs-directory))

  ;; enable
  (ac-config-default)
  (global-auto-complete-mode t)
  (setq ac-auto-start nil)

  ;; set hot key
  (setq ac-use-menu-map t)
  (define-key ac-mode-map "\M-/" 'auto-complete)
  (define-key ac-completing-map "\M-/" 'ac-stop))

;; Emmet mode
(defun enable-emmet-mode ()
  (require 'emmet-mode)
  (emmet-mode)

  (define-key emmet-mode-keymap (kbd "C-M-[") 'emmet-prev-edit-point)
  (define-key emmet-mode-keymap (kbd "C-M-]") 'emmet-next-edit-point))

;; Highlight parentheses mode
(defun enable-highlight-parentheses-mode ()
  (require 'highlight-parentheses)
  (global-highlight-parentheses-mode)

  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda ()
      (highlight-parentheses-mode t))))

;; History
(defun enable-history ()
  (require 'history))

;; JS2 mode
(defun enable-js2-mode ()
  (interactive)
  (require 'js2-mode)
  (js2-mode))

;; Markdown mode
(defun enable-markdown-mode ()
  (interactive)
  (require 'markdown-mode)
  (markdown-mode)

  (when *Windows*                       ; set markdown-command for windows
    (custom-set-variables '(markdown-command "markdown.pl"))))

;; Multiple cursors
(defun enable-multiple-cursors ()
  (require 'multiple-cursors)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

;; Powerline
(defun enable-powerline ()
  (require 'powerline)
  (powerline-default-theme))

;; Project explorer
(defun enable-project-explorer ()
  (require 'project-explorer)
  (global-set-key [f1] 'project-explorer-toggle))

;; Web mode
(defun enable-web-mode ()
  (interactive)
  (require 'web-mode)
  (web-mode)

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-current-element-highlight t)

  ;; 绑定 用浏览器打开文件 快捷键为 C-c C-v
  (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-file))

;; Windows numbering
(defun enable-windows-numbering ()
  (require 'window-numbering)
  (window-numbering-mode 1))

;; Yaml mode
(defun enable-yaml-mode ()
  (interactive)
  (require 'yaml-mode))

;; YASnippet
(defun enable-yasnippet ()
  (require 'yasnippet)
  (yas-global-mode 1)

  ;; use popup menu for yas-choose-value
  (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
    (when (featurep 'popup)
      (popup-menu*
       (mapcar
        (lambda (choice)
          (popup-make-item
           (or (and display-fn (funcall display-fn choice))
               choice)
           :value choice))
        choices)
       :prompt prompt
       :isearch t ; start isearch mode immediately
       )))
  (setq yas-prompt-functions '(yas-popup-isearch-prompt))

  ;; completing point by some yasnippet key
  (defun yas-ido-expand ()
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
  (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-ido-expand))

;;-------------------------------------------------
;; 主模式
;;-------------------------------------------------

(setq auto-mode-alist
      (append '(("\\.md\\'" . (lambda ()
                                (enable-markdown-mode)))
                ("\\.php\\'" . (lambda ()
                                 (enable-web-mode)))
                ("\\.py\\'" . python-mode)
                ("\\.yaml\\'" . (lambda ()
                                  (enable-yaml-mode))))
              auto-mode-alist))

;;-------------------------------------------------
;; Hook
;;-------------------------------------------------

;; AC js2 mode
(defun ac-js2-mode-hook ()
  (require 'ac-js2)

  (ac-js2-mode)
  (my-web-dev-hook))

(add-hook 'js2-mode-hook 'ac-js2-mode-hook)

;; Auto complete mode
(add-hook 'conf-mode-hook 'auto-complete-mode)
(add-hook 'text-mode-hook 'auto-complete-mode)

;; Elisp mode
(add-hook 'emacs-lisp-mode-hook 'unable-quotation-hook)

;; Emacs init
(add-hook 'after-init-hook (lambda ()
                             ;; Auto complete mode
                             (enable-auto-complete-mode)

                             ;; Auto pair
                             (auto-pair)

                             ;; Highlight parentheses
                             (enable-highlight-parentheses-mode)

                             ;; History
                             (enable-history)

                             ;; Mulitple cursors
                             (enable-multiple-cursors)

                             ;; Powerline
                             (if window-system
                                 (enable-powerline))

                             ;; Project explorer
                             (enable-project-explorer)

                             ;; Windows numbering
                             (enable-windows-numbering)

                             ;; YASnippet
                             (enable-yasnippet)

                             ;; 五笔输入法
                             (enable-wbpy-hook)

                             ;; 最大化
                             (custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))))

;; JavaScript IDE
(add-hook 'js-mode-hook 'enable-js2-mode)

;; Org mode
(defun my-org-mode-hook ()
  ;; 禁止 [ 自动补齐
  (setq skeleton-pair-alist
        '((?\[ "" >)))

  (setq org-startup-indented t)         ; 自动缩进

  ;; 代码高亮
  (require 'htmlize)
  (setq org-src-fontify-natively t)

  (defun org-insert-src-block (src-code-type)
    (interactive
     (let ((src-code-types
            '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
              "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
              "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
              "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
              "scheme" "sqlite" "html")))
       (list (ido-completing-read "Source code type: " src-code-types))))
    (progn
      (newline-and-indent)
      (insert (format "#+BEGIN_SRC %s\n" src-code-type))
      (newline-and-indent)
      (insert "#+END_SRC\n")
      (previous-line 2)
      (org-edit-src-code)))
  (local-set-key (kbd "C-c c e") 'org-edit-src-code)
  (local-set-key (kbd "C-c c i") 'org-insert-src-block))

(add-hook 'org-mode-hook 'my-org-mode-hook)

;; Web mode
(defun my-web-dev-hook ()
  ;; 禁止 < 自动补齐
  (setq skeleton-pair-alist
        '((?\< "" >)))

  ;; plugins
  (hs-minor-mode t)
  (enable-emmet-mode))

(add-hook 'css-mode-hook 'enable-web-mode)
(add-hook 'html-mode-hook 'enable-web-mode)
(add-hook 'nxml-mode-hook 'enable-web-mode)
(add-hook 'web-mode-hook 'my-web-dev-hook)

;; 保存前删除多余空格
(add-hook 'before-save-hook 'delete-trailing-whitespace)
