;;-------------------------------------------------
;; Basic setting
;;-------------------------------------------------

;; 设置个人信息
(setq user-full-name "MingxunBai"
      user-mail-address "mingxunbai@outlook.com")

;; 检测系统
(defconst *Windows* (eq system-type 'windows-nt))

;; 路径配置
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path (expand-file-name "plugins/" user-emacs-directory))

(when *Windows*
  (setq default-directory "D:/Tools/xampp/htdocs"))

;; 配置五笔输入法
(defun auto-enable-wbpy-hook ()
  (autoload 'chinese-wbim-use-package "chinese-wbim" "Another emacs input method")
  (register-input-method "chinese-wbim" "euc-cn" 'chinese-wbim-use-package "五笔" "汉字五笔输入法" "wb.txt")

  (setq chinese-wbim-use-tooltip nil)   ; Tooltip 暂时还不好用

  ;; 用 ; 暂时输入英文
  (require 'chinese-wbim-extra)
  (global-set-key ";" 'chinese-wbim-insert-ascii)

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
(when *Windows*                         ; 设置中文字体为 "明兰黑"
  (set-fontset-font t 'han (font-spec :family "Minglan_Code")))

;; 语法高亮
(global-font-lock-mode)
(setq font-lock-maximum-decoration t    ; 只渲染当前 buffer 语法高亮
      font-lock-verbose t
      font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000)))

;; 显示行号
(global-linum-mode)
(setq linum-format "%4d "
      column-number-mode
      line-number-mode)

(show-paren-mode)                       ; 高亮匹配括号
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

      track-eol t                       ; 换行时，光标始终保持在行首尾

      x-select-enable-clipboard t       ; 支持和外部程序的拷贝

      make-backup-files nil             ; 不生成备份文件
      auto-save-default nil             ; 不生成临时文件

      max-lisp-eval-depth 10000)

(fset 'yes-or-no-p 'y-or-n-p)           ; 使用 y/n 替代 yes/no

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

;; 设置缩进
(setq-default indent-tabs-mode nil      ; 设置缩进为空格
              default-tab-width 4       ; 设置默认缩进为 4
              c-basic-offset 4)         ; 修改 C 语言缩进为 4

;; 移动缩进
(defun resize-indentation (n)
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
(global-set-key (kbd "<backtab>") 'resize-indentation)

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
(setq skeleton-pair-alist
      '((?\" _ "\"" >)
        (?\' _ "\'" >)
        (?\( _ ")" >)
        (?\[ _ "]" >)
        (?\{ _ "}" >)
        (?\< _ ">" >))

      skeleton-pair t)

(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)

;; 在右侧新建一个窗口
(defun new-right-window ()
  (interactive)
  (split-window-right)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'new-right-window)

;;-------------------------------------------------
;; Internal mode
;;-------------------------------------------------

;; HS mode
(global-set-key [f2] 'hs-toggle-hiding)

;; Ido mode
(ido-mode)
(setq ido-save-directory-list-file nil)

;; Winner mode
(when (fboundp 'winner-mode)
  (winner-mode))
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)

;;-------------------------------------------------
;; Extension
;;-------------------------------------------------

;; Auto complete mode
(defun auto-enable-auto-complete-mode ()
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (expand-file-name "plugins/auto-complete/dict" user-emacs-directory))
  (ac-config-default)
  (global-auto-complete-mode)
  (setq ac-auto-start nil)

  (setq ac-use-menu-map t)
  (define-key ac-mode-map "\M-/" 'auto-complete)
  (define-key ac-completing-map "\M-/" 'ac-stop))

;; Emmet mode
(defun enable-emmet-mode ()
  (interactive)
  (require 'emmet-mode)
  (emmet-mode)

  (define-key emmet-mode-keymap (kbd "C-M-[") 'emmet-prev-edit-point)
  (define-key emmet-mode-keymap (kbd "C-M-]") 'emmet-next-edit-point))

;; Highlight indent guides
(require 'highlight-indent-guides)

(setq highlight-indent-guides-method 'character)

;; Highlight parentheses mode
(require 'highlight-parentheses)
(highlight-parentheses-mode)

;; History
(require 'history)

;; JavaScript IDE mode
(defun enable-js2-mode ()
  (interactive)
  (require 'js2-mode)
  (js2-mode))

;; Markdown mode
(defun enable-markdown-mode ()
  (interactive)
  (require 'markdown-mode)
  (markdown-mode)

  (when *Windows*                   ; set markdown-command for windows
    (custom-set-variables '(markdown-command "markdown.pl"))))

;; Multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; Project explorer
(require 'project-explorer)

(global-set-key [f1] 'project-explorer-toggle)

;; Web mode
(defun enable-web-mode ()
  (interactive)
  (require 'web-mode)
  (web-mode)

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-current-element-highlight t)

  ;; C-c C-v: 在浏览器中预览
  (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-file))

;; Windows numbering
(require 'window-numbering)
(window-numbering-mode)

;; Yaml mode
(defun enable-yaml-mode ()
  (interactive)
  (require 'yaml-mode)
  (yaml-mode))

;; YASnippet
(defun auto-enable-yasnippet ()
  (require 'yasnippet)
  (yas-global-mode)

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
       :isearch t                     ; start isearch mode immediately
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
                ("\\.ya?ml\\'" . (lambda ()
                                   (enable-yaml-mode))))
              auto-mode-alist))

;;-------------------------------------------------
;; Hook
;;-------------------------------------------------

;; Auto complete mode
(add-hook 'conf-mode-hook 'auto-complete-mode)
(add-hook 'text-mode-hook 'auto-complete-mode)

;; Elisp mode
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (setq skeleton-pair-alist
                                         '((?\' "" >)))))

;; Emacs init
(add-hook 'after-init-hook (lambda ()
                             ;; Auto complete mode
                             (auto-enable-auto-complete-mode)

                             ;; YASnippet
                             (auto-enable-yasnippet)

                             ;; 五笔输入法
                             (auto-enable-wbpy-hook)))

;; JavaScript IDE
(add-hook 'js-mode-hook 'enable-js2-mode)
(add-hook 'js2-mode-hook 'my-web-dev-hook)

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
      (insert (format "#+BEGIN_SRC %s\n" src-code-type))
      (newline-and-indent)
      (insert "#+END_SRC")
      (previous-line 2)
      (org-edit-src-code)))
  (local-set-key (kbd "C-c c e") 'org-edit-src-code)
  (local-set-key (kbd "C-c c i") 'org-insert-src-block))

(add-hook 'org-mode-hook 'my-org-mode-hook)

;; Python mode
(defun my-python-mode-hook ()
  (interactive)
  (require 'elpy)
  (elpy-mode)

  (highlight-indent-guides-mode)

  (require 'py-autopep8)
  (py-autopep8-enable-on-save)

  (setq python-shell-prompt-detect-enabled nil))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Web mode
(defun my-web-mode-hook ()
  ;; 禁止 < 自动补齐
  (setq skeleton-pair-alist
        '((?\< "" >)))

  (enable-emmet-mode)
  (highlight-indent-guides-mode)
  (hs-minor-mode))

(add-hook 'css-mode-hook 'enable-web-mode)
(add-hook 'html-mode-hook 'enable-web-mode)
(add-hook 'nxml-mode-hook 'enable-web-mode)
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; 保存前删除多余空格
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 最大化
(custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))
