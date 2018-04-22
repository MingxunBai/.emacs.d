;; extensions.el --- full emacs initial file

;; Copyright (C) 2017 MingxunBai

;; Author: MingxunBai
;; Keywords: convenience
;; Version: 1.0

;;; Code:

;;-------------------------------------------------
;; Basic Setting
;;-------------------------------------------------

;; 定义常量
(defconst *TERMINAL* (eq window-system 'nil))
(defconst *WINDOWS*  (eq system-type 'windows-nt))
(defconst *PLUGINS*  (expand-file-name "plugins" user-emacs-directory))

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

;; Custeom Variables
(custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))

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

  ;; Font
  (set-fontset-font t 'han (font-spec :family "Minglan_Code"))

  ;; GC
  (setq gc-cons-threshold (* 512 1024 1024)
        gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect))

;; Mode
(ido-mode)
(setq ido-save-directory-list-file nil
      ido-enable-flex-matching t)       ; 模糊匹配

(global-auto-revert-mode)               ; Auto revert

(global-linum-mode)                     ; 显示行号

(recentf-mode)                          ; 历史记录
(setq recentf-save-file (recentf-expand-file-name "~/.recentf"))

(server-mode)

(show-paren-mode)                       ; 高亮匹配括号

(winner-mode)                           ; 窗口控制

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

(when *TERMINAL* (menu-bar-mode -1))
(when (not *TERMINAL*)
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
    (propertize (format " %2d " line-number) 'face
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

;;-------------------------------------------------
;; Custom Feature
;;-------------------------------------------------

(defun custom-feature ()

  ;; Before save hook
  (defun custom-before-save-hook ()
    (delete-trailing-whitespace)
    (custom-ff-utf-8-unix))
  (add-hook 'before-save-hook 'custom-before-save-hook)

  (defun custom-ff-utf-8-unix ()
    (if (and (and (not (eq major-mode 'bat-mode))
                  (not (eq major-mode 'java-mode)))
             (not (string-match "utf-8-unix" (symbol-name buffer-file-coding-system))))
        (set-buffer-file-coding-system 'utf-8-unix)))

  ;; Command 分离窗口运行
  (defun custom-split-window (command &optional param)
    (delete-other-windows)
    (if (> (count-windows) 1)
        (neotree-toggle))
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (funcall command param)
    (other-window 1))

  ;; Git
  (defun custom-find-dir (dir reg)
    (interactive)
    (if (not (string-match "[a-z0-9_-]/" dir))
        nil
      (if (file-exists-p (expand-file-name reg dir))
          dir
        (custom-find-dir (expand-file-name "../" dir) reg))))

  (defun custom-git-push (root)
    (shell-command (concat "cd " root " && git add ."))
    (shell-command (concat "cd " root " && git commit -m 'Update'"))
    (shell-command (concat "cd " root " && git push")))

  (defun custom-git-push-current-buffer ()
    (interactive)
    (if (condition-case nil
            (setq file-path (file-name-directory (buffer-file-name)))
          (error nil))
        (let ((root (custom-find-dir file-path ".git/")))
          (custom-git-push root))
      (message "Git need a local file!")))

  ;; 缩进重排
  (defun custom-remeber-line ()
    (if (bolp)
        (1+ (count-lines 1 (point)))
      (count-lines 1 (point))))

  (defun custom-indent-buffer ()
    (interactive)
    (let ((line (custom-remeber-line))
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

  ;; 行号点击
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
    (cond ((or (bobp) (eobp))
           (newline-and-indent))
          ((custom-is-in-paren?)
           (custom-middle-newline))
          ((custom-is-javadoc-begin?)
           (custom-javadoc-begin-newline))
          ((custom-is-javadoc-mid?)
           (custom-javadoc-mid-newline))
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

  (defun custom-is-javadoc-begin? ()
    (if (equal 0 (string-match "\s*/\\*+" (thing-at-point 'line t)))
        't
      nil))

  (defun custom-is-javadoc-mid? ()
    (if (equal 0 (string-match "\s*\\*+" (thing-at-point 'line t)))
        't
      nil))

  ;; 标点中新建一行
  (defun custom-middle-newline ()
    (interactive)
    (newline-and-indent)
    (newline-and-indent)
    (previous-line)
    (indent-according-to-mode))

  ;; Javadoc
  (defun custom-javadoc-begin-newline ()
    (interactive)
    (newline-and-indent)
    (insert "*")
    (newline-and-indent)
    (insert "*/")
    (previous-line)
    (custom-indent-buffer))

  (defun custom-javadoc-mid-newline ()
    (interactive)
    (newline-and-indent)
    (insert "*")
    (custom-indent-buffer))

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

  )
(custom-feature)

;;-------------------------------------------------
;; Extensions
;;-------------------------------------------------

;; Input method
(require 'chinese-wbim-extra)
(autoload 'chinese-wbim-use-package "chinese-wbim" "Another emacs input method")
(register-input-method "chinese-wbim" "euc-cn" 'chinese-wbim-use-package "五笔" "汉字五笔输入法" "wb.txt")
(setq chinese-wbim-use-tooltip nil)

(set-input-method 'chinese-wbim)
(toggle-input-method)

;; AutoHotKey
(require-package 'xahk-mode)
(defun enable-ahk-mode ()
  (interactive)
  (require 'xahk-mode)
  (ahk-mode))

;; Auto Complete
(require-package 'auto-complete)
(require-package 'ac-helm)
(require 'auto-complete-config)
(require 'ac-helm)
(ac-config-default)
(setq ac-auto-start nil
      ac-ignore-case nil
      ac-use-menu-map t)
(add-to-list 'ac-dictionary-directories (concat *PLUGINS* "/dict"))

;; Emmet
(require-package 'emmet-mode)
(defun enable-emmet-mode ()
  (interactive)
  (require 'emmet-mode)
  (emmet-mode)
  (setq emmet-self-closing-tag-style ""
        emmet-move-cursor-between-quotes t)

  (define-key emmet-mode-keymap (kbd "<C-return>") nil)
  (define-key emmet-mode-keymap (kbd "C-M-[") 'emmet-prev-edit-point)
  (define-key emmet-mode-keymap (kbd "C-M-]") 'emmet-next-edit-point))

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
        (eshell/cd path))
    (progn
      (custom-split-window 'eshell)
      (other-window 1))))

;; Evil Nerd Commenter
(require-package 'evil-nerd-commenter)
(require 'evil-nerd-commenter)

;; GoLang
(require-package 'go)
(defun enable-go-mode ()
  (interactive)
  (require 'go-mode-autoloads)
  (go-mode)
  (defun custom-go-save-fmt ()
    (interactive)
    (shell-command (concat "go fmt " (buffer-file-name))))

  (define-key go-mode-map (kbd "C-c f") 'custom-go-save-fmt))

;; Helm
(require-package 'helm)
(require 'helm-config)

;; Java
(add-hook 'java-mode-hook 'custom-java-mode-hook)
(defun custom-java-mode-hook ()
  (defun custom-java-run ()
    (interactive)
    (save-buffer)
    (setq file-path (file-name-directory (buffer-file-name)))
    (let ((root (custom-find-dir file-path "src/")))
      (if root
          (let ((package (file-relative-name file-path root)))
            (setq cmd
                  (concat "cd " root
                          " && javac -d bin/ " package "*.java"
                          " && java -cp bin/ " (replace-regexp-in-string "/" "." (substring package 4)) (file-name-sans-extension (buffer-name)) "\n")))
        (progn
          (setq cmd
                (concat "cd " file-path
                        " && javac " (buffer-name)
                        " && java " (file-name-sans-extension (buffer-name)) "\n")))))
    (custom-split-window 'shell)
    (other-window 1)
    (end-of-buffer)
    (let ((line (custom-remeber-line)))
      (other-window 1)
      (process-send-string "shell" cmd)
      (other-window 1)
      (goto-line line)
      (other-window 1)))

  (define-key java-mode-map (kbd "<f5>") 'custom-java-run))

;; JavaScript IDE
(require-package 'js2-mode)
(require-package 'js-comint)
(defun enable-js2-mode ()
  (interactive)
  (require 'js2-mode)
  (js2-mode)
  (setq js2-strict-missing-semi-warning nil)

  (require 'js-comint)
  (setq inferior-js-program-command "node")
  (setq inferior-js-program-arguments '("--interactive"))

  (defun custom-js-send-buffer ()
    (interactive)
    (custom-split-window 'switch-to-buffer "*js*")
    (js-send-buffer))

  (define-key js2-mode-map (kbd "<f5>") 'custom-js-send-buffer))

;; JSON
(require-package 'json)
(defun enable-json-mode ()
  (interactive)
  (require 'json-mode)
  (json-mode))

;; Less
(require-package 'less-css-mode)
(defun enable-less-css-mode ()
  (interactive)
  (require 'less-css-mode)
  (less-css-mode)
  (setq less-css-compile-at-save t
        less-css-output-directory "../css"))

;; Lisp
(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "<f5>") 'eval-last-sexp)

;; Markdown
(require-package 'markdown-mode)
(defun enable-markdown-mode ()
  (interactive)
  (require 'markdown-mode)
  (markdown-mode)
  (when *WINDOWS*
    (custom-set-variables '(markdown-command "markdown.pl")))

  (define-key markdown-mode-map (kbd "C-c C-k") nil))

;; Multiple Cursors
(require-package 'multiple-cursors)
(require 'multiple-cursors)

;; NEROTree
(require-package 'neotree)
(require 'neotree)
(setq neo-theme 'ascii)

(defun custom-neotree-copy-relative-path ()
  (interactive)
  (neotree-copy-filepath-to-yank-ring)
  (other-window 1)
  (kill-new (file-relative-name (car kill-ring) (file-name-directory (buffer-file-name)))))

(define-key neotree-mode-map (kbd "j") 'neotree-next-line)
(define-key neotree-mode-map (kbd "k") 'neotree-previous-line)
(define-key neotree-mode-map (kbd "]") 'neotree-select-next-sibling-node)
(define-key neotree-mode-map (kbd "[") 'neotree-select-previous-sibling-node)
(define-key neotree-mode-map (kbd "C-c c") 'custom-neotree-copy-relative-path)

;; Org
(require-package 'htmlize)
(add-hook 'org-mode-hook 'custom-org-mode-hook)
(defun custom-org-mode-hook ()
  (org-indent-mode)
  (setq org-startup-indented t)
  (require 'htmlize)
  (setq org-src-fontify-natively t)   ;  HTML 代码高亮

  (define-key org-mode-map (kbd "C-c C-k") nil)

  (defun org-insert-src-block ()
    (interactive
     (let ((src-code-types
            '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++"
              "css" "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond"
              "mscgen" "octave" "oz" "plantuml" "R" "sass" "screen" "sql"
              "awk" "ditaa" "haskell" "latex" "lisp" "matlab" "ocaml"
              "org" "perl" "ruby" "scheme" "sqlite" "html")))
       (list (ido-completing-read "Source code type: " src-code-types))))
    (progn
      (insert (format "#+BEGIN_SRC %s\n" src-code-type))
      (newline-and-indent)
      (insert "#+END_SRC")
      (previous-line 2)
      (org-edit-src-code))

    (local-set-key (kbd "C-c c e") 'org-edit-src-code)
    (local-set-key (kbd "C-c c i") 'org-insert-src-block)))

;; Origami
(require-package 'origami)
(require 'origami)
(global-origami-mode)

;; Python
(require-package 'elpy)
(require-package 'py-autopep8)
(add-hook 'python-mode-hook 'custom-python-mode-hook)
(defun custom-python-mode-hook ()
  (require 'elpy)
  (elpy-mode)
  (require 'py-autopep8)
  (py-autopep8-enable-on-save)

  (define-key python-mode-map (kbd "<backtab>") nil)
  (setq python-shell-prompt-detect-enabled nil))

;; Scheme
(setq scheme-program-name "scheme")
(add-hook 'scheme-mode-hook 'custom-init-scheme-mode)
(add-hook 'inferior-scheme-mode-hook
          (lambda ()
            (define-key inferior-scheme-mode-map (kbd "C-c C-k") 'nil)))

(defun scheme-proc ()
  (unless (and ("shceme-buffer")
               (get-buffer "scheme-buffer"))
    (comint-check-proc "scheme-buffer"))
  (save-window-excursion
    (run-scheme scheme-program-name))
  (or ("scheme-get-process")))

(defun custom-scheme-send-definition ()
  (interactive)
  (end-of-line)
  (custom-split-window 'switch-to-buffer "*scheme*")
  (scheme-send-definition))

(defun custom-init-scheme-mode ()
  (require 'cmuscheme)

  (define-key scheme-mode-map (kbd "C-c C-k") 'nil)
  (define-key scheme-mode-map (kbd "<f5>")    'custom-scheme-send-definition))

;; SCSS
(require-package 'scss-mode)
(defun enable-scss-mode ()
  (require 'scss-mode)
  (scss-mode)
  (setq scss-compile-at-save t))

;; Smart Parens
(require-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode)
(add-hook 'eshell-mode-hook 'smartparens-mode)

;; Tab Bar
(require 'tabbar)
(setq tabbar-use-images nil)
(tabbar-mode)

(defun my-tabbar-buffer-groups ()
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
              ((eq major-mode 'dired-mode) "Dir")
              (t "File"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; Tramp
(setq tramp-default-host "45.78.52.152#29135"
      tramp-default-user "root")

;; Vimrc
(require-package 'vimrc-mode)
(defun enable-vimrc-mode ()
  (interactive)
  (require 'vimrc-mode)
  (vimrc-mode))

;; Web
(require-package 'web-mode)
(defun enable-web-mode ()
  (interactive)
  (require 'web-mode)
  (web-mode)
  (setq web-mode-markup-indent-offset             2
        web-mode-css-indent-offset                4
        web-mode-code-indent-offset               4
        web-mode-style-padding                    4
        web-mode-script-padding                   4
        web-mode-block-padding                    4
        web-mode-enable-current-element-highlight t)

  (define-key web-mode-map (kbd "C-c v") 'browse-url-of-file)
  (set-face-attribute 'web-mode-current-element-highlight-face nil :background "#F6F192"))

(add-hook 'html-mode-hook 'enable-web-mode)
(add-hook 'css-mode-hook  'enable-emmet-mode)
(add-hook 'js2-mode-hook  'enable-emmet-mode)
(add-hook 'json-mode-hook 'enable-emmet-mode)
(add-hook 'web-mode-hook  'enable-emmet-mode)

;; YAML
(require-package 'yaml-mode)
(defun enable-yaml-mode ()
  (interactive)
  (require 'yaml-mode)
  (yaml-mode))

;; YASnippet
(require-package 'yasnippet)
(require-package 'popup)
(require 'yasnippet)
(setq yas-snippet-dirs (concat *PLUGINS* "/snippets"))
(yas-global-mode)
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))

(define-key yas-minor-mode-map (kbd "C-;") 'yas-expand)

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
     :isearch t)))

(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
  (let ((original-point (point)))
    (while (and
            (not (= (point) (point-min) ))
            (not (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))

;;-------------------------------------------------
;; Global Set Key
;;-------------------------------------------------

(dolist (key-list
         '(("C-x 2"   . custom-split-window-below)
           ("C-x 3"   . custom-split-window-right)
           ("C-c 4 r" . winner-redo)
           ("C-c 4 u" . winner-undo)

           ;; Auto complete with helm
           ("M-/" . ac-complete-with-helm)

           ;; Custom Feature
           ("C-c k"      . custom-delete-whitespace-to-upline)
           ("C-o"        . custom-down-newline)
           ("C-c d"      . custom-duplicate-line)
           ("C-c p"      . custom-git-push-current-buffer)
           ("C-M-\\"     . custom-indent-buffer)
           ("<C-return>" . custom-middle-newline)
           ("M-]"        . custom-move-down-current-line)
           ("M-["        . custom-move-up-current-line)
           ("C-c r"      . custom-resize-indentation)
           ("<backtab>"  . custom-resize-indentation--4)
           ("RET"        . custom-return)
           ("M-o"        . custom-up-newline)
           ("C-y"        . custom-yank)
           ("<left-margin> <mouse-1>" . custom-go-to-click-line)

           ;; Evil nerd commenter
           ("C-M-;"   . evilnc-comment-or-uncomment-lines)
           ("C-c c c" . evilnc-copy-and-comment-lines)
           ("C-c c p" . evilnc-comment-or-uncomment-paragraphs)

           ;; Helm
           ("C-c c b" . helm-buffers-list)
           ("C-c c f" . helm-find-files)
           ("M-x"     . helm-M-x)

           ;; Multiple cursors
           ("M-<down-mouse-1>" . nil)
           ("M-<mouse-1>"      . mc/add-cursor-on-click)
           ("C-S-c C-S-c"      . mc/edit-lines)

           ;; NEROTree
           ("<f1>" . neotree-toggle)

           ;; Origami mode
           ("<f2>"    . origami-toggle-node)
           ("C-c o a" . origami-show-only-node)
           ("C-c o o" . origami-open-node-recursively)
           ("C-c o n" . origami-next-fold)
           ("C-c o p" . origami-previous-fold)
           ("C-c o f" . origami-forward-fold-same-level)
           ("C-c o b" . origami-backward-fold-same-level)
           ("C-c o r" . origami-reset)

           ;; Tab bar mode
           ("C-M-=" . tabbar-press-home)
           ("C--"   . tabbar-backward)
           ("M--"   . tabbar-backward-group)
           ("C-="   . tabbar-forward)
           ("M-="   . tabbar-forward-group)

           ;; YASnippet mode
           ("<C-tab>" . yas-ido-expand)

           ;; 五笔输入法
           (";" . chinese-wbim-insert-ascii)))

  (global-set-key (kbd (car key-list)) (cdr key-list)))

;;; Major Mode
(setq auto-mode-alist
      (append '(("/[^\\./]*\\'"    . conf-mode) ; File name has no dot
                ("\\.bash"         . sh-mode)
                ("\\.yasnippet\\'" . snippet-mode)
                ("\\.ahk\\'"   . (lambda () (enable-ahk-mode)))
                ("\\.go\\'"    . (lambda () (enable-go-mode)))
                ("\\.js\\'"    . (lambda () (enable-js2-mode)))
                ("\\.json\\'"  . (lambda () (enable-json-mode)))
                ("\\.less\\'"  . (lambda () (enable-less-css-mode)))
                ("\\.md\\'"    . (lambda () (enable-markdown-mode)))
                ("\\.w?xml\\'" . (lambda () (enable-web-mode)))
                ("\\.php\\'"   . (lambda () (enable-web-mode)))
                ("\\.s[ac]ss"  . (lambda () (enable-scss-mode)))
                ("\\.vimrc\\'" . (lambda () (enable-vimrc-mode)))
                ("\\.ya?ml\\'" . (lambda () (enable-yaml-mode))))
              auto-mode-alist))

(provide 'extensions)
