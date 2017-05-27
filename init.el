;;-------------------------------------------------
;; Basic setting
;;-------------------------------------------------

;; 设置个人信息
(setq user-full-name "MingxunBai"
      user-mail-address "mingxunbai@outlook.com")

;; 定义常量
(defconst *NO-WINDOW*                   ; 针对终端进行一些样式调整
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

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

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

;; 设置主题
(if *NO-WINDOW*
    (message "Terminal won't load theme.")
  (load-theme 'monokai t))

;; 设置字体
;; (set-default-font "Source Code Pro-12")
(when *WINDOWS*
  (set-fontset-font t 'han (font-spec :family "Minglan_Code")))

(defalias 'alr 'align-regexp)           ;;
(defalias 'cw  'compare-windows)        ; 设置别名
(defalias 'ff  'set-buffer-file-coding-system)
(defalias 'rs  'replace-string)         ;;

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

      eshell-prompt-function            ;;
      (lambda ()                        ; Eshell 提示符
        (concat
         (propertize (format-time-string "[%Y-%m-%d %H:%M] " (current-time)) 'face `(:foreground "#A6E22E"))
         (propertize (eshell/pwd) 'face `(:foreground "Pink"))
         (if (= (user-uid) 0) " # " " $ "))))

(fset 'yes-or-no-p 'y-or-n-p)           ; 使用 y/n 替代 yes/no, 使用 Enter 替代 y
(defun y-or-n-p-with-return (orig-func &rest args)
  (let ((query-replace-map (copy-keymap query-replace-map)))
    (define-key query-replace-map (kbd "RET") 'act)
    (apply orig-func args)))

(advice-add 'y-or-n-p :around #'y-or-n-p-with-return)

;;-------------------------------------------------
;; Internal mode
;;-------------------------------------------------

(global-auto-revert-mode)               ; Auto revert

(display-time)                          ; 显示时间

(if *NO-WINDOW*
    (message "Terminal won't hava scroll-bar and tool-bar.")
  (scroll-bar-mode -1)                  ; 隐藏滚动条
  (tool-bar-mode -1))                   ; 隐藏工具栏
(menu-bar-mode -1)                      ; 隐藏菜单栏

(show-paren-mode)                       ; 高亮匹配括号

(global-font-lock-mode)                 ; 语法高亮

(global-linum-mode)                     ; 显示行号

;; 格式化并高亮行号
(require 'hl-line)
(global-hl-line-mode)

(defface my-linum-hl
  `((t :inherit linum
       :background "#3C3D37"
       :foreground "#FFFFFF",(face-background 'hl-line nil t)))
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
(when *NO-WINDOW*
  (set-face-attribute hl-line-face nil :background "#E8E8FF"))

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

;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'company-dict)
(setq company-idle-delay 0
      company-dict-dir (concat *PLUGINS* "/company/dict"))
(add-to-list 'company-backends 'company-dict)

;; Dumb jump mode
(require 'dumb-jump)
(dumb-jump-mode)

;; Emacs lisp mode
(defun enable-emacs-lisp-mode ()
  (emacs-lisp-mode)
  (setq skeleton-pair-alist
        '((?\' "" >)
          (?\[ "" >))))

;; Emmet mode
(defun enable-emmet-mode ()
  (interactive)
  (require 'emmet-mode)
  (emmet-mode)

  (setq emmet-self-closing-tag-style ""
        emmet-move-cursor-between-quotes t)

  (define-key emmet-mode-keymap (kbd "<C-return>") nil)
  (define-key emmet-mode-keymap (kbd "C-M-[") 'emmet-prev-edit-point)
  (define-key emmet-mode-keymap (kbd "C-M-]") 'emmet-next-edit-point))

;; GoLang
(defun enable-go-mode ()
  (interactive)
  (require 'go-mode-autoloads)
  (go-mode)

  (defun go-save-fmt ()
    (interactive)
    (shell-command (concat "go fmt " (buffer-file-name))))

  (local-set-key (kbd "C-x f") 'go-save-fmt))

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

  (local-set-key (kbd "C-c f")   'js-load-file-and-go)
  (local-set-key (kbd "C-c b")   'js-send-buffer)
  (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go))

;; JAVA IDE mode
(defun enable-jdee-mode ()
  (interactive)
  (require 'jdee)
  (jdee-mode))

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

;; Origami mode
(require 'origami)
(global-origami-mode)

;; Paren face mode
(require 'paren-face)
(global-paren-face-mode)

;; Project explorer
(require 'project-explorer)
(defun pe/copy-relative-path ()
  (interactive)
  (pe/copy-file-name-as-kill)
  (other-window 1)
  (kill-new (file-relative-name (car kill-ring) (file-name-directory (buffer-file-name)))))

;; Scheme mode
(setq scheme-program-name "scheme")

(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))

(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))

(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(defun init-scheme-mode ()
  (require 'cmuscheme)

  (define-key scheme-mode-map (kbd "C-c C-k")    'nil)
  (define-key scheme-mode-map (kbd "<f5>")       'scheme-send-last-sexp-split-window)
  (define-key scheme-mode-map (kbd "<f6>")       'scheme-send-definition-split-window))

(add-hook 'scheme-mode-hook 'init-scheme-mode)
(add-hook 'inferior-scheme-mode-hook
          (lambda ()
            (define-key inferior-scheme-mode-map (kbd "C-c C-k") 'nil)))

;; SCSS mode
(defun enable-scss-mode ()
  (require 'scss-mode)
  (scss-mode)

  (setq scss-compile-at-save t))

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
        web-mode-code-indent-offset               4

        web-mode-style-padding                    4
        web-mode-script-padding                   4
        web-mode-block-padding                    4

        web-mode-enable-current-element-highlight t)

  (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-file)

  (set-face-attribute 'web-mode-current-element-highlight-face nil :background "#F6F192"))

;; Webkit mode
;; (require 'webkit)

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
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
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
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))

;;-------------------------------------------------
;; 主模式
;;-------------------------------------------------

(setq auto-mode-alist
      (append '(("/[^\\./]*\\'" .   conf-mode) ; File name has no dot

                ("\\.bash"      .   sh-mode)
                ("\\.css\\'"    .   (lambda () (enable-web-mode)))
                ("\\.el\\'"     .   (lambda () (enable-emacs-lisp-mode)))
                ("\\.go\\'"     .   (lambda () (enable-go-mode)))
                ("\\.java\\'"   .   (lambda () (enable-jdee-mode)))
                ("\\.js\\'"     .   (lambda () (enable-js2-mode)))
                ("\\.json\\'"   .   (lambda () (enable-json-mode)))
                ("\\.less\\'"   .   (lambda () (enable-less-css-mode)))
                ("\\.md\\'"     .   (lambda () (enable-markdown-mode)))
                ("\\.w?xml\\'"  .   (lambda () (enable-web-mode)))
                ("\\.php\\'"    .   (lambda () (enable-web-mode)))
                ("\\.s[ac]ss"   .   (lambda () (enable-scss-mode)))
                ("\\.vimrc\\'"  .   (lambda () (enable-vimrc-mode)))
                ("\\.wxss\\'"   .   css-mode)
                ("\\.ya?ml\\'"  .   (lambda () (enable-yaml-mode))))
              auto-mode-alist))

;;-------------------------------------------------
;; Custom feature
;;-------------------------------------------------

;; Before save hook
(defun custom-before-save-hook ()
  (delete-trailing-whitespace)
  (custom-ff-utf-8-unix))

;; File coding system use utf-8-unix
(defun custom-ff-utf-8-unix ()
  (if (not (string-match "utf-8-unix" (symbol-name buffer-file-coding-system)))
      (set-buffer-file-coding-system 'utf-8-unix)
    (message "It's a unix file.")))

;; Git
(defun find-git-repo (dir)
  "Find base git directory"
  (if (or (string= "/" dir)
          (string= "c:/" dir)
          (string= "d:/" dir)
          (string= "e:/" dir)
          (string= "f:/" dir))
      (message "It's not a git repo.")
    (if (file-exists-p (expand-file-name ".git/" dir))
        dir
      (find-git-repo (expand-file-name "../" dir)))))

(defun git-push (root)
  (shell-command (concat "cd " root " && git add -A"))
  (shell-command (concat "cd " root " && git commit -m 'Update'"))
  (shell-command (concat "cd " root " && git push")))

(defun git-push-current-buffer ()
  (interactive)
  (let ((root (find-git-repo default-directory)))
    (git-push root)))

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
  (when (eq major-mode 'scheme-mode)
    (custom-lisp-paren-return))
  (if (or (and (string-equal "{" (string (char-before (point))))
               (string-equal "}" (string (char-after  (point)))))
          (and (string-equal "[" (string (char-before (point))))
               (string-equal "]" (string (char-after  (point)))))
          (and (string-equal "(" (string (char-before (point))))
               (string-equal ")" (string (char-after  (point))))))
      (custom-middle-newline)
    (newline-and-indent)))

;; Lisp 括号换行
(defun custom-lisp-paren-return ()
  (progn
    (newline-and-indent)
    (previous-line)
    (end-of-line)))

;; 标签内新建一行
(defun custom-middle-newline ()
  (interactive)
  (progn
    (newline-and-indent)
    (newline-and-indent)
    (previous-line)
    (indent-according-to-mode)))

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

;; 删除 buffer
(defun custom-ido-kill-buffer ()
  (interactive)
  (ido-kill-buffer)
  (delete-window))

;; 粘贴
(defun custom-yank ()
  (interactive)
  (let ((point-before (point)))
    (yank)
    (indent-region point-before (point))))

;; 移动当前行
(defun not-whole-line ()
  (end-of-line)
  (if (eobp) 't))

(defun remeber-cols ()
  (setq cols (point))
  (beginning-of-line)
  (setq step (- cols (point))))

(defun custom-move-current-line (n)
  (progn
    (kill-new "")
    (beginning-of-line)
    (kill-whole-line)
    (forward-line n)
    (custom-yank)
    (forward-line -1)
    (beginning-of-line)
    (forward-char step)
    (indent-according-to-mode)))

;; 上移一行
(defun custom-move-up-current-line ()
  (interactive)
  (remeber-cols)
  (progn
    (beginning-of-line)
    (if (bobp)
        (progn
          (message "Beginning of buffer!")
          (forward-char step))
      (progn
        (if (eq (not-whole-line) 't)
            (progn
              (newline)
              (forward-line -1)
              (custom-move-current-line -1))
          (custom-move-current-line -1))))))

;; 下移一行
(defun custom-move-down-current-line ()
  (interactive)
  (remeber-cols)
  (progn
    (end-of-line)
    (if (eobp)
        (progn
          (message "End of buffer!")
          (beginning-of-line)
          (forward-char step))
      (progn
        (custom-move-current-line 1)))))

;;-------------------------------------------------
;; Hook
;;-------------------------------------------------

(add-hook 'before-save-hook 'custom-before-save-hook)

;; Org mode
(add-hook 'org-mode-hook 'custom-org-mode-hook)
(defun custom-org-mode-hook ()
  (org-indent-mode)
  (lazy-unset-key '("C-c C-k") org-mode-map)
  (setq org-startup-indented t)

  ;; 生成 html 文件时代码高亮
  (require 'htmlize)
  (setq org-src-fontify-natively t)

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

;; Python mode
(add-hook 'python-mode-hook 'custom-python-mode-hook)
(defun custom-python-mode-hook ()
  (lazy-unset-key '("<backtab>") python-mode-map)

  (require 'elpy)
  (elpy-mode)

  ;; (highlight-indent-guides-mode)

  (require 'py-autopep8)
  (py-autopep8-enable-on-save)

  (setq python-shell-prompt-detect-enabled nil))

;; Text mode
(when *WINDOWS*
  (add-hook 'text-mode-hook 'init-wb-dict-gitrepo))

(defun init-wb-dict-gitrepo ()
  (add-hook 'kill-emacs-query-functions 'wb-dict-git-push)
  (defun wb-dict-git-push ()
    (interactive)
    (if (equal (buffer-name) "userdefinephrase.dat")
        (let* ((wb-dict-root (concat (getenv "ToolsHome") "\\BingWuBiDict")))
          (progn
            (shell-command (concat "cp -f '" (buffer-file-name) "' " wb-dict-root "'"))
            (git-push wb-dict-root)))
      (message "Exiting."))))

;; Web mode
(add-hook 'html-mode-hook 'enable-web-mode)

(add-hook 'css-mode-hook  'enable-emmet-mode)
(add-hook 'js2-mode-hook  'enable-emmet-mode)
(add-hook 'json-mode-hook 'enable-emmet-mode)
(add-hook 'web-mode-hook  'enable-emmet-mode)

;; 最大化
(custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))