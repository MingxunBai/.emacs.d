;; extensions.el --- full emacs initial file

;; Copyright (C) 2017 MingxunBai

;; Author: MingxunBai
;; Keywords: convenience
;; Version: 1.0

;;; Code:

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
    (if (equal 0 (string-match "\s*/\\*+" (thing-at-point 'line t)))
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
  (defun custom-javadoc-newline ()
    (interactive)
    (newline-and-indent)
    (insert "*")
    (newline-and-indent)
    (insert "*/")
    (previous-line)
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
(defun enable-ahk-mode ()
  (interactive)
  (require 'xahk-mode)
  (ahk-mode))

;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil
      ac-ignore-case nil
      ac-use-menu-map t)

(define-key ac-mode-map (kbd "M-/") 'auto-complete)
(define-key ac-complete-mode-map (kbd "M-/") 'ac-stop)

;; Dired
(add-hook 'dired-load-hook
          '(lambda ()
             (define-key dired-mode-map (kbd "j") 'dired-next-line)
             (define-key dired-mode-map (kbd "k") 'dired-previous-line)))

;; Emmet
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
        (eshell/cd path)))
  (message "Eshell need a local file!"))

;; Evil Nerd Commenter
(require 'evil-nerd-commenter)

;; GoLang
(defun enable-go-mode ()
  (interactive)
  (require 'go-mode-autoloads)
  (go-mode)
  (defun custom-go-save-fmt ()
    (interactive)
    (shell-command (concat "go fmt " (buffer-file-name))))

  (define-key go-mode-map (kbd "C-c f") 'custom-go-save-fmt))

;; History
(require 'history)

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
(defun enable-json-mode ()
  (interactive)
  (require 'json-mode)
  (json-mode))

;; Less
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
(defun enable-markdown-mode ()
  (interactive)
  (require 'markdown-mode)
  (markdown-mode)
  (when *WINDOWS*
    (custom-set-variables '(markdown-command "markdown.pl")))

  (define-key markdown-mode-map (kbd "C-c C-k") nil))

;; Multiple Cursors
(require 'multiple-cursors)
()

;; NEROTree
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
(require 'origami)
(global-origami-mode)

;; Python
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
(defun enable-scss-mode ()
  (require 'scss-mode)
  (scss-mode)
  (setq scss-compile-at-save t))

;; Smart Parens
(require 'smartparens-config)
(smartparens-global-mode)
(add-hook 'eshell-mode-hook 'smartparens-mode)

;; Tab Bar
(require 'tabbar)
(setq tabbar-use-images nil)
(tabbar-mode)

;; Tramp
(setq tramp-default-host "45.78.52.152#29135"
      tramp-default-user "root")

;; Vimrc
(defun enable-vimrc-mode ()
  (interactive)
  (require 'vimrc-mode)
  (vimrc-mode))

;; Web
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
(defun enable-yaml-mode ()
  (interactive)
  (require 'yaml-mode)
  (yaml-mode))

;; YASnippet
(require 'yasnippet)
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
      (append '(("\\.ahk\\'"   . (lambda () (enable-ahk-mode)))
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
