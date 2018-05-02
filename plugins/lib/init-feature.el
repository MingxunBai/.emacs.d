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

(provide 'init-feature)
