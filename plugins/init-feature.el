;;; init-feature.el --- custom feature

;;; Commentary:

;;; Code:

;; Save hook
(defun custom-before-save-hook ()
  "Before save hook."
  (delete-trailing-whitespace)
  (custom-ff-utf-8-unix))

(defun custom-ff-utf-8-unix ()
  "UTF8 format."
  (if (not
       (string-match "utf-8-unix" (symbol-name buffer-file-coding-system)))
      (set-buffer-file-coding-system 'utf-8-unix)))

(add-hook 'before-save-hook 'custom-before-save-hook)

;; Command 分离窗口运行
(defun custom-split-window (command &optional param)
  "Run COMMAND with PARAM in split window."
  (delete-other-windows)
  (if (> (count-windows) 1)
      (neotree-toggle))
  (split-window-vertically (floor (* 0.68 (window-height))))
  (other-window 1)
  (funcall command param)
  (other-window 1))

;; Find file or dir in current diskpart
(defun custom-find-dir (dir reg)
  "Find file via REG in DIR."
  (interactive)
  (if (not (string-match "[a-z0-9_-]/" dir))
      nil
    (if (file-exists-p (expand-file-name reg dir))
        dir
      (custom-find-dir (expand-file-name "../" dir) reg))))

;; 移动光标至行首或非空字符处
(defun custom-move-beginning-of-line ()
  "Move caret beginning of line or indentation of line."
  (interactive)
  (let ((pos (point)))
    (back-to-indentation)
    (if (eq pos (point))
        (move-beginning-of-line nil))))

;; 缩进重排
(defun custom-remeber-line ()
  "Remeber line."
  (if (bolp)
      (1+ (count-lines 1 (point)))
    (count-lines 1 (point))))

(defun custom-indent-buffer ()
  "Indent buffer."
  (interactive)
  (let ((line (custom-remeber-line))
        (step (custom-remeber-point-step)))
    (indent-region (point-min) (point-max))
    (goto-line line)
    (back-to-indentation)
    (forward-char step)))

;; 自定缩进
(defun custom-resize-indentation (n)
  "Indent N."
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
  "Indent back."
  (interactive)
  (custom-resize-indentation -4))

;; 删除空白字符至上一行末尾
(defun custom-delete-whitespace-to-upline ()
  "Delete whitespace to upline."
  (interactive)
  (progn
    (delete-indentation)
    (indent-according-to-mode)))

;; 行号点击
(defun custom-go-to-click-line ()
  "Goto click line."
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
  "Newline previous."
  (interactive)
  (progn
    (beginning-of-line)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

;; 向下新建一行
(defun custom-down-newline ()
  "Newline."
  (interactive)
  (progn
    (end-of-line)
    (newline-and-indent)))

;; 换行
(defun custom-return ()
  "Return."
  (interactive)
  (cond ((or (bobp) (eobp))
         (newline-and-indent))
        ((custom-is-in-paren?)
         (custom-middle-newline))
        (t (newline-and-indent))))

;; 匹配光标上下文
(defun custom-paren-match (bef end)
  "Paren match BEF END."
  (if (and (string-match bef (string (preceding-char)))
           (string-match end (string (following-char))))
      't
    nil))

(defun custom-is-in-paren? ()
  "Is in paren?"
  (if (or (custom-paren-match "{" "}")
          (custom-paren-match "\\[" "\\]")
          (custom-paren-match "(" ")")
          (custom-paren-match ">" "<"))
      't
    nil))

;; 标点中新建一行
(defun custom-middle-newline ()
  "Middle newline."
  (interactive)
  (newline-and-indent)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; 在右侧新建一个窗口
(defun custom-split-window-right ()
  "Split window right."
  (interactive)
  (split-window-right)
  (other-window 1))

;; 在下方新建一个窗口
(defun custom-split-window-below ()
  "Split window below."
  (interactive)
  (split-window-below)
  (other-window 1))

;; 粘贴
(defun custom-yank ()
  "Yank."
  (interactive)
  (let ((point-before (point)))
    (yank)
    (indent-region point-before (point))))

;; 移动当前行
(defun custom-move-current-line (line caret)
  "Move caret to (LINE, CARET) ."
  (kill-new "")
  (beginning-of-line)
  (kill-whole-line)
  (forward-line line)
  (yank)
  (forward-line -1)
  (back-to-indentation)
  (forward-char caret)
  (indent-according-to-mode))

(defun custom-remeber-point-step ()
  "Remeber point step."
  (let ((bef (point)))
    (back-to-indentation)
    (let ((step (- bef (point))))
      (if (< step 0)
          0
        step))))

(defun custom-repos (msg n)
  "Forward N chars with MSG."
  (message msg)
  (back-to-indentation)
  (forward-char n))

(defun custom-forward-line-end (n)
  "Forward N lines."
  (forward-line n)
  (end-of-line))

(defun custom-bobp? ()
  "Bobp?"
  (beginning-of-line)
  (if (bobp)
      't
    nil))

(defun custom-eobp? (n)
  "Eobp after N lines?"
  (custom-forward-line-end n)
  (let ((rst (eobp)))
    (custom-forward-line-end (- 0 n))
    rst))

(defun custom-next-line-empty? ()
  "Next line empty?"
  (forward-line 1)
  (let ((rst (eq (point-at-bol) (point-at-eol))))
    (forward-line -1)
    rst))

;; 上移一行
(defun custom-move-up-current-line ()
  "Move line up."
  (interactive)
  (let ((step (custom-remeber-point-step)))
    (cond ((custom-bobp?)
           (custom-repos "Beginning of buffer!" step))
          ((custom-eobp? 0)
           (newline)
           (forward-line -1)
           (custom-move-current-line -1 step))
          (t (custom-move-current-line -1 step)))))

;; 下移一行
(defun custom-move-down-current-line ()
  "Move line down."
  (interactive)
  (let ((step (custom-remeber-point-step)))
    (cond ((custom-eobp? 0)
           (custom-repos "End of buffer!" step))
          ((and (custom-next-line-empty?) (custom-eobp? 1))
           (custom-repos "End of buffer!" step))
          ((and (not (custom-next-line-empty?)) (custom-eobp? 1))
           (custom-forward-line-end 1)
           (newline)
           (forward-line -2)
           (custom-move-current-line 1 step))
          (t (custom-move-current-line 1 step)))))

;; 复制当前行
(defun custom-duplicate-line ()
  "Duplicate line."
  (interactive)
  (let ((step (custom-remeber-point-step)))
    (kill-ring-save (point-at-bol) (point-at-eol))
    (custom-down-newline)
    (custom-yank)
    (back-to-indentation)
    (forward-char step)))

(provide 'init-feature)

;;; init-feature.el ends here
