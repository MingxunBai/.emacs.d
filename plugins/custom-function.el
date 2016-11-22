;;; custom-function.el --- custom function

;; Copyright (C) 2016 MingxunBai

;; Author: MingxunBai
;; Keywords: convenience
;; Version: 1.0

;; This is some custom function

;;; Code:

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

;; 标签内新建一行
(defun custom-middle-newline ()
  (interactive)
  (progn
	(newline-and-indent)
	(newline-and-indent)
	(previous-line)
	(indent-according-to-mode)))

;; 在右侧新建一个窗口
(defun custom-new-right-window ()
  (interactive)
  (split-window-right)
  (other-window 1))

;; 在下方新建一个窗口
(defun custom-new-below-window ()
  (interactive)
  (split-window-below)
  (other-window 1))

;; 移动当前行
(defun is-whole-line ()
  (end-of-line)
  (if (eobp) 'true))

(defun remember-pos ()
  (set 'currpos (point))
  (beginning-of-line)
  (set 'step (- currpos (point))))

(defun custom-move-current-line (n)
  (interactive)
  (progn
	(beginning-of-line)
	(kill-whole-line)
	(forward-line n)
	(custom-yank)
	(forward-char step)))

;; 上移一行
(defun custom-move-up-current-line ()
  (interactive)
  (remember-pos)
  (progn
	(beginning-of-line)
	(if (bobp)
		(progn
		  (message "Beginning of buffer now!")
		  (forward-char step))
	  (progn
		(if (eq (is-whole-line) 'true)
			(progn
			  (newline)
			  (forward-line -1)
			  (custom-move-current-line -1))
		  (custom-move-current-line -1))))))

;; 下移一行
(defun custom-move-down-current-line ()
  (interactive)
  (remember-pos)
  (progn
	(end-of-line)
	(if (eobp)
		(progn
		  (message "End of buffer now!")
		  (beginning-of-line)
		  (forward-char step))
	  (progn
		(if (eq (is-whole-line) 'true)
			(progn
			  (newline)
			  (forward-line)
			  (custom-move-current-line 1))
		  (custom-move-current-line 1))))))


;; 粘贴
(defun custom-yank ()
  (interactive)
  (yank)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; 显示主模式
(defun custom-show-major-mode ()
  (interactive)
  (message "%s" major-mode))

(provide 'custom-function)
