;;; custom-function.el --- custom function

;; Copyright (C) 2016 MingxunBai

;; Author: MingxunBai
;; Keywords: convenience
;; Version: 1.0

;; This is some custom function

;;; Code:

;;; 自定缩进
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

;;; 删除空白字符至上一行末尾
(defun custom-delete-whitespace-to-upline ()
  (interactive)
  (progn
    (delete-indentation)
    (indent-according-to-mode)))

;;; 向上新建一行
(defun custom-up-newline ()
  (interactive)
  (progn
    (beginning-of-line)
    (newline-and-indent)
    (previous-line)
    (indent-according-to-mode)))

;;; 向下新建一行
(defun custom-down-newline ()
  (interactive)
  (progn
    (end-of-line)
    (newline-and-indent)))

;;; 标签内新建一行
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

;;; 移动当前行
(defun not-whole-line ()
  (end-of-line)
  (if (eobp) 't))

(defun remember-pos ()
  (setq currpos (point))
  (beginning-of-line)
  (setq step (- currpos (point))))

(defun custom-move-current-line (n)
  (interactive)
  (progn
    (beginning-of-line)
    (kill-whole-line)
    (forward-line n)
    (yank)
    (forward-line -1)
    (beginning-of-line)
    (forward-char step)
    (indent-according-to-mode)))

;; 上移一行
(defun custom-move-up-current-line ()
  (interactive)
  (remember-pos)
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
          (custom-move-current-line -1))
        (message "Move up current line.")))))

;;; 下移一行
(defun custom-move-down-current-line ()
  (interactive)
  (remember-pos)
  (progn
    (end-of-line)
    (if (eobp)
        (progn
          (message "End of buffer!")
          (beginning-of-line)
          (forward-char step))
      (progn
        (custom-move-current-line 1)
        (message "Move down current line.")))))

;;; 粘贴
(defun custom-yank ()
  (interactive)
  (if (equal (car kill-ring) nil)
      (yank)
    (progn
      (if (string= (substring (car kill-ring) -1) "
")
          (progn
            (yank)
            (indent-according-to-mode)
            (forward-line -1)
            (indent-according-to-mode)
            (end-of-line))
        (yank)))))

;;; 显示主模式
(defun custom-show-major-mode ()
  (interactive)
  (message "%s" major-mode))

;;; Project explorer mode
(defun pe/copy-relative-path ()
  (interactive)
  (pe/copy-file-name-as-kill)
  (other-window 1)
  (kill-new (file-relative-name (car kill-ring) (file-name-directory (buffer-file-name)))))

;;; Org mode
(defun custom-org-mode-hook ()
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
      (org-edit-src-code))))

;;; Python mode
(defun custom-python-mode-hook ()
  (lazy-unset-key '("<backtab>") python-mode-map)

  (require 'elpy)
  (elpy-mode)

  ;; (highlight-indent-guides-mode)

  (require 'py-autopep8)
  (py-autopep8-enable-on-save)

  (setq python-shell-prompt-detect-enabled nil))

;;; YASnippet
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
     ;; start isearch mode immediately
     :isearch t
     )))

;; completing point by some yasnippet key
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

;;; Codes end
(provide 'custom-function)
