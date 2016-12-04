;;; lazy-key-bind.el --- lazy-set-key configuation

;; Copyright (C) 2016 MingxunBai

;; Author: MingxunBai
;; Keywords: convenience
;; Version: 1.0

;; This is custom key bind use lazy-set-key.

;;; Code:

(lazy-set-key '(("RET" . newline-and-indent)
				("C-c C-k" . kill-buffer-and-window)

				("(" . skeleton-pair-insert-maybe)
				("[" . skeleton-pair-insert-maybe)
				("{" . skeleton-pair-insert-maybe)
				("<" . skeleton-pair-insert-maybe)
				("\"" . skeleton-pair-insert-maybe)
				("\'" . skeleton-pair-insert-maybe)

				;; Auto complete mode
				("C-." . auto-complete)
				("M-/" . ac-stop)

				;; Emmet mode
				("C-M-[" . emmet-prev-edit-point)
				("C-M-]" . emmet-next-edit-point)

				;; Multiple cursors
				("C-S-c C-S-c" . mc/edit-lines)
				("M-<mouse-1>" . mc/add-cursor-on-click)

				;; Project explorer
				("<f1>" . project-explorer-toggle)

				;; Org mode
				("C-c c e" . org-edit-src-code)
				("C-c c i" . org-insert-src-block)

				;; Origami mode
				("<f2>" . origami-toggle-node)
				("C-c o a" . origami-show-only-node)
				("C-c o o" . origami-open-node-recursively)
				("C-c o n" . origami-next-fold)
				("C-c o p" . origami-previous-fold)
				("C-c o f" . origami-forward-fold-same-level)
				("C-c o b" . origami-backward-fold-same-level)
				("C-c o r" . origami-reset)

				;; Web mode
				("C-c C-v" . browse-url-of-file)

				;; Winner mode
				("C-x 4 u" . winner-undo)
				("C-x 4 r" . winner-redo)

				;; YASnippet mode
				("<C-tab>" . yas-ido-expand)

				;; Custom function
				("C-x 3" . custom-new-right-window)
				("C-x 2" . custom-new-below-window)
                ("C-o" . custom-down-newline)
                ("M-o" . custom-up-newline)
				("<C-return>" . custom-middle-newline)
				("M-[" . custom-move-up-current-line)
				("M-]" . custom-move-down-current-line)
                ("C-c k" . custom-delete-whitespace-to-upline)
				("C-c r" . custom-resize-indentation)
                ("<backtab>" . custom-resize-indentation--4)
				("C-M-y" . custom-yank)

				;; 五笔输入法
				(";" . chinese-wbim-insert-ascii)))

(provide 'lazy-key-bind)
