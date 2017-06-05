;;; lazy-key-bind.el --- lazy-set-key configuation

;; Copyright (C) 2016 MingxunBai

;; Author: MingxunBai
;; Keywords: convenience
;; Version: 1.0

;; This is custom key bind use lazy-set-key.

;;; Code:
(lazy-unset-key '("M-<down-mouse-1>"))

(lazy-set-key '(("C-c C-k"      .   kill-buffer-and-window)

                ;; Custom feature
                ("C-c k"        .   custom-delete-whitespace-to-upline)
                ("C-o"          .   custom-down-newline)
                ("C-x k"        .   custom-ido-kill-buffer)
                ("C-M-\\"       .   custom-indent-buffer)
                ("<C-return>"   .   custom-middle-newline)
                ("<left-margin> <down-mouse-1>" . custom-md-select-linum)
                ("<left-margin> <drag-mouse-1>" . custom-mu-select-linum)
                ("<left-margin> <mouse-1>"      . custom-mu-select-linum)
                ("M-]"          .   custom-move-down-current-line)
                ("M-["          .   custom-move-up-current-line)
                ("C-c r"        .   custom-resize-indentation)
                ("<backtab>"    .   custom-resize-indentation--4)
                ("RET"          .   custom-return)
                ("C-x 2"        .   custom-split-window-below)
                ("C-x 3"        .   custom-split-window-right)
                ("M-o"          .   custom-up-newline)
                ("C-y"          .   custom-yank)


                ;; Git
                ("C-x p"        .   git-push-current-buffer)

                ;; Multiple cursors
                ("M-<mouse-1>"  .   mc/add-cursor-on-click)
                ("C-S-c C-S-c"  .   mc/edit-lines)

                ;; Origami mode
                ("<f2>"         .   origami-toggle-node)
                ("C-c o a"      .   origami-show-only-node)
                ("C-c o o"      .   origami-open-node-recursively)
                ("C-c o n"      .   origami-next-fold)
                ("C-c o p"      .   origami-previous-fold)
                ("C-c o f"      .   origami-forward-fold-same-level)
                ("C-c o b"      .   origami-backward-fold-same-level)
                ("C-c o r"      .   origami-reset)

                ;; Project explorer mode
                ("<f1>"         .   project-explorer-toggle)

                ;; Tab bar mode
                ("C-M-="        .   tabbar-press-home)
                ("C--"          .   tabbar-backward)
                ("M--"          .   tabbar-backward-group)
                ("C-="          .   tabbar-forward)
                ("M-="          .   tabbar-forward-group)

                ;; Winner mode
                ("C-x 4 r"      .   winner-redo)
                ("C-x 4 u"      .   winner-undo)

                ;; YASnippet mode
                ("<C-tab>"      .   yas-ido-expand)

                ;; 五笔输入法
                (";"            .   chinese-wbim-insert-ascii)))

(provide 'lazy-key-bind)
