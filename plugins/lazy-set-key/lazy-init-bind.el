;;; lazy-init-bind.el --- lazy-set-key with light features

;; Copyright (C) 2016 MingxunBai

;; Author: MingxunBai
;; Keywords: convenience
;; Version: 3.0

;; This is custom key bind use lazy-set-key.

;;; Code:
(lazy-unset-key '("M-<down-mouse-1>"))

(lazy-set-key '(("C-c f"        .   full)
                ("C-c C-k"      .   kill-buffer-and-window)

                ;; Custom feature
                ("C-c k"        .   custom-delete-whitespace-to-upline)
                ("C-o"          .   custom-down-newline)
                ("C-c d"      .   custom-duplicate-line)
                ("C-M-\\"       .   custom-indent-buffer)
                ("<C-return>"   .   custom-middle-newline)
                ("M-]"          .   custom-move-down-current-line)
                ("M-["          .   custom-move-up-current-line)
                ("C-c r"        .   custom-resize-indentation)
                ("<backtab>"    .   custom-resize-indentation--4)
                ("RET"          .   custom-return)
                ("C-x 2"        .   custom-split-window-below)
                ("C-x 3"        .   custom-split-window-right)
                ("M-o"          .   custom-up-newline)
                ("C-y"          .   custom-yank)
                ("<left-margin> <mouse-1>" . custom-go-to-click-line)

                ;; Multiple cursors
                ("M-<mouse-1>"  .   mc/add-cursor-on-click)
                ("C-S-c C-S-c"  .   mc/edit-lines)

                ;; Tab bar mode
                ("C-M-="        .   tabbar-press-home)
                ("C--"          .   tabbar-backward)
                ("M--"          .   tabbar-backward-group)
                ("C-="          .   tabbar-forward)
                ("M-="          .   tabbar-forward-group)))

(provide 'lazy-init-bind)
