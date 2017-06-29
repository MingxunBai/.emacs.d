;;; lazy-full-bind.el --- lazy-set-key with full features

;; Copyright (C) 2017 MingxunBai

;; Author: MingxunBai
;; Keywords: convenience
;; Version: 1.0

;; This is custom key bind use lazy-set-key.

;;; Code:
(lazy-set-key '(("C-c p"        .   custom-git-push-current-buffer)

                ;; Evil nerd commenter
                ("C-M-;"        .   evilnc-comment-or-uncomment-lines)
                ("C-c c c"      .   evilnc-copy-and-comment-lines)
                ("C-c c p"      .   evilnc-comment-or-uncomment-paragraphs)

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

                ;; Winner mode
                ("C-c 4 r"      .   winner-redo)
                ("C-c 4 u"      .   winner-undo)

                ;; YASnippet mode
                ("<C-tab>"      .   yas-ido-expand)

                ;; 五笔输入法
                (";"            .   chinese-wbim-insert-ascii)))

(provide 'lazy-full-bind)