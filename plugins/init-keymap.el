;;; init-keymap.el --- Custom keymap

;;; Commentary:

;;; Code:

(dolist (key-map
         '(
           ;; System

           ;; Winner mode
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
           ("C-x 2"      . custom-split-window-below)
           ("C-x 3"      . custom-split-window-right)
           ("M-o"        . custom-up-newline)
           ("C-y"        . custom-yank)
           ("<left-margin> <mouse-1>" . custom-go-to-click-line)

           ;; Evil nerd commenter
           ("C-M-;"   . evilnc-comment-or-uncomment-lines)
           ("C-c c c" . evilnc-copy-and-comment-lines)
           ("C-c c p" . evilnc-comment-or-uncomment-paragraphs)

           ;; Helm
           ("C-c c b" . helm-buffers-list)
           ("C-c c f" . helm-find-files)
           ("M-x"     . helm-M-x)

           ;; Multiple cursors
           ("M-<down-mouse-1>" . nil)
           ("M-<mouse-1>"      . mc/add-cursor-on-click)
           ("C-S-c C-S-c"      . mc/edit-lines)

           ;; NEROTree
           ("<f1>" . neotree-toggle)

           ;; Origami mode
           ("<f2>"    . origami-toggle-node)
           ("C-c o a" . origami-show-only-node)
           ("C-c o r" . origami-reset)))

  (global-set-key (kbd (car key-map)) (cdr key-map)))

(provide 'init-keymap)

;;; init-keymap.el ends here
