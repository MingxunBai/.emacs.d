(ido-mode)
(setq ido-save-directory-list-file nil
      ido-enable-flex-matching t)       ; 模糊匹配

(global-auto-revert-mode)               ; Auto revert

(global-linum-mode)                     ; 显示行号

(recentf-mode)                          ; 历史记录

(scroll-bar-mode -1)                    ; 隐藏滚动条

(server-mode)                           ; 守护进程

(show-paren-mode)                       ; 高亮匹配括号

(tool-bar-mode -1)                      ; 隐藏工具栏

(winner-mode)                           ; 窗口控制

;; Input Method
(require-package 'chinese-wbim)
(autoload 'chinese-wbim-use-package "chinese-wbim" "Another emacs input method")
(register-input-method "chinese-wbim" "euc-cn" 'chinese-wbim-use-package "五笔" "汉字五笔输入法" (expand-file-name "plugins/wb.txt" user-emacs-directory))
(require 'chinese-wbim-extra)
(setq chinese-wbim-use-tooltip nil)

(set-input-method 'chinese-wbim)
(toggle-input-method)

;; Auto Complete
(require-package 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start t
      ac-auto-show-menu 0
      ac-ignore-case nil
      ac-use-menu-map t)

(define-key ac-mode-map (kbd "M-/") 'auto-complete)
(define-key ac-completing-map (kbd "M-/") 'ac-stop)

;; Emmet
(require-package 'emmet-mode)
(require 'emmet-mode)
(setq emmet-self-closing-tag-style ""
      emmet-move-cursor-between-quotes t)

(define-key emmet-mode-keymap (kbd "<C-return>") nil)
(define-key emmet-mode-keymap (kbd "C-M-[") 'emmet-prev-edit-point)
(define-key emmet-mode-keymap (kbd "C-M-]") 'emmet-next-edit-point)

;; Eshell
(add-hook 'eshell-exit-hook (lambda () (if (not (eq (count-windows) 1)) (delete-window))))
(defun custom-eshll ()
  (interactive)
  (if (condition-case nil
          (setq path (file-name-directory (buffer-file-name)))
        (error nil))
      (progn
        (custom-split-window 'eshell)
        (other-window 1)
        (eshell/cd path))
    (progn
      (custom-split-window 'eshell)
      (other-window 1))))

;; Evil Nerd Commenter
(require-package 'evil-nerd-commenter)
(require 'evil-nerd-commenter)

;; Helm
(require-package 'helm)
(require 'helm-config)

;; JSON
(require-package 'json-mode)
(require 'json-mode)

;; Lisp
(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "<f5>") 'eval-last-sexp)

;; Multiple Cursors
(require-package 'multiple-cursors)
(require 'multiple-cursors)

;; NEROTree
(require-package 'neotree)
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
(setq org-indent-mode t
      org-html-validation-link nil
      org-src-fontify-natively t
      org-log-done 'time)

;; Origami
(require-package 'origami)
(require 'origami)
(global-origami-mode)

;; Scheme
(setq scheme-program-name "scheme")
(add-hook 'scheme-mode-hook 'custom-init-scheme-mode)

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
  (define-key scheme-mode-map (kbd "<f5>") 'custom-scheme-send-definition))

;; Smart Parens
(require-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode)
(add-hook 'eshell-mode-hook 'smartparens-mode)

(provide 'init-mode)
