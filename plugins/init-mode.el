;;; init-mode.el --- modes

;;; Commentary:

;;; Code:

(ido-mode t)
(setq ido-save-directory-list-file nil
      ido-enable-flex-matching t)       ; 模糊匹配

(global-auto-revert-mode)               ; Auto revert

(recentf-mode)                          ; 历史记录

(show-paren-mode)                       ; 高亮匹配括号

(winner-mode)                           ; 窗口控制

;; Input Method
(require-package 'chinese-wbim)
(autoload 'chinese-wbim-use-package "chinese-wbim" "Emacs input method")
(register-input-method "chinese-wbim" "euc-cn" 'chinese-wbim-use-package "WuBi " "汉字五笔输入法" (expand-file-name "plugins/wb.txt" user-emacs-directory))
(require 'chinese-wbim-extra)
(setq chinese-wbim-use-tooltip nil)

(set-input-method 'chinese-wbim)
(toggle-input-method)

;; Auto Complete
;; (require-package 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-auto-start t
;;       ac-auto-show-menu 0
;;       ac-ignore-case nil
;;       ac-use-menu-map t)
;; (add-to-list 'ac-modes 'bat-mode)
;; (add-to-list 'ac-modes 'json-mode)

;; (define-key ac-mode-map (kbd "M-/") 'auto-complete)
;; (define-key ac-completing-map (kbd "M-/") 'ac-stop)

;; Company
(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0
      company-echo-delay 0
      company-minimum-prefix-length 1
      company-tooltip-limit 10
      company-tooltip-align-annotations t)

(define-key company-mode-map (kbd "M-/") #'company-complete)

;; Emmet
(require-package 'emmet-mode)
(defun emmet-mode-on-hook ()
  "Emmet mode hook."
  (setq emmet-self-closing-tag-style " /"
        emmet-move-cursor-between-quotes t)

  (define-key emmet-mode-keymap (kbd "<C-return>") nil)
  (define-key emmet-mode-keymap (kbd "C-M-[") 'emmet-prev-edit-point)
  (define-key emmet-mode-keymap (kbd "C-M-]") 'emmet-next-edit-point)
  (define-key emmet-mode-keymap (kbd "C-c w") 'emmet-wrap-with-markup))

(add-hook 'emmet-mode-hook #'emmet-mode-on-hook)
(add-hook 'web-mode-hook 'emmet-mode)

;; Eshell
(add-hook 'eshell-exit-hook (lambda () (if (not (eq (count-windows) 1)) (delete-window))))
(defun custom-eshll ()
  "Eshell."
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
      (other-window 1)))
  (tabbar-local-mode)
  (company-mode -1))

;; Evil Nerd Commenter
(require-package 'evil-nerd-commenter)

;; Flycheck
(require-package 'flycheck)
(global-flycheck-mode)

;; Helm
(require-package 'helm)

;; Hunger Delete
(require-package 'hungry-delete)
(global-hungry-delete-mode)

;; JSON
(require-package 'json-mode)

;; Lisp
(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "<f5>") 'eval-last-sexp)

;; Multiple Cursors
(require-package 'multiple-cursors)

;; NEOTree
(require-package 'neotree)
(setq neo-theme 'ascii)

(defun custom-copy-relative-path ()
  "Copy relative path."
  (interactive)
  (neotree-copy-filepath-to-yank-ring)
  (other-window 1)
  (kill-new (file-relative-name (car kill-ring) (file-name-directory (buffer-file-name)))))

(defun neotree-mode-on-hook ()
  "Neotree mode hook."
  (define-key neotree-mode-map (kbd "j") 'neotree-next-line)
  (define-key neotree-mode-map (kbd "k") 'neotree-previous-line)
  (define-key neotree-mode-map (kbd "]") 'neotree-select-next-sibling-node)
  (define-key neotree-mode-map (kbd "[") 'neotree-select-previous-sibling-node)
  (define-key neotree-mode-map (kbd "C-c c") 'custom-copy-relative-path))

(add-hook 'neotree-mode-hook #'neotree-mode-on-hook)

;; Org
(setq org-startup-indented t
      org-html-validation-link nil
      org-src-fontify-natively t
      org-log-done 'time
      org-agenda-custom-commands
      '(("d" "Agenda for current week" agenda "DONE"
         ((org-agenda-time-grid nil)
          (org-agenda-span 'week)
          (org-agenda-entry-types '(:closed))))))

;; Origami
(require-package 'origami)
(global-origami-mode)

;; PATH
(require-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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

;; Server Mode
(require-package 'server)
(unless (server-running-p) (server-mode))

;; Smart Parens
(require-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode)
(add-hook 'eshell-mode-hook 'smartparens-mode)

;; TIDE
(require-package 'tide)
(defun setup-tide-mode ()
  (tide-setup)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode)
  (setq-default typescript-indent-level 2))

(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Web
(require-package 'web-mode)
(defun web-mode-on-hook ()
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2)
  (flycheck-add-mode 'html-tidy 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode)
  (define-key web-mode-map (kbd "C-c v") 'browse-url-of-file))

;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-hook 'web-mode-hook #'web-mode-on-hook)

;; Window Numbering
(require-package 'window-numbering)
(window-numbering-mode)

(provide 'init-mode)

;;; init-mode.el ends here
