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
(require 'chinese-wbim-extra)
(autoload 'chinese-wbim-use-package "chinese-wbim" "Another emacs input method")
(register-input-method "chinese-wbim" "euc-cn" 'chinese-wbim-use-package "五笔" "汉字五笔输入法" "wb.txt")
(setq chinese-wbim-use-tooltip nil)

(set-input-method 'chinese-wbim)
(toggle-input-method)

;; Auto Complete
(require-package 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil
      ac-ignore-case nil
      ac-use-menu-map t)

(define-key ac-mode-map (kbd "M-;") 'auto-complete)
(define-key ac-completing-map (kbd "M-;") 'ac-stop)

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

;; Lisp
(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "<f5>") 'eval-last-sexp)

;; Markdown
(require-package 'markdown-mode)
(defun enable-markdown-mode ()
  (interactive)
  (require 'markdown-mode)
  (markdown-mode)
  (when *WINDOWS*
    (custom-set-variables '(markdown-command "markdown.pl")))

  (define-key markdown-mode-map (kbd "C-c C-k") nil))

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
(require-package 'htmlize)
(add-hook 'org-mode-hook 'custom-org-mode-hook)
(defun custom-org-mode-hook ()
  (org-indent-mode)
  (setq org-startup-indented t)
  (require 'htmlize)
  (setq org-src-fontify-natively t)   ;  HTML 代码高亮

  (define-key org-mode-map (kbd "C-c C-k") nil)

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
      (org-edit-src-code))

    (local-set-key (kbd "C-c c e") 'org-edit-src-code)
    (local-set-key (kbd "C-c c i") 'org-insert-src-block)))

;; Origami
(require-package 'origami)
(require 'origami)
(global-origami-mode)

;; Scheme
(setq scheme-program-name "scheme")
(add-hook 'scheme-mode-hook 'custom-init-scheme-mode)
(add-hook 'inferior-scheme-mode-hook
          (lambda ()
            (define-key inferior-scheme-mode-map (kbd "C-c C-k") 'nil)))

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

  (define-key scheme-mode-map (kbd "C-c C-k") 'nil)
  (define-key scheme-mode-map (kbd "<f5>")    'custom-scheme-send-definition))

;; Smart Parens
(require-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode)
(add-hook 'eshell-mode-hook 'smartparens-mode)

(provide 'init-mode)
