;;; extensions.el --- full emacs initial file

;; Copyright (C) 2017 MingxunBai

;; Author: MingxunBai
;; Keywords: convenience
;; Version: 1.0

;;; Code:
;; 配置五笔输入法
(require 'chinese-wbim-extra)

(autoload 'chinese-wbim-use-package "chinese-wbim" "Another emacs input method")
(register-input-method "chinese-wbim" "euc-cn" 'chinese-wbim-use-package "五笔" "汉字五笔输入法" "wb.txt")
(setq chinese-wbim-use-tooltip nil)

;; 启动五笔输入法
(set-input-method 'chinese-wbim)
(toggle-input-method)

;;; Extensions
;; Dumb jump mode
(require 'dumb-jump)
(dumb-jump-mode)

;; Emmet mode
(defun enable-emmet-mode ()
  (interactive)
  (require 'emmet-mode)
  (emmet-mode)
  (setq emmet-self-closing-tag-style ""
        emmet-move-cursor-between-quotes t)

  (define-key emmet-mode-keymap (kbd "<C-return>") nil)
  (define-key emmet-mode-keymap (kbd "C-M-[") 'emmet-prev-edit-point)
  (define-key emmet-mode-keymap (kbd "C-M-]") 'emmet-next-edit-point))

;; Git
(defun find-git-repo (dir)
  (interactive)
  "Find base git directory"
  (if (not (string-match "[a-z0-9_-]/" dir))
      (message "It's not a git repo.")
    (if (file-exists-p (expand-file-name ".git/" dir))
        dir
      (find-git-repo (expand-file-name "../" dir)))))

(defun git-push (root)
  (shell-command (concat "cd " root " && git add -A"))
  (shell-command (concat "cd " root " && git commit -m 'Update'"))
  (shell-command (concat "cd " root " && git push")))

(defun custom-git-push-current-buffer ()
  (interactive)
  (let ((root (find-git-repo (file-name-directory (buffer-file-name)))))
    (git-push root)))

;; GoLang
(defun enable-go-mode ()
  (interactive)
  (require 'go-mode-autoloads)
  (go-mode)
  (defun go-save-fmt ()
    (interactive)
    (shell-command (concat "go fmt " (buffer-file-name))))

  (local-set-key (kbd "C-x f") 'go-save-fmt))

;; Highlight indent guides
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)

;; JavaScript IDE mode
(defun enable-js2-mode ()
  (interactive)
  (require 'js2-mode)
  (js2-mode)
  (require 'js-comint)
  (setq inferior-js-program-command "node")
  (setq inferior-js-program-arguments '("--interactive"))

  (defun custom-js-send-buffer ()
    (interactive)
    (js-send-buffer)
    (delete-other-windows)
    (custom-split-window 'switch-to-buffer "*js*")
    (other-window 1)
    (end-of-buffer)
    (other-window 1))

  (define-key js2-mode-map (kbd "<f5>") 'custom-js-send-buffer))

;; JSON mode
(defun enable-json-mode ()
  (interactive)
  (require 'json-mode)
  (json-mode))

;; Lazy set key
(require 'lazy-full-bind)

;; Less css mode
(defun enable-less-css-mode ()
  (interactive)
  (require 'less-css-mode)
  (less-css-mode)
  (setq less-css-compile-at-save t
        less-css-output-directory "../css"))

;; Markdown mode
(defun enable-markdown-mode ()
  (interactive)
  (require 'markdown-mode)
  (markdown-mode)
  (when *WINDOWS*
    (custom-set-variables '(markdown-command "markdown.pl")))

  (lazy-unset-key '("C-c C-k") markdown-mode-map))

;; Org mode
(add-hook 'org-mode-hook 'custom-org-mode-hook)
(defun custom-org-mode-hook ()
  (org-indent-mode)
  (setq org-startup-indented t)
  (require 'htmlize)
  (setq org-src-fontify-natively t)   ;  HTML 代码高亮

  (lazy-unset-key '("C-c C-k") org-mode-map)

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

;; Origami mode
(require 'origami)
(global-origami-mode)

;; Project explorer
(require 'project-explorer)
(defun pe/copy-relative-path ()
  (interactive)
  (pe/copy-file-name-as-kill)
  (other-window 1)
  (kill-new (file-relative-name (car kill-ring) (file-name-directory (buffer-file-name)))))

(define-key project-explorer-mode-map (kbd "C-c c") 'pe/copy-relative-path)

;; Python mode
(add-hook 'python-mode-hook 'custom-python-mode-hook)
(defun custom-python-mode-hook ()
  (require 'elpy)
  (elpy-mode)
  (require 'py-autopep8)
  (py-autopep8-enable-on-save)

  ;; (highlight-indent-guides-mode)
  (lazy-unset-key '("<backtab>") python-mode-map)
  (setq python-shell-prompt-detect-enabled nil))

;; Scheme mode
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

;; SCSS mode
(defun enable-scss-mode ()
  (require 'scss-mode)
  (scss-mode)
  (setq scss-compile-at-save t))

;; Text mode
(when *WINDOWS*
  (add-hook 'text-mode-hook 'init-wb-dict-gitrepo))

(defun init-wb-dict-gitrepo ()
  (add-hook 'kill-emacs-query-functions 'wb-dict-git-push)
  (defun wb-dict-git-push ()
    (interactive)
    (if (equal (buffer-name) "userdefinephrase.dat")
        (let* ((wb-dict-root (concat (getenv "ToolsHome") "\\BingWuBiDict")))
          (progn
            (shell-command (concat "cp -f '" (buffer-file-name) "' " wb-dict-root "'"))
            (git-push wb-dict-root)))
      (message "Exiting."))))

;; Vimrc mode
(defun enable-vimrc-mode ()
  (interactive)
  (require 'vimrc-mode)
  (vimrc-mode))

;; Web mode
(defun enable-web-mode ()
  (interactive)
  (require 'web-mode)
  (web-mode)
  (setq web-mode-markup-indent-offset             2
        web-mode-css-indent-offset                4
        web-mode-code-indent-offset               4
        web-mode-style-padding                    4
        web-mode-script-padding                   4
        web-mode-block-padding                    4
        web-mode-enable-current-element-highlight t)

  (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-file)
  (set-face-attribute 'web-mode-current-element-highlight-face nil :background "#F6F192")

  (add-hook 'web-mode-hook  'enable-emmet-mode))

(add-hook 'css-mode-hook  'enable-emmet-mode)
(add-hook 'html-mode-hook 'enable-web-mode)
(add-hook 'js2-mode-hook  'enable-emmet-mode)
(add-hook 'json-mode-hook 'enable-emmet-mode)

;; Winner mode
(when (fboundp 'winner-mode)
  (winner-mode))

;; Yaml mode
(defun enable-yaml-mode ()
  (interactive)
  (require 'yaml-mode)
  (yaml-mode))

;; YASnippet
(require 'yasnippet)
(yas-global-mode)
(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))

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
     :isearch t)))

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

;;; Major Mode
(setq auto-mode-alist
      (append '(("\\.css\\'"    .   (lambda () (enable-web-mode)))
                ("\\.go\\'"     .   (lambda () (enable-go-mode)))
                ("\\.js\\'"     .   (lambda () (enable-js2-mode)))
                ("\\.json\\'"   .   (lambda () (enable-json-mode)))
                ("\\.less\\'"   .   (lambda () (enable-less-css-mode)))
                ("\\.md\\'"     .   (lambda () (enable-markdown-mode)))
                ("\\.w?xml\\'"  .   (lambda () (enable-web-mode)))
                ("\\.php\\'"    .   (lambda () (enable-web-mode)))
                ("\\.s[ac]ss"   .   (lambda () (enable-scss-mode)))
                ("\\.vimrc\\'"  .   (lambda () (enable-vimrc-mode)))
                ("\\.ya?ml\\'"  .   (lambda () (enable-yaml-mode))))
              auto-mode-alist))

;; Reload mode
(set-auto-mode)

(provide 'extensions)