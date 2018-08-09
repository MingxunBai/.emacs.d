(set-frame-parameter (selected-frame) 'alpha '(95 . 90))

(setq-default mode-line-format nil)

(setq line-number-mode t                ;;
      linum-format 'my-linum-format     ; 显示行号列号
      column-number-mode t              ;;
      font-lock-maximum-decoration t    ;;
      font-lock-verbose t               ; 渲染当前 buffer 语法高亮
      font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000))
      eshell-prompt-function (lambda () ;; Eshell Prompt
                               (concat
                                (propertize (format-time-string "[%Y-%m-%d %H:%M:%S] " (current-time)))
                                (propertize (eshell/pwd))
                                (if (= (user-uid) 0) " # " " $ "))))

(require 'hl-line)

(defvar my-linum-current-line-number 0)

(defface my-linum-hl
  `((t :inherit linum
       :background "#EEE8D5"
       :foreground "#000000",(face-background 'hl-line nil t)))
  "Face for the current line number."
  :group 'linum)

(defun my-linum-format (line-number)
  (propertize (format " %3d " line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'my-linum-hl
                'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))

(ad-activate 'linum-update)

(global-hl-line-mode)

(set-face-attribute hl-line-face nil :background "#EEE8D5")

(set-face-background 'mode-line "#EDEDED")

(when (eq window-system 'nil) (menu-bar-mode -1))

;; Solarized Theme
(require-package 'solarized-theme)
(setq solarized-scale-org-headlines nil
      solarized-use-variable-pitch nil
      solarized-height-minus-1 1.0
      solarized-height-plus-1 1.0
      solarized-height-plus-2 1.0
      solarized-height-plus-3 1.0
      solarized-height-plus-4 1.0)
(require 'solarized-light-theme)

(provide 'init-gui)
