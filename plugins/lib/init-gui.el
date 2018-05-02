(set-frame-parameter (selected-frame) 'alpha '(95 . 70))

(setq linum-format 'my-linum-format)

(require 'hl-line)

(defvar my-linum-current-line-number 0)

(defface my-linum-hl
  `((t :inherit linum
       :background "#E8E8FF"
       :foreground "#000000",(face-background 'hl-line nil t)))
  "Face for the current line number."
  :group 'linum)

(defun my-linum-format (line-number)
  (propertize (format " %2d " line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'my-linum-hl
                'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))

(ad-activate 'linum-update)

(global-hl-line-mode)

(set-face-attribute hl-line-face nil :background "#E8E8FF")

(set-face-background 'mode-line "#EDEDED")

(when (eq window-system 'nil) (menu-bar-mode -1))

(provide 'init-gui)
