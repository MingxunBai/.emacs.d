(require-package 'emmet-mode)
(require 'emmet-mode)

(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ; indent 2 space
(add-hook 'css-mode-hook 'emmet-mode) ; enable Emmet's css abbreviation
(global-set-key (kbd "\C-x \C-e") 'emmet-mode)

(provide 'init-emmet-mode)
