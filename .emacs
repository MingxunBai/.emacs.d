;; set Emmet
(add-to-list 'load-path "~/.emacs.d/plugins/emmet-mode")
(require 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ; indent 2 space
(add-hook 'css-mode-hook 'emmet-mode) ; enable Emmet's css abbreviation

(load "~/.emacs.d/init.el")

