(require-package 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq tab-always-indent 'complete) ; indent first, then complete
(setq ac-auto-start 1)
; (setq-default ac-expand-on-auto-complete nil) ; stop auto-start
(ac-set-trigger-key "TAB") ; use tab to complete
(setq ac-auto-show-menu 0.2)
(setq ac-use-menu-map t) ; set menu hotkey
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(provide 'init-auto-complete)
