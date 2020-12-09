;;; init.el --- Initial

;;; Commentary:

;;; Code:
(setq debug-on-error t)

;; Encoding
(setq current-language-environment "utf-8"
      locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "\\.bat\\'" 'chinese-iso-8bit)

;; Setting
(setq-default tab-width 4
              indent-tabs-mode nil)
(setq ido-enable-flex-matching t
      make-backup-files nil
      track-eol t)

;; Modes
(global-auto-revert-mode t)
(ido-mode t)
(show-paren-mode t)

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(provide 'init)
;;; init ends here
