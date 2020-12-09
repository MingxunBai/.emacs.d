;;; init.el --- Initial

;;; Commentary:

;;; Code:
(setq debug-on-error t)

;; ELPA
(setq package-archives
      '(("cn-gnu"       . "http://elpa.emacs-china.org/gnu/")
        ("cn-melpa"     . "http://elpa.emacs-china.org/melpa/")
        ("cn-marmalade" . "http://elpa.emacs-china.org/marmalade/")
        ("cn-org"       . "http://elpa.emacs-china.org/org/")))
(require 'package)
(package-initialize)

;; Encoding
(setq current-language-environment "utf-8"
      locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "\\.bat\\'" 'chinese-iso-8bit)

;; Require Package
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (versions (mapcar #'package-desc-version known)))
        (if (cl-find-if (lambda (v) (version-list-<= min-version v)) versions)
            (package-install package)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t)))))
  (require package))

;; Return instead of YES
(defun y-or-n-p-with-return (orig-func &rest args)
  "ORIG-FUNC ARGS?."
  (let ((query-replace-map (copy-keymap query-replace-map)))
    (define-key query-replace-map (kbd "RET") 'act)
    (apply orig-func args)))
(advice-add 'y-or-n-p :around #'y-or-n-p-with-return)
(fset 'yes-or-no-p 'y-or-n-p)

;; Setting
(setq-default tab-width 4
              indent-tabs-mode nil
              ido-enable-flex-matching t
              make-backup-files nil
              track-eol t)

;; Modes
(global-auto-revert-mode t)
(ido-mode t)
(show-paren-mode t)

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(require-package 'doom-modeline)
(setq doom-modeline-icon nil)
(doom-modeline-mode t)

(require-package 'multiple-cursors)

(provide 'init)
;;; init ends here
