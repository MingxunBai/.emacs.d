;; Recursive load path
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
(add-subdirs-to-load-path (expand-file-name "plugins" user-emacs-directory))

;; ELPA
(package-initialize)
(require 'package)
(setq package-archives
      '(
        ("gnu"       . "http://elpa.emacs-china.org/gnu/")
        ("marmalade" . "http://elpa.emacs-china.org/marmalade/")
        ("melpa"     . "http://elpa.emacs-china.org/melpa/")
        ("org"       . "http://elpa.emacs-china.org/org/")
        ))

(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(require 'extensions)
