;; When switching between Emacs 23 and 24, we always use the bundled package.el in Emacs 24
(let ((package-el-site-lisp-dir (expand-file-name "~/.emacs.d/site-lisp/package")))
  (when (and (file-directory-p package-el-site-lisp-dir)
             (> emacs-major-version 23))
    (message "Removing local package.el from load-path to avoid shadowing bundled version")
    (setq load-path (remove package-el-site-lisp-dir load-path))))

(require 'package)

;;------------------------------------------------------------------------------
;; On-demand installation of packages
;;------------------------------------------------------------------------------

(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;;------------------------------------------------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;------------------------------------------------------------------------------

(package-initialize)
(require-package 'smex)
(require-package 'autopair)
(require-package 'undo-tree)
(require-package 'diminish)
(require-package 'maxframe)
(require-package 'color-theme)
(require-package 'color-theme-zenburn)
(require-package 'color-theme-twilight)
(require-package 'color-theme-ir-black)
(require-package 'color-theme-railscasts)
(require-package 'multi-term)
(require-package 'magit)
(require-package 'paredit)
(require-package 'rainbow-delimiters)
(require-package 'clojure-mode)
(require-package 'clojure-test-mode)
(require-package 'clojurescript-mode)
(require-package 'slime)
(require-package 'slime-repl)
(require-package 'groovy-mode)
(require-package 'ruby-mode)
(require-package 'rvm)
(require-package 'rinari)
(require-package 'haml-mode)
(require-package 'yaml-mode)
(require-package 'markdown-mode)
(require-package 'php-mode)
(require-package 'scala-mode)

(provide 'init-elpa)
