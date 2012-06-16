;; Turn off mouse interface early in startup to avoid momentary display
;; You can turn them on later if needed
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Make elisp more civilized
(require 'cl)

;; load path stuff
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir (concat dotfiles-dir "/site-lisp/"))
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons (expand-file-name dotfiles-dir) load-path))

;; basic config
(require 'init-defaults)
(require 'init-platform-specific)
(require 'init-util)
(require 'init-font)
(require 'init-editing)
(require 'init-window)
(require 'init-exec-path)

;; included packages
(require 'init-ibuffer)
(require 'init-ido-mode)
(require 'init-recentf)
(require 'init-linum)
(require 'init-mail)
(require 'init-whitespace)
(require 'init-isearch)
(require 'init-hippie-expand)

;; package management
(require 'init-elpa)

;; extra configs, require additional packages
(require 'init-ido-extras)
(require 'init-editing-extras)
(require 'init-themes)
(require 'init-gui-frames)
(require 'init-shell)
(require 'init-dired)
(require 'init-git)
(require 'init-org-mode)
(require 'init-project)

;; language specific configs
(require 'init-coding-hook)
(require 'init-lisps)
(require 'init-text-mode)
(require 'init-html)
(require 'init-java)
(require 'init-javascript)
(require 'init-groovy)
(require 'init-ruby)
(require 'init-yaml)
(require 'init-haml)
(require 'init-php)
(require 'init-scala)
(require 'init-sh)
(require 'init-cc)
(require 'init-arduino)
(require 'init-xml)
(require 'init-adoc)

;;(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; '(org-agenda-files (quote ("~/org/todo.org" "~/org/inbox.org" "~/org/journal.org"))))
;;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; )
