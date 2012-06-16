;; malabar-mode
;; (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
;;                                   global-semanticdb-minor-mode
;;                                   global-semantic-idle-summary-mode
;;                                   global-semantic-mru-bookmark-mode))
;; (semantic-mode 1)
;; (require 'malabar-mode)
;; (setq malabar-groovy-lib-dir (concat dotfiles-dir "/site-lisp/malabar-1.4.0/lib"))

;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; (add-hook 'malabar-mode 'run-coding-hook)
;; (add-hook 'malabar-mode (lambda ()
;;                           (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)))
(add-hook 'java-mode-hook 'run-coding-hook)

(provide 'init-java)
