(autoload 'php-mode "php-mode" "mode for editing php files" t)
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc" . php-mode))
(add-to-list 'auto-mode-alist '("\\.tpl" . php-mode))

(add-hook 'php-mode-hook 'run-coding-hook)

(provide 'init-php)
