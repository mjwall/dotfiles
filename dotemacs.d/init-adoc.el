(autoload 'adoc-mode "adoc-mode")
(add-to-list 'auto-mode-alist (cons "\\.asc\\'" 'adoc-mode))
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))

;; change font face in asciidoc files
(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))
(add-hook 'text-mode-hook 'turn-on-flyspell)

(provide 'init-adoc)
