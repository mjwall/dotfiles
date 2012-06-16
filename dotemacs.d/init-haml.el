(add-to-list 'auto-mode-alist '("\\.jade" . haml-mode))
(add-hook 'haml-mode-hook 'run-coding-hook)

(provide 'init-haml)
