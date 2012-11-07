;; add other types to this list

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; turn off auto fill for latex
(add-hook 'latex-mode-hook 'turn-off-auto-fill)

(provide 'init-text-mode)
