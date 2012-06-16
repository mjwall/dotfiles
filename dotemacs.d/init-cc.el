;; use c-initialization-hook for keybindings
;; see http://www.gnu.org/software/emacs/manual/html_node/ccmode/CC-Hooks.html

(add-hook 'c-mode-hook 'run-coding-hook)
(add-hook 'c++-mode-hook 'run-coding-hook)

(provide 'init-cc)
