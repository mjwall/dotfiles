(add-to-list 'auto-mode-alist '(".bats" . sh-mode))


(add-hook 'sh-mode-hook 'run-coding-hook)

(require 'init-flymake-shell)
(add-hook 'sh-mode-hook 'flymake-shell-load)

(provide 'init-sh)
