(require 'dired+)
(toggle-diredp-find-file-reuse-dir 1)
(setq dired-recursive-deletes 'top)
(define-key dired-mode-map [mouse-2] 'dired-find-file)


(provide 'init-dired)
