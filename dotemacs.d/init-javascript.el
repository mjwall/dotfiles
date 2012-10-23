(add-hook 'js-mode-hook 'run-coding-hook)

(defun json-pretty-format ()
    (interactive)
    (save-excursion
        (shell-command-on-region (point-min) (point-max) "jsonlint -" (buffer-name) t)
        (indent-region begin end)))


(provide 'init-javascript)
