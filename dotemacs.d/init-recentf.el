(recentf-mode 1)
(setq recentf-max-saved-items 100)

(when (fboundp 'recentf-mode)
  (global-set-key (kbd "M-<f12>") 'recentf-open-files)
  (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
  (global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
  (global-set-key (kbd "C-c y") 'bury-buffer)
  (global-set-key (kbd "C-c r") 'revert-buffer)
  (global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
  (global-set-key (kbd "C-x f") 'recentf-ido-find-file))

(provide 'init-recentf)
