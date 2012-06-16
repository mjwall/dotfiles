(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (menu-bar-mode -1)
)

;; no fringe, wasted space
;;(set-fringe-mode 0) ;; interferes with ansi-term
;;(set-fringe-mode '(0 . 1))
;;(set-fringe-mode (quote (0 . 2)))

;; Window switching, shift and arrow key changes window
(windmove-default-keybindings)

(winner-mode 1)

;; bind some window resizing
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

(provide 'init-window)
