;; set variables based on system type
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *is-gnu-linux* (eq system-type 'gnu/linux))

(when *is-a-mac*
  (setq default-input-method "MacOSX")

  ;; fonts
  (set-default-font "Monaco-15")

  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\.")))

  (when *is-cocoa-emacs*
    ;; allows me to drag into the doc icon and open editor: see
    ;; http://stackoverflow.com/questions/1850292/emacs-23-1-and-mac-os-x-problem-with-files-drag-and-drop
    (define-key global-map [ns-drag-file] 'my-ns-open-files)
    (setq ns-pop-up-frames nil) ; make emacs open in existing frames
    (defun my-ns-open-files ()
      "Open files in the list `ns-input-file'."
      (interactive)
      (mapc 'find-file ns-input-file)
      (setq ns-input-file nil))
    )
  )

(when *is-gnu-linux*
  ;;fonts
  (set-default-font "Monospace-12")
  ;; give me a familiar quit emacs keybinding
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  )

(provide 'init-platform-specific)
