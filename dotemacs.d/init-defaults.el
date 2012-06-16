;; Use a more interesting startup message
(defun startup-echo-area-message ()
  "Hacky time...")


(setq visible-bell nil
      ring-bell-function 'ignore
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      x-select-enable-clipboard t
      kill-whole-line t                 ;; delete line in one stage
      mouse-yank-at-point t             ;; paste at cursor, NOT at mouse pointer position
      next-line-add-newlines nil        ;; don't add new lines when scrolling down
      require-final-newline t           ;; end files with a newline
      safe-local-variable-values '((encoding . utf-8) (prompt-to-byte-compile))
      scroll-margin 0                   ;; do smooth scrolling, ...
      scroll-conservatively 100000      ;; ... the defaults ...
      scroll-up-aggressively 0          ;; ... are very ...
      scroll-down-aggressively 0        ;; ... annoying
      user-full-name "Michael Wall"     ;; Set name
      user-mail-address "mjwall@gmail.com" ;; Set e-mail address
      ;; set ispell to use brew installed aspell,
      ;; http://sunny.in.th/2010/05/08/emacs-enabling-flyspell-mode-gave-an-error.html
      ispell-program-name "aspell"
      )


;;(set-default 'imenu-auto-rescan t)

(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator

;; Backups, don't clutter up directories with files
(setq make-backup-files t ;; do make backups
      backup-by-copying t ;; and copy them here
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)

;; Transparently open compressed files
(auto-compression-mode t)

;; add some server hooks
(add-hook 'after-init-hook 'server-start)
(add-hook 'server-done-hook
   (lambda ()
     (shell-command "screen -r -X select `cat ~/.emacsclient-caller`")))

(defun start-my-server ()
  (progn
    (message "Starting emacs server")
    (if (file-exists-p (concat (getenv "TMPDIR") "emacs" (number-to-string (user-real-uid)) "/server"))
        nil (server-start)
        (message "Server already started"))))

(message "About to call start-my-server")
(start-my-server)

;; Start server for emacs client if not already started
(provide 'init-defaults)
