;;;; This is my Emacs init.el file

;; Things marked with #npoge are Not Part of Gnu Emacs.  They are
;; language specific in most cases, althought I did bring in a couple
;; things to help with imenu.  The rest is configuration from stock GNU
;; Emacs and functions I either borrowed or wrote.

;;;- Startup
;; turn off mouse interface early in startup to avoid momentary
;; display turned on later as needed
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; add ~/.emacs.d/site-lisp
(add-to-list 'load-path (expand-file-name "site-lisp"
                                          user-emacs-directory))

;; put customizations in a seperate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;- Default variables
(setq-default
 ;; me
 user-full-name "Michael Wall"
 user-mail-address "mjwall@gmail.com"
 ;; no bell
 visible-bell nil
 ring-bell-function 'ignore
 ;; echo faster
 echo-keystrokes 0.1
 ;; no startup
 inhibit-startup-message t
 ;; make buffer names unique
 uniquify-buffer-name-style 'forward
 ;; Backups, don't clutter up directories with files
 make-backup-files t ;; do make backups
 backup-by-copying t ;; and copy them here
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 version-control t
 kept-new-versions 2
 kept-old-versions 5
 delete-old-versions t
 ;; show empty lines after buffer end
 indicate-empty-lines t
 ;; indent with tab function
 indent-line-function 'insert-tab
 ;; but use spaces not tabs
 indent-tabs-mode nil
 ;; and make it 2 spaces
 tab-width 2
 ;; set whitespace style, turned on in prog mode hook
 whitespace-style '(trailing space-before-tab indentation
                             space-after-tab tabs tab-mark)
 ;; follow symlinks to real file, default is ask
 vc-follow-symlinks t
 ;; have compile buffers scroll, use first-error to stop there
 compilation-scroll-output t
 )

;;;- Other setting
;; transparently open compressed files
(auto-compression-mode t)
;; make emacs revert files when they change,
;; for example when you switch git branches
(global-auto-revert-mode 1)
;; no line numbers by default
(global-linum-mode 0)
;; make backspace work as expected
(normal-erase-is-backspace-mode t)
;; delete selected on keypress
(delete-selection-mode 1)
;; highlight matching parentheses when the point is on them.
(show-paren-mode 1)
;; use y or n
(defalias 'yes-or-no-p 'y-or-n-p)
;; seed the random-number generator
(random t)
;; encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
;; redefine the boring startup message
(defun startup-echo-area-message () "Make code ...")
;; use winner-mode
(winner-mode 1)
;; S-left, etc; disables Shift select
(windmove-default-keybindings)
;; use custom move buffer functions, in site-lisp #npoge
(require 'move-my-buffer)
(move-my-buffer-default-keybindings)

;;;- Change some keybindings for default functions
;; no mail
(global-unset-key (kbd "C-x m"))
;; make Alt-` go to other frame as expected, like s-`
(global-set-key (kbd "M-`") 'other-frame)
;; keybinding to bring up ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; a few more shortcuts
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
;; some more familiar keybindings for default functions
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key "\r" 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
;; rebind to undo, stop suspending-frame
(global-set-key (kbd "C-z") 'undo)
;; not sure why this works on Mac but not Linux
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
;; delete backwards without having to use BACKSPACE
(global-set-key (kbd "C-S-d") 'backward-delete-char)
;; Setup hippie-expand
(global-set-key [C-tab] 'hippie-expand)
;; bind some window resizing
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)
;; vc-git
(require 'vc-git)

;; Platform specific settings
(defvar *is-a-mac*)
(defvar *is-carbon-emacs*)
(defvar *is-cocoa-emacs*)
(defvar *is-gnu-linux*)
(setq
 *is-a-mac* (eq system-type 'darwin)
 *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac))
 *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns))
 *is-gnu-linux* (eq system-type 'gnu/linux))
(when *is-a-mac*
  (setq
   ;; for multilingual environments
   default-input-method "MacOSX"
   ;; font
   default-frame-alist '((font . "Menlo-13"))
   ;; Work around a bug on OS X where system-name is FQDN
   system-name (car (split-string system-name "\\."))
   ;; make emacs open in existing frames
   ;;ns-pop-up-frames nil
   ;; brew install hunspell, https://joelkuiper.eu/spellcheck_emacs
   ispell-program-name "hunspell"
   ;; modifier keys meta and cmd swapped in system preferences
   ;; but could do it here with
   ;;mac-command-modifier 'meta
   ;;mac-option-modifier 'super
   ;; hitting cmd still gets me in trouble in emacs though
   mac-command-modifier nil
   ))
(when *is-gnu-linux*
  (setq
   ;; font
   default-frame-alist '((font . "Monospace-12"))
   ;; make emacs use the clipboard
   x-select-enable-clipboard t
   ;; use hunspell
   ispell-program-name "hunspell"
   ))

;;;- Setup GUI version
(when window-system
  ;; change font size, only works in GUI
  (global-set-key (kbd "M-+") 'text-scale-increase)
  (global-set-key (kbd "M-_") 'text-scale-decrease)
  ;; set a few things that don't play well with the daemon
  (setq
   use-file-dialog nil
   use-dialog-box nil)
  ;; function to change opacity
  (defun adjust-opacity (frame incr)
    "Function to change the opacity of the FRAME by the given INCR."
    (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
           (newalpha (+ incr oldalpha)))
      (when (and (<= frame-alpha-lower-limit newalpha) (>= 100
                                                           newalpha))
        (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
  ;; keybindings to change opacity
  (global-set-key (kbd "C-8") '(lambda () (interactive)
                                 (adjust-opacity nil -5)))
  (global-set-key (kbd "C-9") '(lambda () (interactive)
                                 (adjust-opacity nil 5)))
  (global-set-key (kbd "C-0")
                  '(lambda ()
                     (interactive)
                     (modify-frame-parameters nil `((alpha . 100)))))
  ;; show menu bar
  (menu-bar-mode 1)
  (defun set-exec-path-from-shell-PATH ()
    "Set the PATH inside emacs to the same as inside a shell.
     Mostly needed when clicking on the Emacs.app icon"
    (let ((path-from-shell
           (replace-regexp-in-string
            "[ \t\n]*$"
            ""
            (shell-command-to-string
             "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq eshell-path-env path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (set-exec-path-from-shell-PATH))


;;;- Editing functions
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(defun untabify-buffer ()
  "Change all tabs to spaces in current buffer"
  (interactive)
  (untabify (point-min) (point-max)))
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a
buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
(defun shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))
(global-set-key (kbd "<M-right>") 'shift-right)
(defun shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (shift-right (* -1 (or arg 1))))
(global-set-key (kbd "<M-left>") 'shift-left)
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))
(defun move-text-down (arg)
  "Move region or current line `arg' lines down."
  (interactive "*p")
  (move-text-internal arg))
(defun move-text-up (arg)
  "Move region or current line `arg' lines up."
  (interactive "*p")
  (move-text-internal (- arg)))
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

;;;- Useful functions
(defun save-buffer-always ()
  "Save the buffer even if it is not modified.
Useful if something is watching file modification times."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))
(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%a %b %d %H:%M:%S %Y")))
(defun delete-this-file ()
  "Delete the current file, and kill the buffer. The `delete-file'
function does not kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory
                              buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file name new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(defun smarter-move-beginning-of-line (arg)
  "Toggle between beginning-of-line and back-to-indentation"
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
(defun dos2unix ()
      "Not exactly but it's easier to remember"
      (interactive)
      (set-buffer-file-coding-system 'unix 't))
(defun open-terminal-at (directory)
  "Open a terminal at the `directory' passed in using a name derived
from the directory.  If a terminal by that name already exists, just
switch to it instead of creating a new one.

If the directory does not exist, an error message is displayed."
  (interactive)
  (if (not (file-exists-p directory))
      (message (concat
                "Terminal not created, Directory does not exist: "
                directory))
    (let* ((name (concat "term-" directory))
           (term-name (concat "*" name "*"))
           (default-directory directory))
      (if (get-buffer term-name)
          (switch-to-buffer term-name)
        (progn
          (ansi-term "/bin/bash" name) ;; wraps name with *'s
          (process-send-string
           (get-process term-name)
           (concat "cd " directory "\nclear\n")))))))
(defun open-eshell-at (directory)
  "Open an eshell at the `directory' passed in using a name derived
from the directory.  If an eashll by that name already exists, just
switch to it instead of creating a new one.

If the directory does not exit, an error message is displayed."
  (interactive)
  (if (not (file-exists-p directory))
      (message (concat
                "Eshell not created, Directory does not exist: "
                directory))
    (let* ((name (concat "*eshell-" directory "*"))
           (default-directory directory))
      (if (not (get-buffer name))
          (progn
            (setq eshell-buffer-name name)
            (eshell)
            (setq eshell-buffer-name "*eshell*")))
      (switch-to-buffer name))))
(defun current-location ()
  "Function that works in buffers, dired, term and eshell to return
current location."
  (interactive)
  (or (if (buffer-file-name) (file-name-directory (buffer-file-name)))
      (if (fboundp 'eshell/pwd) (eshell/pwd))
      (if (fboundp 'default-directory) (default-directory))
      (getenv "HOME")))
(defun eshell-here ()
  "Open an eshell at the `current-location'"
  (interactive)
  (open-eshell-at (current-location)))
(defalias 'ee 'eshell-here)
(defun terminal-here ()
  "Open a terminal at the `current-location'"
  (interactive)
  (open-terminal-at (current-location)))
(defalias 'tt 'terminal-here)

;;;- Dired
(require 'dired-x) ;; provided dired-jump bound to C-x C-j
(defun open-term-from-dired ())
(define-key dired-mode-map (kbd "`")
  (lambda () (interactive) (open-terminal-at dired-directory)))
(define-key dired-mode-map (kbd "'")
  (lambda () (interactive) (open-eshell-at dired-directory)))

;;;- Ido Interactively Do Things
(when (> emacs-major-version 21)
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-prefix nil
        ido-ignore-directories '("\\`auto/" "\\`auto-save-list/"
                                 "\\`backups/" "\\`semanticdb/"
                                 "\\`target/" "\\`\\.git/"
                                 "\\`\\.svn/" "\\`CVS/" "\\`\\.\\./"
                                 "\\`\\./")
        ido-ignore-files '("\\`auto/" "\\.prv/" "_region_" "\\.class/"
                           "\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./"
                           "\\`\\./")
        ;; Display ido results vertically, rather than horizontally
        ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
                                " [No match]" " [Matched]" " [Not
readable]" " [Too big]" " [Confirm]"))
        )
  ;; imenu with ido, from http://www.emacswiki.org/emacs/idomenu.el #npoge
  (require 'idomenu)
  (global-set-key (kbd "C-x TAB") 'idomenu) ;; C-x C-i
  ;; add keybindings so C-p and C-n move next with vertical results
  ;; and C-up and C-down do the same even if the current string
  ;; doesn't match
  (add-hook
   'ido-setup-hook
   (lambda ()
     (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
     (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
     (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
     (define-key ido-completion-map (kbd "C-n") 'ido-next-match)))
  ;; key binding to open in other window
  (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
  ;; setup recentf mode
  (eval-after-load "recentf"
    '(setq recentf-max-saved-items 100))
  (defun recentf-ido-find-file ()
    "Find a recent file using ido."
    (interactive)
    (let ((file
           (ido-completing-read "Choose recent file: "
                                recentf-list nil t)))
      (when file
        (find-file file))))
  (recentf-mode 1)
  (global-set-key (kbd "M-<f12>") 'recentf-open-files)
  (global-set-key (kbd "C-x f") 'recentf-ido-find-file))

;;;- Isearch configs
(defun call-with-current-isearch-string-as-regex (f)
  "Takes current selection as F and then search with isearch string."
  (let ((case-fold-search isearch-case-fold-search))
    (funcall f
             (if isearch-regexp isearch-string
               (regexp-quote isearch-string)))))
;; activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (call-with-current-isearch-string-as-regex 'occur)))
;;
(defun isearch-yank-symbol ()
  "Put symbol at current point into search string so you can search
back/forth for the symbol at point
http://www.emacswiki.org/emacs/SearchAtPoint"
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string
                (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message
                (mapconcat 'isearch-text-char-description
                           isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))
;; activate inside isearch
(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)

;;;- Bookmarks
(require 'bookmark)
(setq bookmark-save-flag 1)

;;;- Eshell
;; load history vars
(load "em-hist")
;; set some variables
(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore
      "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
      eshell-history-size 1024
      eshell-prompt-regexp "^[^#$]*[#$] "
      eshell-highlight-prompt nil
      eshell-visual-commands '("less" "top" "vim")
      eshell-visual-subcommands '(("git" "log" "diff" "di" "show")))
;; eshell prompt
(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize
          (concat user-login-name "@"
                  (car (split-string system-name "\\.")))
          'face '(foreground-color . "green4"))
         (propertize
          (concat " " (abbreviate-file-name (eshell/pwd)))
          'face '(foreground-color . "darkgoldenrod4"))
         (when (vc-git-root (eshell/pwd))
           (propertize
            (concat " " (car (vc-git-branches)))
            'face '(foreground-color . "darkcyan")))
         "\n"
         (if (= (user-uid) 0) "# " "$ ")
         )))
(defalias 'ff 'find-file)
(defalias 'd 'dired)
(defalias 'fo 'find-file-other-window)
(defalias 'emacs 'find-file)
(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-q" 'eshell-kill-process)
             (local-set-key "\C-c\C-k" 'compile)
             ;; not very elegant, but whatevs
             (eshell/export (concat
                             "JAVA_HOME="
                             (shell-command-to-string
                              ". ~/.bashrc && echo $JAVA_HOME")))))

;;;- Term
;; Give some indication which mode; mapped to keys below
(defun my-term-line-mode ()
  "Create message on entering term line mode."
  (interactive)
  (term-line-mode)
  (message "entering term-line-mode"))
(defun my-term-char-mode ()
  "Create message on entering term char mode."
  (interactive)
  (term-char-mode)
  (message "entering term-char-mode"))
;; Copied from multi-term, also mapped below
(defun my-term-send-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))
(defun my-term-send-forward-word ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))
(defun my-term-send-backward-kill-word ()
  "Backward kill word in term mode."
  (interactive)
  (term-send-raw-string "\C-w"))
(defun my-term-send-forward-kill-word ()
  "Forward kill word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))
(defun my-term-exit ())
;; better term keybindings
(add-hook 'ansi-term-after-hook
          (lambda ()
            ;; char-mode-map
            (define-key term-raw-map (kbd "C-y")
              'term-paste)
            (define-key term-raw-map (kbd "C-c C-c")
              'term-interrupt-subjob)
            (define-key term-raw-map (kbd "C-c C-\\")
              'term-quit-subjob)
            (define-key term-raw-map (kbd "C-s")
              'isearch-forward)
            (define-key term-raw-map (kbd "C-r")
              'isearch-backward)
            (define-key term-raw-map (kbd "C-p")
              'previous-line)
            (define-key term-raw-map (kbd "C-n")
              'next-line)
            (define-key term-raw-map (kbd "M-f")
              'my-term-send-forward-word)
            (define-key term-raw-map (kbd "M-b")
              'my-term-send-backward-word)
            (define-key term-raw-map (kbd "C-c C-j")
              'my-term-line-mode)
            (define-key term-raw-map (kbd "M-DEL")
              'my-term-send-backward-kill-word)
            (define-key term-raw-map (kbd "M-d")
              'my-term-send-forward-kill-word)
            (define-key term-raw-map (kbd "M-w")
              'kill-ring-save)
            (define-key term-raw-map (kbd "C-x k")
              'buffer-kill)
            (define-key term-raw-map (kbd "C-x C-k")
              'kill-this-buffer)
            ;; line-mode map
            (define-key term-mode-map (kbd "C-c C-k")
              'my-term-char-mode)
            ;; C-x C-k is already compile
            ))
;; close terminal buffer on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;;;- Org-mode
(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq
 org-directory (concat (getenv "HOME") "/.org/")
 org-agenda-custom-commands
 (quote (("1" "Today's agenda" ((agenda "" ((org-agenda-ndays 1)))))
         ("n" "Week agenda + TODOs" ((agenda "") (todo)))))
 org-agenda-files (list
                   (concat org-directory "work.org")
                   (concat org-directory "personal.org")
                   (concat org-directory "someday.org")
                   (concat org-directory "inbox.org")
                   (concat org-directory "journal.org")
                   (concat org-directory "notes.org"))
 org-agenda-include-diary t
 org-agenda-ndays 7
 org-agenda-show-all-dates t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-start-on-weekday nil
 org-capture-templates
 (quote (("t" "Todo" entry
          (file+headline (concat org-directory "inbox.org") "Tasks")
          "* TODO %? %i %a")
         ("j" "Journal" entry
          (file+datetree (concat org-directory "journal.org"))
          "* %? Entered on %U %i %a")
         ("n" "Note" entry
          (file (concat org-directory "notes.org"))
          "* %? :NOTE: %U %a")
         ("s" "Someday" entry
          (file (concat org-directory "someday.org"))
          "* %? :SOMEDAY: %U %a")))
 org-completion-use-ido t
 org-deadline-warning-days 14
 org-default-notes-file (concat org-directory "inbox.org")
 org-indirect-buffer-display (quote current-window)
 org-log-done (quote note)
 org-outline-path-complete-in-steps nil
 org-refile-allow-creating-parent-nodes (quote confirm)
 org-refile-targets
 (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
 org-refile-use-outline-path t
 org-reverse-note-order t
 org-support-shift-select nil)

;; turn on languages
(org-babel-do-load-languages
 'org-babel-load-languages
 ;; add plantuml, requires plantuml.jar
 '(;; other Babel languages
   (plantuml . t)))
(setq org-plantuml-jar-path ;; in site-lisp, #npoge
      (expand-file-name
       (concat user-emacs-directory "/site-lisp/plantuml.jar")))
(defun sync-org-files ()
  "Run a bash script located in org-directory to sync org files via
git."
  (interactive)
  (org-save-all-org-buffers)
  (message "Syncing org files")
  (with-output-to-temp-buffer "*org-git-sync*"
    (shell-command
     (concat org-directory "org-git-sync") "*org-git-sync*")
    (pop-to-buffer "*org-git-sync*"))
  (message "Done syncing org files"))
;; run sync when starting emacs
(sync-org-files)
;; and again on shutodwn
(add-hook 'kill-emacs-hook 'sync-org-files)

;;;- Pomodoro
;; my own pomodoro script #npoge
(defvar pomo-script (concat (getenv "HOME") "/bin/pomo")
  "Location of the 'pomo' script. See https://github.com/mjwall/pomo
for script details")
(defun pomo ()
  "If buffer *pomo* does not exist, create the buffer with eshell
running `pomo-script'.  If the buffer does exist, simply switch to
it."
  (interactive)
  (if (not (get-buffer "*pomo*"))
      (progn
        (message "Starting pomo")
        (setq eshell-buffer-name "*pomo*")
        (eshell)
        (setq eshell-buffer-name "*eshell*")
        (switch-to-buffer "*pomo*")
        (insert (concat pomo-script "\n"))
        (eshell-send-input)))
  (switch-to-buffer "*pomo*"))
(global-set-key (kbd "<f6>") 'pomo)

;;;- Project mode
(defun my-git-root ()
  "Find the root of a project based on the presence of a .git directory.
 Return empty if not is found"
  (interactive)
  (let ((curr (current-location)))
    (if (not (string= "" curr))
        (or (locate-dominating-file curr ".project-root")
            (locate-dominating-file curr ".git"))
      (message "No current file found"))))
(defun my-project-root ()
  "Find the root of a project based on the presence of .project-root
file or some other file defined below.  As a last resort, look for
my-git-root.  Return empty if not is found"
  (interactive)
  (let ((curr (current-location)))
    (if (not (string= "" curr))
        (or (locate-dominating-file curr ".project-root")
            (locate-dominating-file curr "pom.xml")
            (locate-dominating-file curr "build.xml")
            (locate-dominating-file curr "project.clj")
            (locate-dominating-file curr "Rakefile")
            (locate-dominating-file curr "init.el")
            (my-git-root))
      (message "No file found."))))
(defun grep-in-project-root ()
  "Run rgrep for the given pattern in (my-project-root)"
  (interactive)
  (let* ((regexp (grep-read-regexp))
         (files (grep-read-files regexp)))
    (rgrep regexp files (my-project-root))))
(defun find-file-in-project-root (name)
  "Run find-name-dired in (my-project-root)"
  (interactive "sFilename: ")
  (find-name-dired (my-project-root) name))
(defun eshell-in-project-root ()
  "Launch `open-eshell-at' in `(my-project-root)'."
  (interactive)
  (open-eshell-at (my-project-root)))
(defun term-in-project-root ()
  "Launch `open-terminal-at' in `(my-project-root)'."
  (interactive)
  (open-terminal-at (my-project-root)))
(defun compile-in-project-root (command)
  "Launch `compile' in the `(my-project-root)' with the input command"
  (interactive "sCompile Command: ")
  (let* ((default-directory (my-project-root)))
    (compile command)))
(defun grep-in-git-root (term)
  "Run rgrep for the given pattern in (my-git-root)"
  (interactive)
  (let* ((regexp (grep-read-regexp))
         (files (grep-read-files regexp)))
    (rgrep regexp files (my-project-root))))
(defun find-file-in-git-root (name)
  "Run find-name-dired in (my-git-root)"
  (interactive "sFilename: ")
  (find-name-dired (my-git-root) name))
(defun eshell-in-git-root ()
  "Launch `open-eshell-at' in `(my-git-root)'."
  (interactive)
  (open-eshell-at (my-git-root)))
(defun term-in-git-root ()
  "Launch `open-terminal-at' in `(my-git-root)'."
  (interactive)
  (open-terminal-at (my-git-root)))
(defun compile-in-git-root (command)
  "Launch `compile' in the `(my-git-root)' with the input command"
  (interactive "sCompile Command: ")
  (let* ((default-directory (my-git-root)))
    (compile command)))
(global-set-key (kbd "C-c p g") 'grep-in-project-root)
(global-set-key (kbd "C-c p f") 'find-file-in-project-root)
(global-set-key (kbd "C-c p e") 'eshell-in-project-root)
(global-set-key (kbd "C-c p t") 'term-in-project-root)
(global-set-key (kbd "C-c p c") 'compile-in-project-root)
(global-set-key (kbd "C-c g g") 'grep-in-git-root)
(global-set-key (kbd "C-c g f") 'find-file-in-git-root)
(global-set-key (kbd "C-c g e") 'eshell-in-git-root)
(global-set-key (kbd "C-c g t") 'term-in-git-root)
(global-set-key (kbd "C-c g c") 'compile-in-git-root)

;;;- Packages
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Themes
;; my theme, tango-dark is nice for a default
(require 'ample-zen-theme) ;; package-install ample-zen-theme #npoge
(load-theme 'dichromacy t)

;;;- File type specific
;; functions for the new prog-mode-hook
;; not turning on linum, but here is format when it is is used
(eval-after-load "linum"
  '(setq linum-format "%4d "))
(defun turn-on-whitespace ()
  "Turn on whitespace."
  (whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))
(defun turn-on-hideshow ()
  "Turn on hideshow."
  (hs-minor-mode t))
(defun add-watchwords ()
  "Add watchwords."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|XXX\\):"
      1 font-lock-warning-face t))))
(defun bye-flyspell ()
  "Turn off flyspell."
  (turn-off-flyspell))
;; setup the new prog-mode
(add-hook 'prog-mode-hook 'turn-on-whitespace)
(add-hook 'prog-mode-hook 'add-watchwords)
(add-hook 'prog-mode-hook 'turn-on-hideshow)
(add-hook 'prog-mode-hook 'bye-flyspell)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'turn-off-auto-fill)

;;;- Text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

;;;- Org files
;;(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;;;- Elisp mode
(defun imenu-elisp-sections ()
  "Make section in elisp imenu defined by ';;;-' ."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression
               '("Sections" "^;;;- \\(.+\\)$" 1) t))
 (add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;;;- Latex mode
(add-hook 'latex-mode-hook 'turn-off-auto-fill)

;;;- Java mode
;; change the way imenu shows stuff, in site-lisp
(require 'java-imenu) ;; in site-lisp #npoge

;;;- Clojure mode
(require 'clojure-mode) ;; package-install clojure-mode #npoge

;;;- Scala mode
(require 'scala-mode2) ;; package-install scala-mode2 #npoge
(add-to-list 'auto-mode-alist '(".sbt" . scala-mode))

;;;- Sh mode (bash)
(add-to-list 'auto-mode-alist '(".bashrc" . sh-mode))
(add-to-list 'interpreter-mode-alist '("sh" . sh-mode))
(add-to-list 'interpreter-mode-alist '("bash" . sh-mode))
(add-hook 'sh-mode-hook
          '(lambda ()
             (setq sh-basic-offset 2
                   sh-indentation 2)))
;;;- Ruby mode
(require 'ruby-mode)
(require 'ruby-end) ;; package-install ruby-end #npoge
(add-to-list 'auto-mode-alist '("buildfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))
(add-to-list 'auto-mode-alist '("Isolate" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.autotest" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(setq ruby-indent-level 2)
(add-hook 'ruby-mode-hook '(lambda ()
                             (setq ruby-deep-arglist t)
                             (setq ruby-deep-indent-paren nil)
                             (setq c-tab-always-indent nil)
                             ;;(require 'inf-ruby)
                             ;;(require 'ruby-compilation)
                             ))
(add-hook 'ruby-mode-hook 'ruby-end-mode)
(defun use-rbenv ()
  "Setup paths to use rbenv global mode, may need to `C-c r' to revert
open buffer."
  (interactive)
  (setenv "PATH" (concat
                  (getenv "HOME") "/.rbenv/shims:"
                  (getenv "HOME") "/.rbenv/bin:"
                  (getenv "PATH")))
  (setq exec-path (cons (concat
                         (getenv "HOME") "/.rbenv/shims")
                        (cons (concat
                               (getenv "HOME") "/.rbenv/bin")
                              exec-path))))
(defalias 'rbenv 'use-rbenv)

;;;- JFlex mode
;;from http://jflex.de/emacs.html #npoge
(require 'jflex-mode) ;; in site-lisp #npoge
(autoload 'jflex-mode "jflex-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.jflex" . jflex-mode))
(add-to-list 'auto-mode-alist '("\\.flex" . jflex-mode))

;;;- Javascript mode
(require 'json)
(add-to-list 'auto-mode-alist '(".json" . js-mode))
(defun json-pretty-format ()
  "Run a jsonlint shell script on the region and then indent.

  jsonlint is on my path and looks like this
#!/usr/bin/python
import json
import sys

j = json.load(sys.stdin)
print json.dumps(j, sort_keys=True, indent=2)"
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max)
                             "jsonlint -" (buffer-name) t)
    (indent-region begin end)))

;;;- YAML mode
(require 'yaml-mode) ;; package-install yaml-mode #npoge
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;- HAML mode
(require 'haml-mode) ;; package-install haml-mode #npoge
(add-to-list 'auto-mode-alist '("\\.jade" . haml-mode))

;;;- Groovy mode
(require 'groovy-mode) ;; package-install groovy-mode #npoge
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))

;;;- PHP mode
(require 'php-mode) ;; package-install php-mode #npoge
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc" . php-mode))
(add-to-list 'auto-mode-alist '("\\.tpl" . php-mode))

;;;- CC mode
;; use c-initialization-hook for keybindings see
;; http://www.gnu.org/software/emacs/manual/html_node/ccmode/CC-Hooks.html

;;;- Arduino mode
;;https://raw.github.com/bookest/arduino-mode/master/arduino-mode.el
(require 'arduino-mode) ;; in site-lisp #npoge

;;;- Asciidoc stuff
;; in site-lisp #npoge
(add-to-list 'load-path
             (concat
              (expand-file-name "site-lisp" user-emacs-directory)
              "/doc-mode-1.1"))
(autoload 'doc-mode "doc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . doc-mode))
(add-hook 'doc-mode-hook
          '(lambda ()
             (turn-off-auto-fill)
             (turn-on-flyspell)
             (require 'asciidoc) ;; in site-lisp #npoge
             ))

;;;- XML mode
;; Use nxml-mode instead of sgml, xml or html mode.
(mapc
 (lambda (pair)
   (if (or (eq (cdr pair) 'xml-mode)
           (eq (cdr pair) 'sgml-mode)
           (eq (cdr pair) 'html-mode))
       (setcdr pair 'nxml-mode)))
 auto-mode-alist)
(defun nxml-pretty-format ()
  "Function to format the current selected region.

  calls xmllint in a shell"
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max)
                             "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region begin end)))
;; autocomplete tags on </
(setq nxml-slash-auto-complete-flag t)

;;;- Thrift mode
;; in site-lisp from https://gist.github.com/2470924 #npoge
(require 'thrift-mode)

;;;- Markdown mode
(require 'markdown-mode) ;; package-install markdown-mode #npoge

;;;- Web mode
(require 'web-mode) ;; package-install web-mode #npoge
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;;- Python
;; does anyone really use this?

(provide 'init)
