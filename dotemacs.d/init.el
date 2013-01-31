
;; Emacs config outline
;; - Defaults
;; - Display
;; - Platform specific
;; - Navigation
;; - Editing and formatting
;; - Utility functions
;; - Language specific

;;----------------------------------------------------------------------------
;; - Defaults
;;----------------------------------------------------------------------------

;; Turn off mouse interface early in startup to avoid momentary display, can be turned on later if needed
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Make elisp more civilized
(require 'cl)

;; default load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Use a more interesting startup message
(defun startup-echo-area-message ()
  "Go...")

;; default variables
(setq visible-bell nil
      ring-bell-function 'ignore
      echo-keystrokes 0.1
      inhibit-startup-message t
      transient-mark-mode t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      user-full-name "Michael Wall"     ;; Set name
      user-mail-address "mjwall@gmail.com" ;; Set e-mail address
      ;; Backups, don't clutter up directories with files
      make-backup-files t ;; do make backups
      backup-by-copying t ;; and copy them here
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t
      )

;; use y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Seed the random-number generator
(random t)

;; Transparently open compressed files
(auto-compression-mode t)

;; encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; show empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; make emacs revert files when they change, for example when you switch git branches
(global-auto-revert-mode 1)

;; setup the paths, this was useful when I was using ansi-term
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo
 $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(if window-system (set-exec-path-from-shell-PATH))

;; no line numbers unless I say so, but set the format for when I do, coding-hooks will
;; provide line numbers for all code
(global-linum-mode 0)
(setq linum-format "%4d ")

;; no mail
(global-unset-key (kbd "C-x m"))

;; Packages

;; not supporting < 24
(when (< emacs-major-version 24)
  (error "This configuration is not supported for Emacs version < 24"))

(require 'package)

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;----------------------------------------------------------------------------
;; - Display
;;----------------------------------------------------------------------------

;; display variables
(setq font-lock-maximum-decoration t ;; decoration for fonts
)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Change Font size, only works in GUI
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; bind some window resizing
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

;; setup formatting with not running in terminal
;; (when window-system
;;   (setq frame-title-format '(buffer-file-name "%f" ("%b")))
;;   (tooltip-mode -1)
;;   (mouse-wheel-mode t)
;;   (blink-cursor-mode -1)
;;   (menu-bar-mode 1) ; turn on the menu bar in the gui
;; )

;; fight modeline clutter, need to eval-after-load for whatever you want diminished
(require-package 'diminish)

;; themes
(require-package 'solarized-theme)
;; https://github.com/chriskempson/tomorrow-theme/tree/master/GNU%20Emacs
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes/tomorrow-theme"))
;; default theme
;(load-theme 'solarized-dark t)
(load-theme 'tomorrow-night-bright t)

;; run only in gui
(if window-system
    (setq use-file-dialog nil)
    (setq use-dialog-box nil))

;; function to change opacity
(defun adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;; keybindings to change opacity
(global-set-key (kbd "C-8") '(lambda () (interactive) (adjust-opacity nil -5)))
(global-set-key (kbd "C-9") '(lambda () (interactive) (adjust-opacity nil 5)))
(global-set-key (kbd "C-0") '(lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;;----------------------------------------------------------------------------
;; - Platform specific
;;----------------------------------------------------------------------------

;; set variables based on system type
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *is-gnu-linux* (eq system-type 'gnu/linux))

(when *is-a-mac*
  (setq default-input-method "MacOSX")
  ;; fonts
  ;;(setq default-frame-alist '((font . "Monoco-15")))
  (setq default-frame-alist '((font . "Liberation Mono-15")))

  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\.")))

  ;; Fix shift+up for iterm2
  ;;(lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00211.html)
  (if (equal "xterm" (substring (tty-type) 0 5))
    (define-key input-decode-map "\e[1;2A" [S-up]))



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
  (setq default-frame-alist '((font . "Monospace-12")))
  ;; give me a familiar quit emacs keybinding
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  )

;;----------------------------------------------------------------------------
;; - Navigation
;;----------------------------------------------------------------------------

;; navigation variables
(setq shift-select-mode t         ;; shift arrow to select
      next-line-add-newlines nil  ;; don't add new lines when scrolling down
      scroll-margin 0                   ;; do smooth scrolling, ...
      scroll-conservatively 100000      ;; ... the defaults ...
      scroll-up-aggressively 0          ;; ... are very ...
      scroll-down-aggressively 0        ;; ... annoying
)

; movement keys C-<left> etc don't seem to work in terminal and winner-mode
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <up>") 'windmove-up)

;; keybinding to bring up ibuffer
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

;; ido-mode is like magic pixie dust!
(ido-mode t)  ; use 'buffer rather than t to use only buffer switching

;; Use C-f during file selection to switch to regular find-file
(ido-everywhere t)
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
;        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess ;nil
        ido-auto-merge-work-directories-length 0
        ido-default-buffer-method 'selected-window
        ido-max-prospects 10
        ido-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
          "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
          "_region_" " output\\*$" "^TAGS$" "^\*Ido")
        ido-ignore-directories
        '("\\`auto/" "\\`auto-save-list/" "\\`backups/" "\\`semanticdb/" "\\`target/" "\\`\\.git/" "\\`\\.svn/" "\\`CVS/" "\\`\\.\\./" "\\`\\./")
        ido-ignore-files
        '("\\`auto/" "\\.prv/" "_region_" "\\.class/"  "\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./")))

;; Display ido results vertically, rather than horizontally
;; from http://www.emacswiki.org/emacs/InteractivelyDoThings#toc17
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; update keybindings so up and down move next with vertical results
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
            (define-key ido-completion-map (kbd "<down>") 'ido-next-match)))

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

;; setup recentf mode
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; taken from http://github.com/superbobry/emacs/blob/master/rc/emacs-rc-defuns.el
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; some keybindings I usually forget about
(when (fboundp 'recentf-mode)
  (global-set-key (kbd "M-<f12>") 'recentf-open-files)
  (global-set-key (kbd "C-x f") 'recentf-ido-find-file))

;; Use regex searching by default
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)

;; function for isearch as regex
(defun call-with-current-isearch-string-as-regex (f)
  (let ((case-fold-search isearch-case-fold-search))
    (funcall f (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (call-with-current-isearch-string-as-regex 'occur)))

;; or fire up "all"
(define-key isearch-mode-map (kbd "C-l")
  (lambda ()
    (interactive)
    (call-with-current-isearch-string-as-regex 'all)))

;; Search back/forth for the symbol at point
;; See http://www.emacswiki.org/emacs/SearchAtPoint
(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

;; use this after C-s to search for word under cursor
(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)

;; ido completion in M-x
(require-package 'smex)
(smex-initialize)
(global-set-key "\M-x" 'smex)

;; avoid M-x if possible, https://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

;; a few more shortcuts
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; ECB, not sure how to use it yet
(require-package 'ecb)

;;----------------------------------------------------------------------------
;; - Editing and formatting
;;----------------------------------------------------------------------------
;; editing and formatting variables
(setq x-select-enable-clipboard t       ;; make emacs use the clipboard
      kill-whole-line t                 ;; delete line in one stage
      mouse-yank-at-point t             ;; paste at cursor, NOT at mouse pointer position
      require-final-newline t           ;; end files with a newline
      ;; set ispell to use brew installed aspell, see http://sunny.in.th/2010/05/08/emacs-enabling-flyspell-mode-gave-an-error.html
      ispell-program-name "aspell"
      ;; make backspace mode work correctly, use F1 instead of Ctrl-h when running in the terminal
      normal-erase-is-backspace t
)

;; delete selected on keypress
(delete-selection-mode 1)

;; use spaces to indent
(set-default 'indent-tabs-mode nil)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; some more familiar keybindings for default functions
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key "\r" 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-region)

;; set whitespace style, mode turned on later in run-coding-hook
(setq whitespace-style '(trailing space-before-tab indentation
                             space-after-tab tabs tab-mark)
      c-basic-indent 2
      tab-width 4
      indent-tabs-mode nil)

;; format indention on current buffer
(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  )

;; change all tabs to spaces
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

;; call some formatting function on current buffer
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; Like shift-o in vi
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "C-S-o") 'vi-open-line-above)

;; grabbed from http://blog.tuxicity.se/elisp/emacs/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
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

;; The following two function are taken from textmate.el package by defunkt.
(defun textmate-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))
(global-set-key (kbd "<C-S-right>") 'textmate-shift-right)

(defun textmate-shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (textmate-shift-right (* -1 (or arg 1))))
(global-set-key (kbd "<C-S-left>") 'textmate-shift-left)

;; more of a private function for the next 2
(defun move-line (arg)
  "Moves line up or down, depending on the arg."
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines arg))
    (if (eql arg 1) (forward-line))
    (move-to-column col)))

;; move current line up 1
(defun move-line-up ()
  (interactive)
  (move-line -1))
(global-set-key (kbd "M-<up>") 'move-line-up)

;; move current line down 1
(defun move-line-down ()
  (interactive)
  (move-line 1))
(global-set-key (kbd "M-<down>") 'move-line-down)

;; useful for stuff like autotest to force things to rerun
(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))
(global-set-key (kbd "s-s") 'save-buffer-always)

;; rebind to undo, stop suspending-frame
;;(global-set-key (kbd "C-z") 'undo)

;; Hippie expand: at times perhaps too hip
;;(delete 'try-expand-line hippie-expand-try-functions-list)
;;(delete 'try-expand-list hippie-expand-try-functions-list)
(defun try-complete-abbrev (old)
   (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ))

;;hippie expand binding
(global-set-key (kbd "M-TAB") 'hippie-expand)

;; autopair quotes and parentheses
(require-package 'autopair)
(setq autopair-autowrap t)

;; supercharge undo/redo
(require-package 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "M-?") 'undo-tree-redo)
;; no need to see undo-tree in modeline
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))

;;----------------------------------------------------------------------------
;; - Utility functions
;;----------------------------------------------------------------------------

;; Insert date string
(defun insert-date-string ()
 "Insert a nicely formated date string."
 (interactive)
 (insert (format-time-string "%a %b %d %H:%M:%S %Y")))

;; delete all buffers except scratch
(defun clean-slate ()
    "Kills all buffers except *scratch*"
    (interactive)
    (let ((buffers (buffer-list)) (safe '("*scratch*")))
      (while buffers
        (when (not (member (car buffers) safe))
          (kill-buffer (car buffers))
          (setq buffers (cdr buffers))))))

;; Delete the current file
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Rename the current file
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

;; Browse current HTML file
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))

;; dired+
(require-package 'dired+)
(toggle-diredp-find-file-reuse-dir 1)
(setq dired-recursive-deletes 'top)
(define-key dired-mode-map [mouse-2] 'dired-find-file)

;; Git stuff
;; I hear such good things about magit,
;; but I have invested a lot in learning the git commands
;; Magit is moved to elpa-noload
;;(require-package 'magit)

;; I use git from the terminal and GIT_EDITOR=et, so I need to close
;; the commit message when I am done.  C-x # is the command, which runs
;; server-edit.  But I do that so often, I want an easier key combo
(global-set-key "\C-c\C-w" 'server-edit)

;; load git stuff from git-core contrib/emacs into site-lisp,
;; see http://git.kernel.org/?p=git/git.git;a=tree;hb=HEAD;f=contrib/emacs
;; vc-git.el included with emacs now
(require 'git)
(require 'git-blame)

;; follow symlinks to real file
(setq vc-follow-symlinks t)

;; Project package
(require-package 'projectile)
(projectile-global-mode)

;; Deft, like notational velocity for Emacs
(require-package 'deft)
(setq deft-use-filename-as-title t)

;; install ESS
(require-package 'ess)

;; org-mode
;; active Org-babel languages
(setq org-support-shift-select t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name (concat user-emacs-directory "/site-lisp/plantuml.jar")))

;;----------------------------------------------------------------------------
;; - Language specific
;;----------------------------------------------------------------------------

;; All code
;; --------
;; Set up hook that can be called for progamming modes
(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(defun turn-on-whitespace ()
  (whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun turn-on-hideshow () (hs-minor-mode t))

(defun turn-on-linum () (linum-mode t))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|XXX\\):"
      1 font-lock-warning-face t))))

(defun bye-flyspell ()
  (turn-off-flyspell))

(add-hook 'coding-hook 'turn-on-whitespace)
(add-hook 'coding-hook 'add-watchwords)
(add-hook 'coding-hook 'turn-on-hideshow)
(add-hook 'coding-hook 'turn-on-linum)
(add-hook 'coding-hook 'bye-flyspell)
(add-hook 'coding-hook 'autopair-mode)

(defun run-coding-hook ()
  (interactive)
  (run-hooks 'coding-hook))

;; All Lisps
;; -----
(require-package 'paredit)
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(require-package 'rainbow-delimiters)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; Slime
;; -----
;; slime is in elpa-autoloads.  Using nrepl for clojure
;;
;;(require 'slime)
;;(require 'slime-autoloads)
;; (setq slime-lisp-implementations
;;        '((clisp ("clisp") :coding-system utf-8-unix)
;;          (clojure ,(swank-clojure-cmd) :init swank-clojure-init)
;;          ;; (scheme ("scheme") :coding-system utf-8-unix)
;;          ))
;; (setf slime-default-lisp 'clisp)
;; (setf slime-default-lisp 'clojure)
;;(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;;(slime-setup '(slime-repl))
;;(setq slime-net-coding-system 'utf-8-unix)
;; add paredit to slime
;;(defun slimeify ()
;;  (paredit-mode 1)
;;  (define-key slime-repl-mode-map ;; stop slime from grabbing del
;;  (read-kbd-macro paredit-backward-delete-key)
;;  nil))
;;(add-hook 'slime-repl-mode-hook 'slimeify)

;; Clojure mode
;; ------------
(require-package 'clojure-mode)
;;(require-package 'clojure-test-mode) ;; has slime built in?
(add-hook 'clojure-mode-hook 'run-coding-hook)
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (rainbow-delimiters-mode +1)))
(require-package 'nrepl)

;; Elisp
;; -----
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode +1)))
(add-hook 'emacs-lisp-mode-hook 'run-coding-hook)
;;(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode) ;;this is cool, but it seems to really slow down editing

;; Text mode
;; ---------
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-off-truncate-lines)

;; Latex mode
;; ----------
;; turn off auto fill for latex
(add-hook 'latex-mode-hook 'turn-off-auto-fill)

;; HTML mode
;; ---------
(add-hook 'html-mode-hook 'run-coding-hook)

;; Java mode
;; ---------
(add-hook 'java-mode-hook 'run-coding-hook)

;; JavaScript mode
;; ---------------
(add-hook 'js-mode-hook 'run-coding-hook)

(defun json-pretty-format ()
  "Runs a jsonlint shell script on the region and then indents

  jsonlint is on my path and looks like this
#!/usr/bin/python
import json
import sys

j = json.load(sys.stdin)
print json.dumps(j, sort_keys=True, indent=2)
"
    (interactive)
    (save-excursion
        (shell-command-on-region (point-min) (point-max) "jsonlint -" (buffer-name) t)
        (indent-region begin end)))

;; Ruby mode
;; ---------
(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("buildfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))
(add-to-list 'auto-mode-alist '("Isolate" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.autotest" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile" . ruby-mode))
(add-hook 'ruby-mode-hook 'run-coding-hook)

(require-package 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

(require-package 'rinari)

;; YAML mode
;; ---------
(require-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
      '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'yaml-mode-hook 'run-coding-hook)

;; HAML mode
;; ---------
(require-package 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.jade" . haml-mode))
(add-hook 'haml-mode-hook 'run-coding-hook)

;; Groovy mode
;; -----------
(require-package 'groovy-mode)
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-hook 'groovy-mode-hook 'run-coding-hook)

;; sh mode (bash)
;; ---------
(add-to-list 'auto-mode-alist '(".bats" . sh-mode))
(add-to-list 'auto-mode-alist '(".bashrc" . sh-mode))
(add-to-list 'interpreter-mode-alist '("sh" . sh-mode))
(add-to-list 'interpreter-mode-alist '("bash" . sh-mode))

(add-hook 'sh-mode-hook 'run-coding-hook)
(require-package 'flymake-shell)
(add-hook 'sh-mode-hook 'flymake-shell-load)

;; PHP mode
;; --------
(require-package 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc" . php-mode))
(add-to-list 'auto-mode-alist '("\\.tpl" . php-mode))

(add-hook 'php-mode-hook 'run-coding-hook)

;; Scala mode
;; ----------
(require-package 'scala-mode)
;; ensime doesn't appear to have been update for 24, comment out until I can look at it
;;(require 'ensime) ;; in site-lib from https://github.com/aemoncannon/ensime/downloads

(add-to-list 'auto-mode-alist '(".sbt" . scala-mode))

;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'run-coding-hook)

;; CC mode
;; -------
;; use c-initialization-hook for keybindings
;; see http://www.gnu.org/software/emacs/manual/html_node/ccmode/CC-Hooks.html
(add-hook 'c-mode-hook 'run-coding-hook)
(add-hook 'c++-mode-hook 'run-coding-hook)

;; Arduino mode
;; ------------
;; in site-lisp from https://raw.github.com/bookest/arduino-mode/master/arduino-mode.el
(require 'arduino-mode) ;; in site-lisp
(add-hook 'arduino-mode-hook 'run-coding-hook)

;; Adoc mode (asciidoc)
;; -------------------
(require-package 'adoc-mode)
(add-to-list 'auto-mode-alist (cons "\\.asc\\'" 'adoc-mode))
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))
;; change font face in asciidoc files
(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))
;; turn on spellcheck
(add-hook 'adoc-mode-hook 'turn-on-flyspell)

;; XML mode
;; --------
;;; Use nxml-mode instead of sgml, xml or html mode.
(mapc
 (lambda (pair)
   (if (or (eq (cdr pair) 'xml-mode)
           (eq (cdr pair) 'sgml-mode)
           (eq (cdr pair) 'html-mode))
       (setcdr pair 'nxml-mode)))
 auto-mode-alist)

(add-hook 'nxml-mode-hook 'run-coding-hook)

(defun nxml-pretty-format ()
  "Function to format the current selected region

  calls xmllint in a shell"
    (interactive)
    (save-excursion
        (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
        (nxml-mode)
        (indent-region begin end)))

;; Thrift mode
;; lifted from https://gist.github.com/2470924
(require 'thrift-mode)
(add-hook 'thift-mode-hook 'run-coding-hook)
;; TODO
;; autocomplete
