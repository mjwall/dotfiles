;;; package --- ~/.emacs.d/init.el for mjwall

;;; Commentary:
;;; This is my Emacs init.el file

;;; Code:

;; - Defaults
;;----------------------------------------------------------------------------

;; Turn off mouse interface early in startup to avoid momentary display, can be turned on later if needed
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Make elisp more civilized
(eval-when-compile (require 'cl))

;; default load path
(defvar dotfiles-dir)
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(defvar site-lisp-dir)
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Use a more interesting startup message
(defun startup-echo-area-message ()
  "Go...")

;; default variables
(defvar uniquify-buffer-name-style)
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

;; make emacs revert files when they change, for example
;; when you switch git branches
(global-auto-revert-mode 1)

;; no line numbers unless I say so, but set the format for when I do,
;; coding-hooks will provide line numbers for all code
(global-linum-mode 0)
(eval-after-load "linum"
  '(setq linum-format "%4d "))

;; no mail
(global-unset-key (kbd "C-x m"))

;; Packages

;; not supporting < 24
(when (< emacs-major-version 24)
  (error "This configuration is not supported for Emacs version < 24"))

(require 'package)

;; On-demand installation of packages
;; may have to run package-refresh-contents
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE.
MIN-VERSION optional
NO-REFRESH optional"
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; not sure about marmalade yet
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;----------------------------------------------------------------------------
;; - Display
;;----------------------------------------------------------------------------

;; display variables
;; decoration for fonts
(setq font-lock-maximum-decoration t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Change Font size, only works in GUI
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-_") 'text-scale-decrease)

;; bind some window resizing
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

;; fight modeline clutter, need to eval-after-load for
;; whatever you want diminished
(require-package 'diminish)

;; themes
(require-package 'solarized-theme)
(require-package 'zenburn-theme)
(require-package 'underwater-theme)
(require-package 'ample-theme)
;; https://github.com/chriskempson/tomorrow-theme/tree/master/GNU%20Emacs
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes/tomorrow-theme"))
;; default theme
;;(load-theme 'underwater t)
;;(load-theme 'solarized-dark t)
;;(load-theme 'tomorrow-night-bright t)
;;(load-theme 'ample t)
(require-package 'ample-zen-theme)
(load-theme 'ample-zen t)

;; fix cursor on some linux,
;; see https://github.com/chriskempson/tomorrow-theme/issues/42
;; (add-hook 'window-setup-hook '(lambda () (set-cursor-color "#778899")))
;; (add-hook 'after-make-frame-functions '(lambda (f) (with-selected-frame f (set-cursor-color "#778899"))))

;; run only in gui, doesn't play well with daemon
(if window-system
    (setq use-file-dialog nil)
    (setq use-dialog-box nil))

;; show menu-bar-mode in GUI
(if (display-graphic-p)
    (menu-bar-mode 1)
  (menu-bar-mode -1))

;; function to change opacity
(defun adjust-opacity (frame incr)
  "Function to change the opacity of the FRAME by the given INCR."
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
(defvar *is-a-mac*)
(defvar *is-carbon-emacs*)
(defvar *is-cocoa-emacs*)
(defvar *is-gnu-linux*)
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

  ;; make the modifiers work like linux
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)

  ;; turn off delete frame, I hit that too much
  (global-unset-key (kbd "s-w"))

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

(defun my/swap (l)
  "Swap function use in ido-jump-to-window.  Take first element of L."
  (if (cdr l)
      (cons (cadr l) (cons (car l) (cddr l)))
    l))
(defun ido-jump-to-window ()
  "This ido-jump-to-window function taken from http://www.emacswiki.org/emacs/WindowNavigation."
  (interactive)
  (let* ((visible-buffers
          (my/swap (mapcar #'(lambda (window) (buffer-name (window-buffer window))) (window-list))))
         (buffer-name (ido-completing-read "Window: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (setq window-of-buffer
            (delq nil (mapcar #'(lambda (window)
                                 (if (equal buffer-name (buffer-name (window-buffer window)))
                                     window
                                   nil))
                              (window-list))))
      (select-window (car window-of-buffer)))))
(global-set-key (kbd "\C-x v") 'ido-jump-to-window)
(global-set-key (kbd "\C-x C-v") 'ido-jump-to-window)

;; make Alt-` go to other frame as expected, like s-`
(global-set-key (kbd "M-`") 'other-frame)

;; keybinding to bring up ibuffer
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

;; Use C-f during file selection to switch to regular find-file

(when (> emacs-major-version 21)
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
;        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess ;nil
        ido-auto-merge-work-directories-length 0
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-max-prospects 10
        ido-max-directory-size 100000
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
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
              " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

; imenu with ido
(require-package 'idomenu)
(global-set-key (kbd "C-.") 'idomenu)

;; update keybindings so up and down move next with vertical results
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
            (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            ))

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

;; setup recentf mode
(recentf-mode 1)
(eval-after-load "recentf"
  '(setq recentf-max-saved-items 100))

;; From http://github.com/superbobry/emacs/blob/master/rc/emacs-rc-defuns.el
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

(defun call-with-current-isearch-string-as-regex (f)
  "Takes current selection as F and then search with isearch string."
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

(eval-after-load "isearch"
                 '(diminish 'isearch-mode))

;; show counts of matchs in modeline
(require-package 'anzu)
(global-anzu-mode +1)
(eval-after-load "anzu" '(setq anzu-search-threshold 1000))
(eval-after-load "anzu" '(diminish 'anzu-mode))

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

;; avoid M-x if possible, see
;; https://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

;; a few more shortcuts
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; CEDET, looks like 2.0 is included in Emacs 24.3
;; (global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;; (global-srecode-minor-mode 1)            ; Enable template insertion menu

;; ECB, not sure how to use it yet
;; (require-package 'ecb)

(require-package 'window-numbering) ;;enabled at bottom
;;----------------------------------------------------------------------------
;; - Editing and formatting
;;----------------------------------------------------------------------------
;; editing and formatting variables
(setq
 ;; make emacs use the clipboard
 x-select-enable-clipboard t
 ;; delete line in one stage
 kill-whole-line t
 ;; paste at cursor, NOT at mouse pointer position
 mouse-yank-at-point t
 ;; end files with a newline
 require-final-newline t
 )

(eval-after-load "ispell-mode"
  ;; set ispell to use brew installed aspell, see
  ;; http://sunny.in.th/2010/05/08/emacs-enabling-flyspell-mode-gave-an-error.html
  '(setq ispell-program-name "aspell"))

;; make backspace work as expected
(normal-erase-is-backspace-mode 1)

;; delete selected on keypress
(delete-selection-mode 1)

;; use spaces to indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)


;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; some more familiar keybindings for default functions
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key "\r" 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
;; try to use this instead of M-w, as that closes windows on Mac
(global-set-key (kbd "s-w") 'kill-ring-save)

;; set whitespace style, mode turned on later in run-coding-hook
(setq whitespace-style
      '(trailing space-before-tab indentation
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
  "Move line up or down, depending on the ARG."
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines arg))
    (if (eql arg 1) (forward-line))
    (move-to-column col)))

(defun move-line-up ()
  "Move current line up one."
  (interactive)
  (move-line -1))
(global-set-key (kbd "M-<up>") 'move-line-up)

(defun move-line-down ()
  "Move current line down one."
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
(global-set-key (kbd "C-z") 'undo)

;; Hippie expand: at times perhaps too hip
;;(delete 'try-expand-line hippie-expand-try-functions-list)
;;(delete 'try-expand-list hippie-expand-try-functions-list)
(defun try-complete-abbrev (old)
  "Define a hippie complete function using abbrev using OLD."
   (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ))

;;hippie expand binding
(global-set-key [C-tab] 'hippie-expand)

(require-package 'yasnippet)
(yas-global-mode 1)
(eval-after-load "yasnippet"
  '(progn
    (setq yas/root-directory (concat dotfiles-dir "snippets"))
    (yas/load-directory yas/root-directory)
    (diminish 'yas-minor-mode)))

;;autocomplete
(require-package 'auto-complete)
(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~ / .emacs.d / ac-dict")
(ac-config-default)

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(setq ac-auto-start nil)
(global-set-key "\M-/" 'ac-start)
(define-key ac-complete-mode-map "\M-/" 'ac-stop)
(eval-after-load "auto-complete"
  '(diminish 'auto-complete-mode "ac"))


;; supercharge undo/redo
(require-package 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "M-?") 'undo-tree-redo)
;; no need to see undo-tree in modeline
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))

(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;;----------------------------------------------------------------------------
;; - Utility functions
;;----------------------------------------------------------------------------

;; reload init.el
;; can also open the file and call eval-buffer
(defun reload-init.el ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(defun pop-off-buffer ()
  "Make a new frame with the current buffer, and bury that buffer on the old frame."
  (interactive)
  (bury-buffer)
  (make-frame)
  (unbury-buffer)
  (message"new frame created"))

;; Insert date string
(defun insert-date-string ()
 "Insert a nicely formated date string."
 (interactive)
 (insert (format-time-string "%a %b %d %H:%M:%S %Y")))

;; delete all buffers except scratch
(defun clean-slate ()
    "Kill all buffers except *scratch*."
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

;; not sure why this works on Mac but not Linux
(global-set-key (kbd "s-k") 'kill-this-buffer)

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

;; show full filename, with path
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(put 'dired-find-alternate-file 'disabled nil)

(autoload 'dirtree "dirtree" "Add directory to tree view" t)

;; Git stuff
(require-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; next section from https://github.com/cjohansen/.emacs.d/blob/master/setup-magit.el
(require-package 'magit-svn)

;; Load git configurations
;; For instance, to run magit-svn-mode in a project, do:
;;
;;     git config --add magit.extension svn
;;
(add-hook 'magit-mode-hook 'magit-load-config-extensions)

;; C-x C-k to kill file on line

(defun magit-kill-file-on-line ()
  "Show file on current magit line and prompt for deletion."
  (interactive)
  (magit-visit-item)
  (delete-current-buffer-file)
  (magit-refresh))

(eval-after-load "magit"
  '(define-key magit-status-mode-map (kbd "C-x C-k") 'magit-kill-file-on-line))

;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  "Setup advice for magit status iwth AROUND, MAGIT-FULLSCREEN and ACTIVATE so it can be put back on quit."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Quit magit, restoring the previous window configuration and killing all magit buffers."
  (interactive)
  (mapc (lambda (b)
          (if (string-prefix-p "*magit" (buffer-name b)) (kill-buffer b)))
        (buffer-list))
  ;;(kill-buffer)
  (jump-to-register :magit-fullscreen))

(eval-after-load "magit"
  '(define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; full screen vc-annotate

(defun vc-annotate-quit ()
  "Restore the previous window configuration and kill the \"vc-annotate\" buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

;; ignore whitespace

(eval-after-load "magit"
  '(defun magit-toggle-whitespace ()
    (interactive)
    (if (member "-w" magit-diff-options)
        (magit-dont-ignore-whitespace)
      (magit-ignore-whitespace))))

(eval-after-load "magit"
  '(defun magit-ignore-whitespace ()
    (interactive)
    (add-to-list 'magit-diff-options "-w")
    (magit-refresh)))

(eval-after-load "magit"
  '(defun magit-dont-ignore-whitespace ()
    (interactive)
    (setq magit-diff-options (remove "-w" magit-diff-options))
    (magit-refresh)))

(eval-after-load "magit"
  '(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))
;; end stuff from https://github.com/cjohansen/.emacs.d/blob/master/setup-magit.el

;; help me format better commit messages please
;; https://github.com/lunaryorn/git-modes
(require-package 'git-commit-mode)

;; load git stuff from git-core contrib/emacs into site-lisp,
;; see http://git.kernel.org/?p=git/git.git;a=tree;hb=HEAD;f=contrib/emacs
;; vc-git.el included with emacs now
(add-to-list 'load-path (concat site-lisp-dir "/git.el"))
(require 'git)

(require-package 'mo-git-blame)
;;(require 'git-blame)

;; git-gutter
(require-package 'git-gutter)
(global-git-gutter-mode t)
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(eval-after-load "git-gutter" '(diminish 'git-gutter-mode))

;; eshell
;; call magit from eshell
(defun eshell/magit ()
  "Run magit status here."
  (call-interactively #'magit-status)
  nil)
(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
(setq eshell-history-size 1024)
(setq eshell-prompt-regexp "^[^#$]*[#$] ")
(load "em-hist")           ; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)

(defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))

;;; ---- path manipulation
(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
   (home-len (length home)))
    (if (and
   (>= (length pwd) home-len)
   (equal home (substring pwd 0 home-len)))
  (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat "["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "]") 'face `(:foreground "green"))
      )))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize ((lambda (p-lst)
            (if (> (length p-lst) 3)
                (concat
                 (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                            (substring elm 0 1)))
                            (butlast p-lst 3)
                            "/")
                 "/"
                 (mapconcat (lambda (elm) elm)
                            (last p-lst 3)
                            "/"))
              (mapconcat (lambda (elm) elm)
                         p-lst
                         "/")))
          (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "yellow"))
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (propertize "# " 'face 'default))))

(setq eshell-highlight-prompt nil)

(add-hook 'eshell-mode-hook
         '(lambda ()
         (local-set-key "\C-c\C-q" 'eshell-kill-process)
         (local-set-key "\C-c\C-k" 'compile)
         (local-set-key "\C-xg" 'magit-status)))


;; follow symlinks to real file
(setq vc-follow-symlinks t)

;; Project package
(require-package 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(diminish 'projectile-mode "proj")

;; install ESS
(require-package 'ess)

;; org-mode
(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-directory (concat (getenv "HOME") "/.org/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212121" "#CC5542" "#6aaf50" "#7d7c61" "#5180b3" "#DC8CC3" "#9b55c3" "#bdbdb3"])
 '(custom-safe-themes (quote ("5dfacaf380068d9ed06e0872a066a305ab6a1217f25c3457b640e76c98ae20e6" "7df5b36ef661649550614a15e9afb9d3e785706be6a577058f1b440dff1b03e3" default)))
 '(deft-auto-save-interval 30)
 '(deft-directory (concat org-directory "deft"))
 '(deft-extension "org")
 '(deft-text-mode (quote org-mode))
 '(deft-use-filename-as-title t)
 '(fci-rule-color "#2e2e2e")
 '(org-agenda-custom-commands (quote (("1" "Today's agenda" ((agenda "" ((org-agenda-ndays 1))))) ("n" "Week agenda + TODOs" ((agenda "") (todo))))))
 '(org-agenda-files (list (concat org-directory "work.org") (concat org-directory "personal.org") (concat org-directory "someday.org") (concat org-directory "inbox.org") (concat org-directory "journal.org") (concat org-directory "notes.org")))
 '(org-agenda-include-diary t)
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-capture-templates (quote (("t" "Todo" entry (file+headline (concat org-directory "inbox.org") "Tasks") "* TODO %?
  %i
  %a") ("j" "Journal" entry (file+datetree (concat org-directory "journal.org")) "* %?
Entered on %U
  %i
  %a") ("n" "Note" entry (file (concat org-directory "notes.org")) "* %? :NOTE:
%U
%a
") ("s" "Someday" entry (file (concat org-directory "someday.org")) "* %? :SOMEDAY:
%U
%a
"))))
 '(org-completion-use-ido t)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file (concat org-directory "inbox.org"))
 '(org-indirect-buffer-display (quote current-window))
 '(org-log-done (quote note))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-targets (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))))
 '(org-refile-use-outline-path t)
 '(org-reverse-note-order t)
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map (quote ((20 . "#dd5542") (40 . "#CC5542") (60 . "#fb8512") (80 . "#baba36") (100 . "#bdbc61") (120 . "#7d7c61") (140 . "#6abd50") (160 . "#6aaf50") (180 . "#6aa350") (200 . "#6a9550") (220 . "#6a8550") (240 . "#6a7550") (260 . "#9b55c3") (280 . "#6CA0A3") (300 . "#528fd1") (320 . "#5180b3") (340 . "#6380b3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
;; (defun bh/verify-refile-target ()
;;   "Exclude todo keywords with a done state from refile targets"
;;   (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; (setq org-refile-target-verify-function 'bh/verify-refile-target)

;; active Org-babel languages, particularly for PlantUML
(setq org-support-shift-select t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))
(setq org-plantuml-jar-path
      (expand-file-name (concat user-emacs-directory "/site-lisp/plantuml.jar")))

(defun sync-org-files ()
  "Run a bash script located in org-directory to sync org files via git."
  (interactive)
  (org-save-all-org-buffers)
  (message "Syncing org files")
  (with-output-to-temp-buffer "*org-git-sync*"
    (shell-command (concat org-directory "org-git-sync") "*org-git-sync*")
    (pop-to-buffer "*org-git-sync*"))
  (message "Done syncing org files"))


;; Deft, like notational velocity for Emacs
;; I prefer more free flowing notes that get into my agenda
(require-package 'deft)


(require 'tramp-term)

;; just give some indication, maybe this should be in the modeline
;; but I don't know how to do that
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

;; these don't appear to be in term, so copied from multi-term
(defun my-term-send-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))
(defun my-term-send-forward-word ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))

;; better term keybindings
(add-hook 'ansi-term-after-hook
  (lambda ()
    ;; char-mode-map
    (define-key term-raw-map (kbd "C-y") 'term-paste)
    (define-key term-raw-map (kbd "C-c C-c") 'term-interrupt-subjob)
    (define-key term-raw-map (kbd "C-s") 'isearch-forward)
    (define-key term-raw-map (kbd "C-r") 'isearch-backward)
    ; I don't really like these, means I have to use arrows to scoll history
    ;(define-key term-raw-map (kbd "C-p") 'previous-line)
    ;(define-key term-raw-map (kbd "C-n") 'next-line)
    (define-key term-raw-map (kbd "M-f") 'my-term-send-forward-word)
    (define-key term-raw-map (kbd "M-b") 'my-term-send-backward-word)
    (define-key term-raw-map (kbd "C-c C-j") 'my-term-line-mode)
    (define-key term-raw-map (kbd "M-DEL") 'term-send-backward-kill-word)
    (define-key term-raw-map (kbd "M-d") 'term-send-forward-kill-word)
    (define-key term-raw-map (kbd "C-r") 'term-send-reverse-search-history)
    (define-key term-raw-map (kbd "M-w") 'kill-ring-save)
    ;; line-mode map
    (define-key term-mode-map (kbd "C-c C-k") 'my-term-char-mode)
    ))
(defadvice ansi-term (after ansi-term-after-advice (arg))
  "Run hook as after advice for \"ansi-term\"."
  (run-hooks 'ansi-term-after-hook))
(ad-activate 'ansi-term)

;; could have just set a variable, but maybe I will expand this
(defun my-ansi-term ()
  "My \"ansi-term\" function."
  (interactive)
  (ansi-term "/bin/bash"))
(global-set-key [f6] 'my-ansi-term)

(require-package 'rainbow-mode)
;;----------------------------------------------------------------------------
;; - Language specific
;;----------------------------------------------------------------------------

;; All code
;; --------
;; Set up hook that can be called for progamming modes
(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(defun turn-on-whitespace ()
  "Turn on whitespace."
  (whitespace-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun turn-on-hideshow ()
  "Turn on hideshow."
  (hs-minor-mode t))

(defun turn-on-linum ()
  "Turn on linum."
  (linum-mode t))

(defun add-watchwords ()
  "Add watchwords."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|XXX\\):"
      1 font-lock-warning-face t))))

(defun bye-flyspell ()
  "Turn off flyspell."
  (turn-off-flyspell))

(add-hook 'coding-hook 'turn-on-whitespace)
(add-hook 'coding-hook 'add-watchwords)
(add-hook 'coding-hook 'turn-on-hideshow)
;(add-hook 'coding-hook 'turn-on-linum)
(add-hook 'coding-hook 'bye-flyspell)
(add-hook 'coding-hook 'electric-pair-mode)
(add-hook 'coding-hook 'electric-indent-mode)

(defun run-coding-hook ()
  "Run my coding hook."
  (interactive)
  (run-hooks 'coding-hook))

(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; All Lisps
;; -----
(require-package 'paredit)
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)
(eval-after-load "paredit" '(diminish 'paredit-mode "pe"))

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; Slime
;; -----
;; slime is in elpa-noload.  Using nrepl for clojure
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
(require-package 'cider)
;;(require-package 'nrepl)
(require-package 'cider)
;;(require 'lein)

;; Elisp
;; -----
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode +1)))
(add-hook 'emacs-lisp-mode-hook 'run-coding-hook)
;;(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode) ;;this is cool, but it seems to really slow down editing
(add-hook
 'emacs-lisp-mode-hook
 (lambda () (setq mode-name "el")))

;; from http://nullprogram.com/blog/2010/06/10/
(add-hook 'ielm-mode-hook (lambda () (paredit-mode 1)))
(defadvice ielm-eval-input (after ielm-paredit activate)
  "Begin each IELM prompt with a ParEdit parenthesis pair."
  (paredit-open-round)) ; backspace if you don't want it

(defun imenu-elisp-sections ()
  "Define section in elisp imenu."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;; - \\(.+\\)$" 1) t))
 (add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

;; Text mode
;; ---------
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

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

;; JDEE mode - time to try it
;; --------------------------
(add-to-list 'load-path "~/.emacs.d/jdee-2.4.1/lisp")
(load "jde")
(require 'jde-maven)

;; eclim instead of JDEE
;; eclim_2.3.2 and 
;; Kepler Eclipse Java EE IDE for Web Developers.
;;
;; Version: Kepler Service Release 1
;; Build id: 20130919-0819
;;(require 'eclim)
;;(global-eclim-mode)
;;(require 'eclimd)
;;(require 'ac-emacs-eclim-source)
;;(ac-emacs-eclim-config)

;; Javascript mode
;; ---------------
(add-hook 'js-mode-hook 'run-coding-hook)
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
        (shell-command-on-region (point-min) (point-max) "jsonlint -" (buffer-name) t)
        (indent-region begin end)))

;; Ruby mode
;; ---------
(require 'ruby-mode)
(require-package 'ruby-end)
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
                               (require 'inf-ruby)
                               (require 'ruby-compilation)))
(add-hook 'ruby-mode-hook 'run-coding-hook)
(add-hook 'ruby-mode-hook 'ruby-end-mode)

;(require-package 'rvm)
;(rvm-use-default) ;; use rvm's default ruby for the current Emacs session
(require-package 'rbenv)
(setq rbenv-installation-dir "~/.rbenv") ;default, but here in case
(global-rbenv-mode)

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
(add-hook 'sh-mode-hook
          '(lambda ()
             (setq sh-basic-offset 2
                   sh-indentation 2)))
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
(require-package 'scala-mode2)
(require-package 'sbt-mode)

;; awesomeness for scala, but doesn't appear to be updated for emacs 24
;;(add-to-list 'load-path (concat site-lisp-dir "/ensime/elisp/"))
;; in site-lib from https://github.com/aemoncannon/ensime/downloads
;;(require 'ensime)
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(require-package 'ensime)

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

;; Asciidoc stuff
;; -------------------
(add-to-list 'load-path (concat site-lisp-dir "/doc-mode-1.1"))
(autoload 'doc-mode "doc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . doc-mode))
(add-hook 'doc-mode-hook
          '(lambda ()
             (turn-off-auto-fill)
             (turn-on-flyspell)
             (require 'asciidoc)
             ))



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
  "Function to format the current selected region.

  calls xmllint in a shell"
    (interactive)
    (save-excursion
        (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
        (nxml-mode)
        (indent-region begin end)))

;; autocomplete tags on </
(setq nxml-slash-auto-complete-flag t)

;; Thrift mode
;; lifted from https://gist.github.com/2470924
(require 'thrift-mode)
(add-hook 'thift-mode-hook 'run-coding-hook)

;; Markdown mode
(require-package 'markdown-mode)

;; web mode
(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Python
(require-package 'jedi)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

(window-numbering-mode 1)

;; org file sync stuff
;; run now on startup
(sync-org-files)
;; and on shutdown
(add-hook 'kill-emacs-hook 'sync-org-files)

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
