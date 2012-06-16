(ansi-color-for-comint-mode-on)

;; from http://snarfed.org/why_i_run_shells_inside_emacs
(require 'tramp)

(custom-set-variables
 '(tramp-default-method "ssh")          ; uses ControlMaster
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 ;; '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-buffer-maximum-size 20000)    ; max length of the buffer in lines
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () "")) ; what to run when i press enter on a
                                        ; line above the current prompt
 '(comint-input-ring-size 5000)         ; max shell history size
 '(protect-buffer-bury-p nil)
)

;(setenv "PAGER" "cat") ; slows down tig

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun make-my-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  (if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

;; (defun my-dirtrack-mode ()
;;   "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
;;   (when (member (buffer-name) my-shells)
;;     (shell-dirtrack-mode 0)
;;     (set-variable 'dirtrack-list '(":\\([^ :>]*\\)> *$" 1 nil))
;;     (dirtrack-mode 1)))
;; (add-hook 'shell-mode-hook 'my-dirtrack-mode)

; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))
(add-hook 'shell-mode-hook 'set-scroll-conservatively)

;; i think this is wrong, and it buries the shell when you run emacsclient from
;; it. temporarily removing.
;; (defun unset-display-buffer-reuse-frames ()
;;   "Add to shell-mode-hook to prevent switching away from the shell buffer
;; when emacsclient opens a new buffer."
;;   (set (make-local-variable 'display-buffer-reuse-frames) t))
;; (add-hook 'shell-mode-hook 'unset-display-buffer-reuse-frames)

;; make it harder to kill my shell buffers
;;(require 'protbuf)
;;(add-hook 'shell-mode-hook 'protect-process-buffer-from-kill-mode)

(defun make-comint-directory-tracking-work-remotely ()
  "Add this to comint-mode-hook to make directory tracking work
while sshed into a remote host, e.g. for remote shell buffers
started in tramp. (This is a bug fix backported from Emacs 24:
http://comments.gmane.org/gmane.emacs.bugs/39082"
  (set (make-local-variable 'comint-file-name-prefix)
       (or (file-remote-p default-directory) "")))
(add-hook 'comint-mode-hook 'make-comint-directory-tracking-work-remotely)

(defun enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((member (buffer-name) my-shells) (comint-send-input)))))
(add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

(defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
      (progn (fset 'message 'ignore) ad-do-it)
    (fset 'message old-message))))

(defadvice comint-send-input (around go-to-end-of-multiline activate)
  "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
  (flet ((end-of-line () (end-of-buffer)))
    ad-do-it))

;; not sure why, but comint needs to be reloaded from the source (*not*
;; compiled) elisp to make the above advise stick.
(load "comint.el.gz")

;; my old stuff
;;(setq multi-term-program "/bin/bash")
;;(global-set-key "\C-x\C-z" 'multi-term)
;;(global-set-key (kbd "C-c t") 'multi-term-next)
;;(setq multi-term-scroll-show-maximum-output t)

(setq term-default-bg-color nil)
(setq term-default-fg-color nil)

(defun my-term-yank ()
  (interactive)
  (if (term-in-line-mode)
      (yank)
      (term-send-down)))

(add-hook 'ansi-term-after-hook
  (lambda ()
    (define-key term-raw-map "\C-y" 'term-paste)
    (define-key term-raw-map "\M-w" 'my-term-yank) ;; term-send-raw-meta?
    ;;(define-key term-raw-map "\C-x\C-c" 'save-buffers-kill-emacs) ;; Command-q on Mac
    (define-key term-raw-map "\C-c\C-c" 'term-interrupt-subjob)
    ))

(eval-after-load "term"
  '(progn
     (defun term-send-backspace () (interactive) (term-send-raw-string "\C-h"))))

(defadvice ansi-term (after ansi-term-after-advice (arg))
  "run hook as after advice"
  (run-hooks 'ansi-term-after-hook))

(ad-activate 'ansi-term)


(defun my-set-buffer-name-to-directory ()
  (interactive)
  (message "%s" "changing buffer name")
  (rename-buffer (concat "*" default-directory "*") 1)
  )

(defun my-ansi-term ()
  (interactive)
  (ansi-term "/bin/bash")
  )

(global-set-key "\C-x\C-z" 'my-ansi-term)

(provide 'init-shell)
