;;; ensime-mode.el --- ensime mode

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(defvar ensime-source-buffer-saved-hook nil
  "Hook called whenever an ensime source buffer is saved.")

(defvar ensime-source-buffer-loaded-hook nil
  "Hook called whenever an ensime source buffer is loaded.")

(defvar ensime-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))

      (define-key prefix-map (kbd "C-v i") 'ensime-inspect-type-at-point)
      (define-key prefix-map (kbd "C-v 5 i")
	'ensime-inspect-type-at-point-other-frame)
      (define-key prefix-map (kbd "C-v p") 'ensime-inspect-package-at-point)
      (define-key prefix-map (kbd "C-v o") 'ensime-inspect-project-package)
      (define-key prefix-map (kbd "C-v r") 'ensime-show-uses-of-symbol-at-point)
      (define-key prefix-map (kbd "C-v s") 'ensime-sbt-switch)
      (define-key prefix-map (kbd "C-v z") 'ensime-inf-switch)
      (define-key prefix-map (kbd "C-v f") 'ensime-format-source)
      (define-key prefix-map (kbd "C-v u") 'ensime-undo-peek)
      (define-key prefix-map (kbd "C-v v") 'ensime-search)
      (define-key prefix-map (kbd "C-v d") 'ensime-show-doc-for-symbol-at-point)
      (define-key prefix-map (kbd "C-v D") 'ensime-project-docs)
      (define-key prefix-map (kbd "C-v t") 'ensime-type-at-point)
      (define-key prefix-map (kbd "C-v e") 'ensime-print-errors-at-point)
      (define-key prefix-map (kbd "C-v .") 'ensime-expand-selection-command)

      (define-key prefix-map (kbd "C-v C-r") 'ensime-inf-eval-region)
      (define-key prefix-map (kbd "C-v b") 'ensime-inf-eval-buffer)
      (define-key prefix-map (kbd "C-v l") 'ensime-inf-load-file)

      (define-key prefix-map (kbd "C-c c") 'ensime-typecheck-current-buffer)
      (define-key prefix-map (kbd "C-c a") 'ensime-typecheck-all)
      (define-key prefix-map (kbd "C-c r") 'ensime-reload-open-files)
      (define-key prefix-map (kbd "C-c e") 'ensime-show-all-errors-and-warnings)

      (define-key prefix-map (kbd "C-t t") 'ensime-goto-test)
      (define-key prefix-map (kbd "C-t i") 'ensime-goto-impl)

      (define-key prefix-map (kbd "C-d a") 'ensime-db-attach)
      (define-key prefix-map (kbd "C-d b") 'ensime-db-set-break)
      (define-key prefix-map (kbd "C-d u") 'ensime-db-clear-break)
      (define-key prefix-map (kbd "C-d s") 'ensime-db-step)
      (define-key prefix-map (kbd "C-d o") 'ensime-db-step-out)
      (define-key prefix-map (kbd "C-d n") 'ensime-db-next)
      (define-key prefix-map (kbd "C-d r") 'ensime-db-run)
      (define-key prefix-map (kbd "C-d c") 'ensime-db-continue)
      (define-key prefix-map (kbd "C-d q") 'ensime-db-quit)
      (define-key prefix-map (kbd "C-d i") 'ensime-db-inspect-value-at-point)
      (define-key prefix-map (kbd "C-d t") 'ensime-db-backtrace)
      (define-key prefix-map (kbd "C-d a") 'ensime-db-clear-all-breaks)

      (define-key prefix-map (kbd "C-b s") 'ensime-sbt-switch)
      (define-key prefix-map (kbd "C-b S") 'ensime-stacktrace-switch)
      (define-key prefix-map (kbd "C-b c") 'ensime-sbt-do-compile)
      (define-key prefix-map (kbd "C-b n") 'ensime-sbt-do-clean)
      (define-key prefix-map (kbd "C-b E") 'ensime-sbt-do-ensime-config)
      (define-key prefix-map (kbd "C-b o") 'ensime-sbt-do-test-only-dwim)
      (define-key prefix-map (kbd "C-b p") 'ensime-sbt-do-package)
      (define-key prefix-map (kbd "C-b r") 'ensime-sbt-do-run)
      (define-key prefix-map (kbd "C-b t") 'ensime-sbt-do-test-dwim)
      (define-key prefix-map (kbd "C-b q") 'ensime-sbt-do-test-quick-dwim)

      (define-key prefix-map (kbd "C-r a") 'ensime-refactor-add-type-annotation)
      (define-key prefix-map (kbd "C-r r") 'ensime-refactor-diff-rename)
      (define-key prefix-map (kbd "C-r o") 'ensime-refactor-diff-organize-imports)
      (define-key prefix-map (kbd "C-r l") 'ensime-refactor-diff-extract-local)
      (define-key prefix-map (kbd "C-r m") 'ensime-refactor-diff-extract-method)
      (define-key prefix-map (kbd "C-r i") 'ensime-refactor-diff-inline-local)
      (define-key prefix-map (kbd "C-r t") 'ensime-import-type-at-point)

      (define-key map ensime-mode-key-prefix prefix-map)

      ;; Prefix-less shortcuts bindings...
      (define-key map (kbd "M-.") 'ensime-edit-definition)
      (define-key map (kbd "M-,") 'ensime-pop-find-definition-stack)

      (define-key map (kbd "M-n") 'ensime-forward-note)
      (define-key map (kbd "M-p") 'ensime-backward-note)

      (define-key map [C-down-mouse-1] 'ignore)
      (define-key map [C-up-mouse-1] 'ignore)
      (define-key map [C-down-mouse-3] 'ignore)
      (define-key map [C-up-mouse-3] 'ignore)
      (define-key map [C-mouse-1] 'ensime-control-mouse-1-single-click)
      (define-key map [C-mouse-3] 'ensime-control-mouse-3-single-click)
      )

    map)
  "Keymap for ENSIME mode."
  )

;;;;; ensime-mode
(defun ensime-scala-mode-hook ()
  "Conveniance hook function that just starts ensime-mode."
  (ensime-mode 1))

(defun ensime-run-after-save-hooks ()
  "Things to run whenever a source buffer is saved."
  (when (ensime-source-file-p)
    (when (and (ensime-connected-p) (ensime-analyzer-ready))
      (condition-case err-info
          (run-hooks 'ensime-source-buffer-saved-hook)
        (error
         (message
          "Error running ensime-source-buffer-saved-hook: %s"
          err-info))))))

(defun ensime-run-find-file-hooks ()
  "Things to run whenever a source buffer is opened."
  (when (ensime-source-file-p)
    (when (and (ensime-connected-p) (ensime-analyzer-ready))
      (condition-case err-info
          (run-hooks 'ensime-source-buffer-loaded-hook)
        (error
         (message
          "Error running ensime-source-buffer-loaded-hook: %s"
          err-info))))))

(defun ensime-save-buffer-no-hooks ()
  "Just save the buffer per usual, don't type-check!"
  (let ((after-save-hook nil)
        (before-save-hook nil))
    (save-buffer)))

(defun ensime-delete-buffer-and-file ()
  "Kill the current buffer and delete the corresponding file!"
  (interactive)
  (ensime-assert-buffer-saved-interactive
   (let ((f buffer-file-name))
     (ensime-rpc-remove-file f)
     (delete-file f)
     (kill-buffer nil)
     )))

(easy-menu-define ensime-mode-menu ensime-mode-map
  "Menu for ENSIME mode"
  '("ENSIME"
    ("Test")

    ("Source"
     ["Format source" ensime-format-source]
     ["Find all references" ensime-show-uses-of-symbol-at-point]
     ["Inspect type" ensime-inspect-type-at-point]
     ["Inspect type in another frame" ensime-inspect-type-at-point-other-frame]
     ["Inspect enclosing package" ensime-inspect-package-at-point]
     ["Inspect project package" ensime-inspect-project-package]
     ["Undo source change" ensime-undo-peek])

    ("Typecheck"
     ["Typecheck file" ensime-typecheck-current-buffer]
     ["Typecheck project" ensime-typecheck-all]
     ["Reload typechecker" ensime-reload-open-files]
     ["Show all errors and warnings" ensime-show-all-errors-and-warnings])

    ("Refactor"
     ["Add type annotation" (ensime-refactor-add-type-annotation)]
     ["Organize imports" (ensime-refactor-diff-organize-imports)]
     ["Import type at point" ensime-import-type-at-point]
     ["Rename" (ensime-refactor-diff-rename)]
     ["Extract local val" (ensime-refactor-diff-extract-local)]
     ["Extract method" (ensime-refactor-diff-extract-method)]
     ["Inline local val" (ensime-refactor-diff-inline-local)])

    ("Navigation"
     ["Lookup definition" ensime-edit-definition]
     ["Lookup definition in other window" ensime-edit-definition-other-window]
     ["Lookup definition in other frame" ensime-edit-definition-other-frame]
     ["Go to test class" ensime-goto-test]
     ["Go to implementation class" ensime-goto-impl]
     ["Pop definition stack" ensime-pop-find-definition-stack]
     ["Backward compilation note" ensime-backward-note]
     ["Forward compilation note" ensime-forward-note]
     ["Expand selection" ensime-expand-selection-command]
     ["Search" ensime-search])

    ("Documentation"
     ["Browse documentation of symbol" ensime-show-doc-for-symbol-at-point]
     ["Browse all documentation" ensime-project-docs])

    ("SBT"
     ["Start or switch to" ensime-sbt-switch]
     ["Compile" ensime-sbt-do-compile]
     ["Clean" ensime-sbt-do-clean]
     ["Test" ensime-sbt-do-test]
     ["Test Quick" ensime-sbt-do-test-quick]
     ["Test current class" ensime-sbt-do-test-only]
     ["Run" ensime-sbt-do-run]
     ["Package" ensime-sbt-do-package])

    ("Debugger"
     ["Attach" ensime-db-attach]
     ["Set break point" ensime-db-set-break]
     ["Clear breakpoint" ensime-db-clear-break]
     ["Clear all breakpoints" ensime-db-clear-all-breaks]
     ["Step" ensime-db-step]
     ["Next" ensime-db-next]
     ["Run" ensime-db-run]
     ["Continue" ensime-db-continue]
     ["Quit" ensime-db-quit]
     ["Show Backtrace" ensime-db-backtrace]
     ["Inspect value at point" ensime-db-inspect-value-at-point]
     )

    "---"
    ["Go to SBT console" ensime-sbt-switch]
    ["Go to stacktrace buffer" ensime-stacktrace-switch]
    ["Go to Scala REPL" ensime-inf-switch]
    ["Shutdown ENSIME server" ensime-shutdown]
    ))

(define-minor-mode ensime-mode
  "ENSIME: The ENhanced Scala Interaction Mode for Emacs (minor-mode).
\\{ensime-mode-map}"
  nil
  nil
  ensime-mode-map

  (if ensime-mode
      (progn

        (pcase ensime-completion-style
          (`company
           (ensime-company-enable))
          (`auto-complete
           (ensime-ac-enable)
           (add-hook 'completion-at-point-functions
                     'ensime-completion-at-point-function nil t))
          (_ t))

        (easy-menu-add ensime-mode-menu ensime-mode-map)

        (add-hook 'after-save-hook 'ensime-run-after-save-hooks nil t)

	(add-hook 'find-file-hook 'ensime-run-find-file-hooks nil t)

        (add-hook 'ensime-source-buffer-saved-hook
                  'ensime-typecheck-current-buffer)

        (add-hook 'ensime-source-buffer-saved-hook
                  'ensime-sem-high-refresh-hook t)

        (add-hook 'ensime-source-buffer-loaded-hook
                  'ensime-sem-high-refresh-hook t)

        (add-hook 'ensime-source-buffer-loaded-hook
                  'ensime-typecheck-current-buffer)

        (add-hook 'after-change-functions
                  'ensime-after-change-function nil t)

        (add-hook 'window-configuration-change-hook
                  'ensime-show-left-margin-hook)

        (ensime-idle-typecheck-set-timer)

        (when ensime-tooltip-hints
          (add-hook 'tooltip-functions 'ensime-tooltip-handler)
          (make-local-variable 'track-mouse)
          (setq track-mouse t)
          (make-local-variable 'tooltip-delay)
          (setq tooltip-delay 1.0)
          (define-key ensime-mode-map [mouse-movement] 'ensime-mouse-motion))

        (ensime-refresh-all-note-overlays)

	(when (equal major-mode 'scala-mode)
	  (ensime--setup-imenu)))
    (progn
      (pcase ensime-completion-style
        (`auto-complete
         (ensime-ac-disable))
        (_ t))

      (remove-hook 'after-save-hook 'ensime-run-after-save-hooks t)

      (remove-hook 'find-file-hook 'ensime-run-find-file-hooks t)

      (remove-hook 'ensime-source-buffer-saved-hook
                   'ensime-typecheck-current-buffer)

      (remove-hook 'ensime-source-buffer-saved-hook
                   'ensime-sem-high-refresh-hook)

      (remove-hook 'ensime-source-buffer-loaded-hook
                   'ensime-sem-high-refresh-hook)

      (remove-hook 'ensime-source-buffer-loaded-hook
                   'ensime-typecheck-current-buffer)

      (remove-hook 'after-change-functions
                   'ensime-after-change-function t)

      (remove-hook 'window-configuration-change-hook
                   'ensime-show-left-margin-hook)

      (remove-hook 'tooltip-functions 'ensime-tooltip-handler)
      (make-local-variable 'track-mouse)
      (setq track-mouse nil)

      (when (equal major-mode 'scala-mode)
	(ensime--unset-imenu)))))

;;;;;; Mouse handlers

(defun ensime-control-mouse-1-single-click (event)
  "Command handler for control+clicks of mouse button 1.
   If control is held, jump to definition of symbol under
   point."
  (interactive "e")
  (mouse-set-point event)
  (ensime-edit-definition))

(defun ensime-control-mouse-3-single-click (event)
  "Command handler for double clicks of mouse button 1.
   If the user clicks on a package declaration or import,
   inspect that package. Otherwise, try to inspect the type
   of the thing at point."
  (interactive "e")
  (ensime-inspect-type-at-point))


(defun ensime-mouse-motion (event)
  "Command handler for mouse movement events in `ensime-mode-map'."
  (interactive "e")
  (tooltip-hide)
  (when (car (mouse-pixel-position))
    (setq tooltip-last-mouse-motion-event (copy-sequence event))
    (tooltip-start-delayed-tip)))

(defun ensime--setup-imenu ()
  "Setup imenu function and make imenu rescan index with every call."
  (set (make-local-variable 'backup-imenu-auto-rescan) imenu-auto-rescan)
  (set (make-local-variable 'backup-imenu-create-index-function) imenu-create-index-function)
  (set (make-local-variable 'imenu-auto-rescan) t)
  (set (make-local-variable 'imenu-create-index-function) #'ensime-imenu-index-function))

(defun ensime--unset-imenu ()
  "Revert ensime specific imenu settings."
  (setq imenu-auto-rescan backup-imenu-auto-rescan)
  (setq imenu-create-index-function backup-imenu-create-index-function))

;;;;;; Tooltips


(defun ensime-tooltip-show-message (msg)
  "Display tooltip, respecting ensime tooltip options."
  (if ensime-graphical-tooltips
      (tooltip-show msg tooltip-use-echo-area)
    (message msg)))


(defun ensime-tooltip-handler (event)
  "Hook function to display a help tooltip. If an error
   or warning overlay exists at point, show the description
   of that error or warning. Otherwise try to inspect the
   type of the expression under the cursor."

  (when (and (eventp event)
             ensime-mode
             (ensime-connected-p)
             (ensime-analyzer-ready)
             (posn-point (event-end event)))


    (let* ((point (posn-point (event-end event)))
           (external-pos (ensime-externalize-offset point))
           (ident (tooltip-identifier-from-point point))
           (note-overlays (ensime-overlays-at point))
           (sem-high-overlays (ensime-sem-high-sym-types-at-point point))
           (val-at-pt (ensime-db-tooltip point)))

      (cond

       ;; If debugger is active and we can get the value of the symbol
       ;; at the point, show it in the tooltip.
       (val-at-pt (ensime-tooltip-show-message val-at-pt) t)

       ;; If error or warning overlays exist,
       ;; show that message..
       (note-overlays (progn
                        (ensime-tooltip-show-message
                         (overlay-get (car note-overlays) 'help-echo))
                        t))

       ;; Show implicit conversions if present
       ((or (find 'implicitConversion sem-high-overlays)
            (find 'implicitParams sem-high-overlays))
        (ensime-tooltip-show-message
         (mapconcat 'identity (ensime-implicit-notes-at point) "\n")))

       ;; Otherwise show a type hint..
       ((and ident ensime-tooltip-type-hints)
        (progn
          (ensime-eval-async
           `(swank:type-at-point ,buffer-file-name ,external-pos)
           #'(lambda (type)
               (when type
                 (let ((msg (ensime-type-full-name-with-args type)))
                   (ensime-tooltip-show-message msg)
                   ))))
          t
          )))
      )))




;;;;;; Modeline

;; Setup the custom ensime modeline handler
(add-to-list 'minor-mode-alist
             '(ensime-mode (:eval (ensime-modeline-string))))

(defun ensime-modeline-string ()
  "Return the string to display in the modeline.
  \"ENSIME\" only appears if we aren't connected.  If connected, include
  connection-name, and possibly some state
  information."
  (when ensime-mode
    (condition-case err
	(let ((conn (ensime-connection-or-nil)))
	  (cond ((and ensime-mode (not conn))
		 (cond
		  ((ensime-owning-server-process-for-source-file buffer-file-name)
		   " [ENSIME: (Starting)]")
		  (t " [ENSIME: (Disconnected)]")))

		((and ensime-mode (ensime-connected-p conn))
		 (concat " ["
			 (or (plist-get (ensime-config conn) :name)
			     "ENSIME: (Connected)")
			 (let ((status (ensime-modeline-state-string conn))
			       (unready (not (ensime-analyzer-ready conn))))
			   (cond (status (concat " (" status ")"))
				 (unready " (Analyzing)")
				 (t "")))
			 (concat (format " : %s/%s"
					 (ensime-num-errors conn)
					 (ensime-num-warnings conn)))
			 "]"))
		(ensime-mode " [ENSIME: (Dead Connection)]")
		))
      (error (progn
	       " [ENSIME: wtf]"
	       )))))

(defun ensime-modeline-state-string (conn)
  "Return a string possibly describing CONN's state."
  (cond ((not (eq (process-status conn) 'open))
	 (format "%s" (process-status conn)))
	((let ((pending (length (ensime-rex-continuations conn))))
	   (cond ((zerop pending) nil)
		 (t (format "%s" pending)))))))

(provide 'ensime-mode)

;; Local Variables:
;; End:
