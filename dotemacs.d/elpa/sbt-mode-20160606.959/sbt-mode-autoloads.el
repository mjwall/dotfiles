;;; sbt-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "sbt-mode" "sbt-mode.el" (22374 36569 0 0))
;;; Generated autoloads from sbt-mode.el

(autoload 'sbt-start "sbt-mode" "\
Start sbt

\(fn)" t nil)

(autoload 'run-scala "sbt-mode" "\
Pop up Scala REPL buffer.

If the sbt buffer is not in REPL mode, it will switch to REPL mode (console).

\(fn)" t nil)

(autoload 'sbt-command "sbt-mode" "\
Send a command to the sbt process of the current buffer's sbt project.
Prompts for the command to send when in interactive mode. You can
use tab completion.

This command does the following:
  - displays the buffer moving focus to it if focus is t
  - erases the buffer
  - forgets about compilation errors

The command is most usefull for running a compilation command
that outputs errors.

\(fn COMMAND &optional FOCUS)" t nil)

(autoload 'sbt-run-previous-command "sbt-mode" "\
Repeat the command that was previously executed (or run the
sbt:default-command, if no other command has yet been run).

\(fn)" t nil)

;;;***

;;;### (autoloads nil "sbt-mode-rgrep" "sbt-mode-rgrep.el" (22374
;;;;;;  36569 0 0))
;;; Generated autoloads from sbt-mode-rgrep.el

(autoload 'sbt-grep "sbt-mode-rgrep" "\
Recursively grep for REGEXP in FILES in directory tree rooted at DIR. By default DIR is is the sbt project root.

\(fn REGEXP &optional FILES DIR CONFIRM)" t nil)

(autoload 'sbt-find-usages "sbt-mode-rgrep" "\
Recursively grep for ID in scala files in directory tree rooted at DIR. By default DIR is is the sbt project root.

\(fn ID &optional DIR CONFIRM)" t nil)

(autoload 'sbt-find-definitions "sbt-mode-rgrep" "\
Recursively grep for definition of ID in scala files in the directory tree rooted at the sbt project root.

\(fn ID &optional CONFIRM)" t nil)

;;;***

;;;### (autoloads nil nil ("sbt-mode-buffer.el" "sbt-mode-comint.el"
;;;;;;  "sbt-mode-pkg.el" "sbt-mode-project.el" "sbt-mode-vars.el")
;;;;;;  (22374 36569 46919 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; sbt-mode-autoloads.el ends here
