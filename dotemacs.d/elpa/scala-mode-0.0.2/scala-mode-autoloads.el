;;; scala-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (scala-mode) "scala-mode" "scala-mode.el" (20281
;;;;;;  6473))
;;; Generated autoloads from scala-mode.el

(autoload 'scala-mode "scala-mode" "\
Major mode for editing Scala code.
When started, run `scala-mode-hook'.
\\{scala-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "scala-mode-auto" "scala-mode-auto.el" (20281
;;;;;;  6473))
;;; Generated autoloads from scala-mode-auto.el

(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

;;;***

;;;### (autoloads (scala-quit-interpreter scala-load-file scala-eval-buffer
;;;;;;  scala-eval-definition scala-eval-region scala-switch-to-interpreter
;;;;;;  scala-run-scala scala-interpreter-running-p-1) "scala-mode-inf"
;;;;;;  "scala-mode-inf.el" (20281 6473))
;;; Generated autoloads from scala-mode-inf.el

(autoload 'scala-interpreter-running-p-1 "scala-mode-inf" "\
Not documented

\(fn)" nil nil)

(autoload 'scala-run-scala "scala-mode-inf" "\
Run a Scala interpreter in an Emacs buffer

\(fn CMD-LINE)" t nil)

(autoload 'scala-switch-to-interpreter "scala-mode-inf" "\
Switch to buffer containing the interpreter

\(fn)" t nil)

(autoload 'scala-eval-region "scala-mode-inf" "\
Send current region to Scala interpreter.

\(fn START END)" t nil)

(autoload 'scala-eval-definition "scala-mode-inf" "\
Send the current 'definition' to the Scala interpreter.
This function's idea of a definition is the block of text ending
in the current line (or the first non-empty line going
backwards), and begins in the first line that is not empty and
does not start with whitespace or '{'.

For example:

println( \"aja\")
println( \"hola\" )

if the cursor is somewhere in the second print statement, the
interpreter should output 'hola'.

In the following case, if the cursor is in the second line, then
the complete function definition will be send to the interpreter:

def foo =
  1 + 2

\(fn)" t nil)

(autoload 'scala-eval-buffer "scala-mode-inf" "\
Send whole buffer to Scala interpreter.

\(fn)" t nil)

(autoload 'scala-load-file "scala-mode-inf" "\
Load a file in the Scala interpreter.

\(fn FILE-NAME)" t nil)

(autoload 'scala-quit-interpreter "scala-mode-inf" "\
Quit Scala interpreter.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("scala-mode-constants.el" "scala-mode-feature-electric.el"
;;;;;;  "scala-mode-feature-speedbar.el" "scala-mode-feature-tags.el"
;;;;;;  "scala-mode-feature.el" "scala-mode-fontlock.el" "scala-mode-indent.el"
;;;;;;  "scala-mode-lib.el" "scala-mode-navigation.el" "scala-mode-pkg.el"
;;;;;;  "scala-mode-ui.el" "scala-mode-variables.el") (20281 6473
;;;;;;  331372))

;;;***

(provide 'scala-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; scala-mode-autoloads.el ends here
