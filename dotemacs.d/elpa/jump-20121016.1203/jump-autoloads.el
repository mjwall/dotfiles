;;; jump-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (defjump) "jump" "../../../../../.emacs.d/elpa/jump-20121016.1203/jump.el"
;;;;;;  "0a20014e68a143d6fd509e6d0952fde1")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/jump-20121016.1203/jump.el

(autoload 'defjump "jump" "\
Define NAME as a function with behavior determined by SPECS.
SPECS should be a list of cons cells of the form

   (jump-from-spec . jump-to-spec)

NAME will then try subsequent jump-from-specs until one succeeds,
at which point any resulting match information, along with the
related jump-to-spec will be used to jump to the intended buffer.
See `jump-to' and `jump-from' for information on spec
construction.

ROOT should specify the root of the project in which all jumps
take place, it can be either a string directory path, or a
function returning

Optional argument DOC specifies the documentation of the
resulting function.

Optional argument MAKE can be used to specify that missing files
should be created.  If MAKE is a function then it will be called
with the file path as it's only argument.  After possibly calling
MAKE `find-file' will be used to open the path.

Optional argument METHOD-COMMAND overrides the function used to
find the current method which defaults to `which-function'.

\(fn NAME SPECS ROOT &optional DOC MAKE METHOD-COMMAND)" nil t)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/jump-20121016.1203/jump-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/jump-20121016.1203/jump.el")
;;;;;;  (20882 58147 552642 0))

;;;***

(provide 'jump-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jump-autoloads.el ends here
