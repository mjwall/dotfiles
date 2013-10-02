;;; anzu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-anzu-mode anzu-mode) "anzu" "anzu.el" (21068
;;;;;;  6908 0 0))
;;; Generated autoloads from anzu.el

(autoload 'anzu-mode "anzu" "\
minor-mode which display search information in mode-line.

\(fn &optional ARG)" t nil)

(defvar global-anzu-mode nil "\
Non-nil if Global-Anzu mode is enabled.
See the command `global-anzu-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-anzu-mode'.")

(custom-autoload 'global-anzu-mode "anzu" nil)

(autoload 'global-anzu-mode "anzu" "\
Toggle Anzu mode in all buffers.
With prefix ARG, enable Global-Anzu mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Anzu mode is enabled in all buffers where
`(lambda nil (unless (minibufferp) (anzu-mode t)))' would do it.
See `anzu-mode' for more information on Anzu mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("anzu-pkg.el") (21068 6908 361099 0))

;;;***

(provide 'anzu-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; anzu-autoloads.el ends here
