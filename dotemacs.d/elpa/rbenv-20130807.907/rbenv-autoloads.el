;;; rbenv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-rbenv-mode rbenv-use rbenv-use-corresponding
;;;;;;  rbenv-use-global) "rbenv" "rbenv.el" (21016 1467 0 0))
;;; Generated autoloads from rbenv.el

(autoload 'rbenv-use-global "rbenv" "\
activate rbenv global ruby

\(fn)" t nil)

(autoload 'rbenv-use-corresponding "rbenv" "\
search for .ruby-version and activate the corresponding ruby

\(fn)" t nil)

(autoload 'rbenv-use "rbenv" "\
choose what ruby you want to activate

\(fn RUBY-VERSION)" t nil)

(defvar global-rbenv-mode nil "\
Non-nil if Global-Rbenv mode is enabled.
See the command `global-rbenv-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-rbenv-mode'.")

(custom-autoload 'global-rbenv-mode "rbenv" nil)

(autoload 'global-rbenv-mode "rbenv" "\
use rbenv to configure the ruby version used by your Emacs.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("rbenv-pkg.el") (21016 1467 746961 0))

;;;***

(provide 'rbenv-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rbenv-autoloads.el ends here
