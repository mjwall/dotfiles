;;; rbenv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "rbenv" "rbenv.el" (21625 59174 0 0))
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

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rbenv-autoloads.el ends here
