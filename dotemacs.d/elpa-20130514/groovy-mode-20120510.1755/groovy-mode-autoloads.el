;;; groovy-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (groovy-mode groovy-mode-hook) "groovy-mode" "groovy-mode.el"
;;;;;;  (20731 25945 0 0))
;;; Generated autoloads from groovy-mode.el

(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))

(defvar groovy-mode-hook nil "\
*Hook called by `groovy-mode'.")

(custom-autoload 'groovy-mode-hook "groovy-mode" t)

(autoload 'groovy-mode "groovy-mode" "\
Major mode for editing Groovy code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `groovy-mode-hook'.

Key bindings:
\\{groovy-mode-map}

\(fn)" t nil)

(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;;***

;;;### (autoloads nil nil ("groovy-mode-pkg.el") (20731 25945 728516
;;;;;;  0))

;;;***

(provide 'groovy-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; groovy-mode-autoloads.el ends here
