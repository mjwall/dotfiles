;;; dired-narrow-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "dired-narrow" "dired-narrow.el" (22288 60763
;;;;;;  0 0))
;;; Generated autoloads from dired-narrow.el

(autoload 'dired-narrow-regexp "dired-narrow" "\
Narrow a dired buffer to the files matching a regular expression.

\(fn)" t nil)

(autoload 'dired-narrow "dired-narrow" "\
Narrow a dired buffer to the files matching a string.

If the string contains spaces, then each word is matched against
the file name separately.  To succeed, all of them have to match
but the order does not matter.

For example \"foo bar\" matches filename \"bar-and-foo.el\".

\(fn)" t nil)

(autoload 'dired-narrow-fuzzy "dired-narrow" "\
Narrow a dired buffer to the files matching a fuzzy string.

A fuzzy string is constructed from the filter string by inserting
\".*\" between each letter.  This is then matched as regular
expression against the file name.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dired-narrow-autoloads.el ends here
