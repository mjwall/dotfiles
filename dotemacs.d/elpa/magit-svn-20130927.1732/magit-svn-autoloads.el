;;; magit-svn-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (turn-on-magit-svn magit-svn-mode magit-svn-fetch-externals)
;;;;;;  "magit-svn" "magit-svn.el" (21067 4659 0 0))
;;; Generated autoloads from magit-svn.el
 (autoload 'magit-svn-find-rev "magit")
 (autoload 'magit-svn-create-branch "magit")
 (autoload 'magit-svn-create-tag "magit")
 (autoload 'magit-svn-rebase "magit")
 (autoload 'magit-svn-dcommit "magit")
 (autoload 'magit-svn-remote-update "magit")

(autoload 'magit-svn-fetch-externals "magit-svn" "\
Loops through all external repos found by `magit-svn-external-directories'
   and runs git svn fetch, and git svn rebase on each of them.

\(fn)" t nil)

(autoload 'magit-svn-mode "magit-svn" "\
SVN support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-svn "magit-svn" "\
Unconditionally turn on `magit-svn-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("magit-svn-pkg.el") (21067 4659 175658
;;;;;;  0))

;;;***

(provide 'magit-svn-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-svn-autoloads.el ends here
