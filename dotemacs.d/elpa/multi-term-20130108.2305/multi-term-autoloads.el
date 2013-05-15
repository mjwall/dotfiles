;;; multi-term-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (multi-term-dedicated-select multi-term-dedicated-toggle
;;;;;;  multi-term-dedicated-open multi-term-prev multi-term-next
;;;;;;  multi-term) "multi-term" "../../../../../.emacs.d/elpa/multi-term-20130108.2305/multi-term.el"
;;;;;;  "6b403c0b41a3f9cab5c442a0c05c245c")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/multi-term-20130108.2305/multi-term.el

(autoload 'multi-term "multi-term" "\
Create new term buffer.
Will prompt you shell name when you type `C-u' before this command.

\(fn)" t nil)

(autoload 'multi-term-next "multi-term" "\
Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET.

\(fn &optional OFFSET)" t nil)

(autoload 'multi-term-prev "multi-term" "\
Go to the previous term buffer.
If OFFSET is `non-nil', will goto previous term buffer with OFFSET.

\(fn &optional OFFSET)" t nil)

(autoload 'multi-term-dedicated-open "multi-term" "\
Open dedicated `multi-term' window.
Will prompt you shell name when you type `C-u' before this command.

\(fn)" t nil)

(autoload 'multi-term-dedicated-toggle "multi-term" "\
Toggle dedicated `multi-term' window.

\(fn)" t nil)

(autoload 'multi-term-dedicated-select "multi-term" "\
Select the `multi-term' dedicated window.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/multi-term-20130108.2305/multi-term-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/multi-term-20130108.2305/multi-term.el")
;;;;;;  (20882 58143 296956 0))

;;;***

(provide 'multi-term-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; multi-term-autoloads.el ends here
