;;; ruby-end-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ruby-end" "ruby-end.el" (21767 695 840521
;;;;;;  60000))
;;; Generated autoloads from ruby-end.el

(autoload 'ruby-end-mode "ruby-end" "\
Automatic insertion of end blocks for Ruby.

\(fn &optional ARG)" t nil)

(add-hook 'ruby-mode-hook 'ruby-end-mode)

(add-hook 'enh-ruby-mode-hook 'ruby-end-mode)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ruby-end-autoloads.el ends here
