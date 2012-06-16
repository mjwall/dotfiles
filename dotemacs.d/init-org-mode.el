;(global-set-key (kbd "C-c s") 'org-store-link)
;(global-set-key (kbd "C-c a") 'org-agenda)
;(global-set-key (kbd "C-c c") 'org-capture)
;(global-set-key (kbd "C-c b") 'org-iswitchb)

;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;(setq org-log-done t)
;(setq org-completion-use-ido t)

;(setq org-todo-keywords
;      '((sequence "TODO" "INPROGRESS" "ONHOLD" "|" "DONE" "CANCELLED")))

;(setq org-directory "~/org")
;(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))

; org mobile
;(setq org-mobile-directory "~/Dropbox/MobileOrg")
;(setq org-mobile-inbox-for-pull "~/org/todo.org")
;(setq org-startup-indented t)
;(add-hook 'after-init-hook 'org-mobile-pull)
;(add-hook 'kill-emacs-hook 'org-mobile-push)

; refile to other files
;(setq org-outline-path-complete-in-steps t)
;(setq org-refile-use-outline-path 'file)
;(setq org-refile-targets '((org-agenda-files . (:level . 2))))

; capture
; (setq org-capture-templates
;      '(("t" "Todo" entry (file "~/org/todo.org" )
;             "* TODO %^{enter todo}\n%?")
;        ("j" "Journal" entry (file+datetree "~/org/journal.org")
;         "* %^{journal title}\n%?\nEntered on %U")))
;(setq org-default-notes-file (concat org-directory "~/org/inbox.org"))

;; installed elpa compliant latest according to
;; http://orgmode.org/worg/org-faq.html#installing-from-elpa-tarball

(provide 'init-org-mode)
