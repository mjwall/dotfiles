(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(require 'rainbow-delimiters)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; Slime
;;(require 'slime)
;;(require 'slime-autoloads)
;; (setq slime-lisp-implementations
;;        '((clisp ("clisp") :coding-system utf-8-unix)
;;          (clojure ,(swank-clojure-cmd) :init swank-clojure-init)
;;          ;; (scheme ("scheme") :coding-system utf-8-unix)
;;          ))

;; (setf slime-default-lisp 'clisp)
;; ;;(setf slime-default-lisp 'clojure)

;;(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;;(slime-setup '(slime-repl))
;;(setq slime-net-coding-system 'utf-8-unix)

;; add paredit to slime
;;(defun slimeify ()
  ;;(paredit-mode 1)
  ;;(define-key slime-repl-mode-map ;; stop slime from grabbing del
  ;;(read-kbd-macro paredit-backward-delete-key)
  ;;nil))

;;(add-hook 'slime-repl-mode-hook 'slimeify)

;; Clojure mode
(require 'clojure-mode)
;;(require 'clojure-test-mode) ;; has slime built in?
(add-hook 'clojure-mode-hook 'run-coding-hook)
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (rainbow-delimiters-mode +1)))

;; Elisp
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode +1)))
(add-hook 'emacs-lisp-mode-hook 'run-coding-hook)
;;(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode) ;;this is cool, but it seems to really slow down editing

;; MIT scheme
;; from site-lisp/slime/contrib/swank-mit-scheme.scm

;; (setq slime-lisp-implementations
;;       '((mit-scheme ("mit-scheme") :init mit-scheme-init)))

;; (setq slime-find-buffer-package-function 'find-mit-scheme-package)

;; (defun mit-scheme-init (file encoding)
;;   (format "%S\n\n"
;;           `(begin
;;             (load-option 'format)
;;             (load-option 'sos)
;;             (eval
;;              '(construct-normal-package-from-description
;;                (make-package-description '(swank) '(())
;;                                          (vector) (vector) (vector) false))
;;              (->environment '(package)))
;;             (load ,(expand-file-name
;;                     "contrib/swank-mit-scheme.scm" ; <-- insert your path
;;                     slime-path)
;;                   (->environment '(swank)))
;;             (eval '(start-swank ,file) (->environment '(swank))))))

;; (defun mit-scheme ()
;;   (interactive)
;;   (slime 'mit-scheme))

;; (defun find-mit-scheme-package ()
;;   (save-excursion
;;     (let ((case-fold-search t))
;;       (and (re-search-backward "^[;]+ package: \\((.+)\\).*$" nil t)
;;            (match-string-no-properties 1)))))

;; (add-hook 'scheme-mode-hook 'run-coding-hook)
;; (add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'scheme-mode-hook (lambda () (rainbow-delimiters-mode +1)))
;;(add-hook 'scheme-mode-hook 'setup-mit-scheme)



(provide 'init-lisps)
