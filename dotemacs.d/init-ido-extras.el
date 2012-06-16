;; not in elpa, get from http://www0.fh-trier.de/~politza/emacs/ido-hacks.el.gz and put in site-lisp.  New version on git at https://github.com/scottjad/ido-hacks when I move to emacs 24
(require 'ido-hacks)

;;----------------------------------------------------------------------------
;; ido completion in M-x
;;----------------------------------------------------------------------------
(require 'smex)
(smex-initialize)
(global-set-key "\M-x" 'smex)
;; avoid M-x if possible, https://sites.google.com/site/steveyegge2/effective-emacs
;;(global-set-key "\C-x\C-m" 'execute-extended-command)
;;(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

(provide 'init-ido-extras)
