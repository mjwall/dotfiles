;; I hear such good things about magit, but I have invested a lot in learning the git commands
;; Magit is installed, but I am not sure what I'll use it for.

;; since I use git from the terminal in emacs with GIT_EDITOR=emacsclient, I need to close
;; the commit message when I am done.  C-x # is the command, which runs server-edit.  But
;; I do that so often, I want an easier key combo
(global-set-key "\C-c\C-w" 'server-edit)

;; revbufs from http://www.neilvandyke.org/revbufs/revbufs.el
;; most useful when changes branches in a term, emacs will tell you the file has changed
(require 'revbufs)
(global-set-key [f8] 'revbufs)


(provide 'init-git)
