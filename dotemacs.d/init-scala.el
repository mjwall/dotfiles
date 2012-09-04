(require 'scala-mode-auto) ;; from scala 2.9.1, missing from later versions
(require 'ensime) ;; ensime_2.9.2-0.9.7 from https://github.com/aemoncannon/ensime/downloads

(add-to-list 'auto-mode-alist '(".sbt" . scala-mode))

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'run-coding-hook)


(provide 'init-scala)
