(require 'projectile)
(projectile-global-mode)
;; (require 'project-root)

;; (setq project-roots
;;       `(
;;         ("Clojure project"
;;          :root-contains-files ("project.clj")
;;          ;;:filename-regex ,(regexify-ext-list '(clj jsp css js xml html))
;;          ;;:exclude-paths ("lib" "classes")
;;          )
;;         ("Grails project"
;;          :root-contains-files ("grails-app")
;;          ;;:filename-regex ,(regexify-ext-list '(groovy gsp java html jsp css js xml))
;;          ;;:exclude-paths ("target")
;;          )
;;         (".emacs "
;;          :root-contains-files ("init.el")
;;          ;;:filename-regex ,(regexify-ext-list '(el org README txt))
;;          ;;:exclude-paths ("backups" "autosave-list" "semanticdb")
;;          )
;;         ("buildr project"
;;          :root-contains-files ("buildfile"))
;;         ("ant project"
;;          :root-contains-files ("build.xml"))
;;         ("rails project"
;;          :root-contains-files ("app/controllers" "app/models" "config/application.rb"))
;;         ("sinatra project"
;;          :root-contains-files ("app.rb" "config.ru" "spec"))
;;         ("any git projetct"
;;          :root-contains-files (".git"))
;;         ("Maven project"
;;          :root-contains-files ("pom.xml")
;;          ;;:filename-regex ,(regexify-ext-list '(java html groovy jsp js css xml))
;;          ;;          :on-hit (lambda (p) (message (car p)))
;;          ;;:exclude-paths ("target")
;;          )
;;         )
;;       )


;; (global-set-key (kbd "C-c p f") 'project-root-find-file)
;; (global-set-key (kbd "C-c p g") 'project-root-grep)
;; (global-set-key (kbd "C-c p a") 'project-root-ack)
;; (global-set-key (kbd "C-c p d") 'project-root-goto-root)
;; (global-set-key (kbd "C-c p p") 'project-root-run-default-command)
;; (global-set-key (kbd "C-c p l") 'project-root-browse-seen-projects)
;; (global-set-key [f7] 'my-ido-project-files)

;; ;; extra function to use ido to find project files from http://www.emacswiki.org/emacs/InteractivelyDoThings#toc12
;; (defun my-ido-project-files ()
;;   "Use ido to select a file from the project."
;;   (interactive)
;;   (let (my-project-root project-files tbl)
;;     (unless project-details (project-root-fetch))
;;     (setq my-project-root (cdr project-details))
;;     ;; get project files
;;     (setq project-files
;;           (split-string
;;            (shell-command-to-string
;;             (concat "find "
;;                     my-project-root
;;                     " \\( -name \"*.svn\" -o -name \"*.git\" -o -name \"*.jar\" -o -name \"*.class\" -o -path \"*/WEB-INF\" -o -path \"*/target\" -o -path \"*/vendor/isolate\" \\) -prune -o -type f -print"
;;                     )) "\n"))
;;     ;; populate hash table (display repr => path)
;;     (setq tbl (make-hash-table :test 'equal))
;;     (let (ido-list)
;;       (mapc (lambda (path)
;;               ;; format path for display in ido list
;;               (setq key (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))
;;               ;; strip project root
;;               (setq key (replace-regexp-in-string my-project-root "" key))
;;               ;; remove trailing | or /
;;               (setq key (replace-regexp-in-string "\\(|\\|/\\)$" "" key))
;;               (puthash key path tbl)
;;               (push key ido-list)
;;               )
;;             project-files
;;             )
;;       (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl)))))


(provide 'init-project)
