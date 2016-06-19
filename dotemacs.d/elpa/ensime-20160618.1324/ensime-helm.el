;;; ensime-helm.el -- ensime helm -*- lexical-binding: t -*-

;; Copyright (C) 2016 ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

;;; Commentary:
;;
;;; Code:


(require 'helm)

(defun ensime-helm-select-entry (entries name)
  "Select one entry using helm"
  (let ((name-alist (mapcar* 'cons entries entries)))
  (helm :sources (helm-build-sync-source name
                   :candidates name-alist
                   :fuzzy-match t))))

(defun ensime-helm--format-search-elemen (elem)
  "Formats the search element"
  (let ((pos (ensime-search-sym-pos elem)))
    (let* ((file-name (ensime-pos-file pos))
           (maybe-line (ensime-pos-line pos)))
      (let ((line (if maybe-line (number-to-string maybe-line) "?")))
      (concat
       (ensime-search-sym-name elem)
       "\n"
       (propertize (concat file-name ":" line) 'face 'font-lock-comment-face)
       )))))

(defun ensime-helm-public-symbol-search ()
  "searches for symbols with the given query"
  (if (>= (length helm-pattern) ensime-search-min-length)
      (mapcar (lambda (elem) (cons (ensime-helm--format-search-elemen elem) elem)) (ensime-rpc-public-symbol-search (split-string helm-pattern " ") ensime-search-max-results))
    '(("query to short" . "moep"))))

(defun ensime-helm-search ()
  "searches ensime with helm"
  (interactive)
  (helm :sources (helm-build-sync-source "ensime-search"
                  :candidates 'ensime-helm-public-symbol-search
                  :volatile t
                  :requires-pattern ensime-search-min-length
                  :multiline t
                  :action '(("open" . ensime-search-jump-to-item)
                            ("print info" . (lambda (candidate) (message "selected: %s" candidate))))
                  )
        :buffer "*ensime-helm-search*"))

(provide 'ensime-helm)

;; Local Variables:
;; End:
