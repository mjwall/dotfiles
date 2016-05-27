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
                   :fuzzy-match t)))
)

(provide 'ensime-helm)

;; Local Variables:
;; End:
