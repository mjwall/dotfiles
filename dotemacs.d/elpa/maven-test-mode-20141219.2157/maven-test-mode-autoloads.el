;;; maven-test-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "maven-test-mode" "maven-test-mode.el" (22287
;;;;;;  55838 0 0))
;;; Generated autoloads from maven-test-mode.el

(defvar maven-test-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-c , a") 'maven-test-all) (define-key map (kbd "C-c , v") 'maven-test-file) (define-key map (kbd "C-c , s") 'maven-test-method) (define-key map (kbd "C-c , i") 'maven-test-install) (define-key map (kbd "C-c , C") 'maven-test-clean-test-all) (define-key map (kbd "C-c , r") 'recompile) (define-key map (kbd "C-c , t") 'maven-test-toggle-between-test-and-class) (define-key map (kbd "C-c , y") 'maven-test-toggle-between-test-and-class-other-window) map))

(autoload 'maven-test-mode "maven-test-mode" "\
This minor mode provides utilities to run maven test tasks

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; maven-test-mode-autoloads.el ends here
