;;; ensime-company.el
;;
;;;; License
;;
;;     Copyright (C) 2015 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 'ensime-completion-util)
(require 'ensime-util)
(require 'company)
(require 'yasnippet)
(require 'scala-mode-syntax)
(require 's)
(require 'dash)

(defcustom ensime-company-case-sensitive nil
  "If non-nil, omit completions that don't match the case of prefix."
  :type 'boolean
  :group 'ensime-ui)

(defun ensime--yasnippet-escape (s)
  "Return a new string with special yasnippet chars escaped."
  (s-replace "$" "\\$" s))

(defun ensime--build-yasnippet-for-call
    (param-sections &optional infix pass-function-block)
  "Returns a yasnippet template for a method call, where each argument is a
 tab-stop."
  (let ((tab-stop 0)
	(section-count 0))
     (mapconcat
      (lambda (sect)
	(incf section-count)
	(let* ((params (plist-get sect :params)))

	  (if (and pass-function-block
		   (= section-count (length param-sections)))

	      ;; If requested, expand the last section as an inline block.
	      (let* ((param-type (cadr (car params)))
		     (type-args (plist-get param-type :type-args))
		     (arg-types (-take (- (length type-args) 1) type-args))
		     (result-type (car (last type-args))))
		(if (ensime-type-is-by-name-p param-type) " { $0 }"
		  (concat
		   " { "
		   (let ((param-list
			  (mapconcat
			   (lambda (tpe)
			     (let ((type-name (ensime--yasnippet-escape
					       (ensime-type-name-with-args tpe))))
			       (format "${%s:%s}" (incf tab-stop) type-name)))
			   arg-types ", ")))
		     (if (> (length arg-types) 1)
			 (format "(%s)" param-list) param-list))
		   (let ((result-type-name (ensime--yasnippet-escape
					    (ensime-type-name-with-args result-type))))
		     (format " => ${%s:%s} }$0" (incf tab-stop) result-type-name)))))

	    ;; Otherwise build template for a standard parameter list.
	    (concat (if infix " " "(")
		    (mapconcat
		     (lambda (nm-and-tp)
		       (let ((param-name (ensime--yasnippet-escape (car nm-and-tp)))
			     (type-name (ensime--yasnippet-escape
						   (ensime-type-name-with-args
						    (cadr nm-and-tp)))))
			 (format "${%s:%s: %s}"
				 (incf tab-stop)
				 param-name type-name)))
		     params ", ")
		    (if infix "" ")")))))
      param-sections
      "")
     ))

(defun ensime--company-try-completion ()
  "Attempts a company-mode completion at point. Returns nil if
 completion is not available at point."
  (when company-mode
    (let ((unique-candidate (ensime-unique-candidate-at-point)))
      (cond
       ;; If the identifier is already complete, we must invoke parameter
       ;; expansion manually.
       (unique-candidate
	(ensime--yasnippet-complete-action unique-candidate)
	t)

       ((company-manual-begin)
	(company-complete-common)
	t)

       (t nil)))))

(defun ensime-company-complete-or-indent ()
  "Try to complete, falling back to indentation."
  (interactive)
  (when (or (ensime-at-bol-p)
	    (not (ensime--company-try-completion)))
    (if mark-active
        (indent-region (region-beginning) (region-end))
      (indent-according-to-mode))))

(defun ensime-company-enable ()
  (set (make-local-variable 'company-backends) '(ensime-company))
  (company-mode)
  (yas-minor-mode-on)
  (set (make-local-variable 'company-idle-delay) 0)
  (set (make-local-variable 'company-minimum-prefix-length) 2)
  (if (window-system)
      (local-set-key [tab] 'ensime-company-complete-or-indent)
      (local-set-key (kbd "TAB") 'ensime-company-complete-or-indent)))

(defun ensime--yasnippet-complete-action (&optional candidate-in force-block)
  "If the candidate is a callable symbol, expand a yasnippet template for the
 argument list."
  (let* (;; When called by auto-complete-mode, grab from dynamic environment.
	 (candidate (or candidate-in candidate))
	 (name candidate)
	 (is-callable (get-text-property 0 'is-callable candidate))
	 (to-insert (get-text-property 0 'to-insert candidate))
	 (name-start-point (- (point) (length name)))
	 (is-scala (ensime-scala-file-p buffer-file-name))
	 (call-info
	  (when is-callable (ensime-call-completion-info candidate)))
	 (param-sections
	  (when is-callable
	    (-filter
	     (lambda (sect)
	       (not (plist-get sect :is-implicit)))
	     (ensime-type-param-sections call-info))))
	 (is-operator
	  (and is-callable
	       (= 1 (length param-sections))
	       (= 1 (length (plist-get
			     (car param-sections) :params)))
	       (null (string-match "[A-z]" name))))
	 (is-field-assigner (s-ends-with? "_=" name))
	 (skip-params
	  (and is-scala
	       (or
		;; Scala nullary methods will have 0 param-sections
		(null param-sections)
		;; Also make an exception for zero-arg getters.
		(and
		 (null (plist-get (car param-sections) :params))
		 (s-starts-with-p "get" candidate))))))

    (when is-field-assigner
      (delete-char (- 2))
      (insert " ="))

    ;; If we've completed an operator, get rid of superfluous '.'
    (when is-operator
      (delete-char (- (+ 1 (length name))))
      (insert " ")
      (insert name))

    ;; If an to-insert is available, delete the candidate inserted
    ;; into buffer and replace with to-insert
    (when to-insert
      (delete-char (- (length name)))
      (insert to-insert))

    ;; If we're modifying an existing method identifier, delete what
    ;; was there before and don't mess with the params.
    (-when-let (suffix (ensime-completion-suffix-at-point))
	(delete-char (length suffix))
	(setq skip-params t))

    (when (and is-callable call-info (not skip-params))
      (let* ((maybe-braces (ensime-param-section-accepts-block-p
			    (car (last param-sections))))
	     (pass-function-block
	      (and maybe-braces
		   (eq
		    (or force-block
			(read-char-choice
			 "Choose '{' or '(' " '( ?\{ ?\( ))) ?\{)))
	     (snippet
	      (ensime--build-yasnippet-for-call
	       param-sections
	       (or is-operator is-field-assigner)
	       pass-function-block)))
	(yas-expand-snippet snippet (point) (point))
	))))

(defun ensime-company (command &optional arg &rest _args)
  "Ensime backend for company-mode."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'ensime-company))

    (`prefix (if (ensime-connected-p)
                 (ensime-completion-prefix-at-point)
               nil))

    (`candidates
     ;; Just ignore if there's no connection.
     (when (and (ensime-connected-p) (ensime-analyzer-ready))
       (let ((max-results 1000000)  ;; We want *all* candidates.
	     (case-sense nil))
	 `(:async . (lambda (callback)
		      (ensime-get-completions-async
		       ,max-results ,case-sense callback))))))

    ;; Don't do client-side sorting (preserve server-side rankings).
    (`sorted t)

    ;; We handle dup removal on the server.
    (`duplicates nil)

    ;; We request *all* completions, so it's ok to let company manage caching.
    (`no-cache nil)

    ;; Show an inline signature for callable completions.
    (`annotation
     (concat (if (get-text-property 0 'is-callable arg) "" ": ")
	     (ensime-brief-type-sig (get-text-property 0 'type-sig arg))))

    ;; Expand function formal parameters if we've completed a call.
    (`post-completion (ensime--yasnippet-complete-action arg))

    (`ignore-case t)
    (`require-match `never)
    (`doc-buffer nil) ;; TODO for docs!
    (`meta nil) ;; TODO for short docs!
    (`location nil) ;; TODO Maybe use at some point to link to definitions.
    (_ nil)
    ))

(provide 'ensime-company)

;; Local Variables:
;; End:
