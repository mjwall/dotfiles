;;; ensime-model.el --- Data-structure accessors

(eval-when-compile
  (require 'cl)
  (require 'ensime-macros))

(require 's)

(defun ensime-search-sym-name (sym)
  (plist-get sym :name))

(defun ensime-search-sym-local-name (sym)
  (plist-get sym :local-name))

(defun ensime-search-sym-pos (sym)
  (plist-get sym :pos))

(defun ensime-search-sym-owner-name (sym)
  (plist-get sym :owner-name))

(defun ensime-search-sym-decl-as (sym)
  (plist-get sym :decl-as))

(defun ensime-symbol-name (sym)
  (plist-get sym :name))

(defun ensime-symbol-decl-pos (sym)
  (plist-get sym :decl-pos))

(defun ensime-symbol-type (sym)
  (plist-get sym :type))

(defun ensime-symbol-is-callable (sym)
  (plist-get sym :is-callable))

(defun ensime-package-name (info)
  (plist-get info :name))

(defun ensime-package-full-name (info)
  (plist-get info :full-name))

(defun ensime-package-members (info)
  (plist-get info :members))

(defun ensime-package-p (info)
  (equal 'package (plist-get info :info-type)))

(defun ensime-type-inspection-p (info)
  (equal 'typeInspect (plist-get info :info-type)))

(defun ensime-type-name (type)
  ;; legacy method
  (replace-regexp-in-string "\\[.*" ""
                            (plist-get type :name)))

(defun ensime-type-full-name (type)
  ;; legacy method
  (replace-regexp-in-string "\\[.*" ""
                            (plist-get type :full-name)))

(defun ensime-type-is-object-p (type)
  (equal (plist-get type :decl-as) 'object))

(defun ensime-type-full-name-with-args (type)
  (plist-get type :full-name))

(defun ensime-type-param-sections (type)
  (plist-get type :param-sections))

(defun ensime-type-name-with-args (type)
  (plist-get type :name))

(defun ensime-type-is-function-p (type)
  (string-match "^scala.Function[0-9]*" (plist-get type :full-name)))

(defun ensime-type-is-by-name-p (type)
  (string-match "^scala.<byname>" (plist-get type :full-name)))

(defun ensime-parse-type-info-from-scala-name (scala-name)
  "Returns a type-info structure parsed from a `scala-name', e.g.
 scala.foo.Foo[scala.Option[Int]]"
  ;; this shouldn't be needed, and there is no way to guarantee that
  ;; it is well-formed. we should be asking the server
  (let ((s scala-name) (i 0))
    (let ((ast (ensime--parse-type-from-scala-name)))
      (ensime--build-type-info ast))))

(defun ensime--build-type-info (ast)
  (let* ((type-args (mapcar 'ensime--build-type-info (nth 2 ast)))
         (path (nth 0 ast))
         (short-type-params (ensime--build-type-parameters type-args nil))
         (full-type-params (ensime--build-type-parameters type-args t))
         (local-base (nth 1 ast))
         (local-name (concat local-base short-type-params))
         (full-base (if (s-blank? path)
                        local-base
                      (concat path "." local-base)))
         (full-name (concat full-base full-type-params)))

    `(:name ,local-name :full-name ,full-name :type-args ,type-args)))

(defun ensime--build-type-parameters (params full)
  "String representing a list of type-info `PARAMS'.
`FULL' non-nil gets the full name."
  (when params
    (let* ((sym (if full :full-name :name))
           (names (mapcar (lambda (e) (plist-get e sym)) params)))
      (s-with names
        (s-join ", ")
        (s-prepend "[")
        (s-append "]")))))

(defconst ensime--ident-re "\\(?:<.+?>\\|[^][\\., ]+\\)")
(defconst ensime--scala-name-re (concat "\\(\\(?:" ensime--ident-re "\\.\\)" "*" "\\)" "\\(" ensime--ident-re "\\)"))
(defconst ensime--type-args-re "\\[\\(.+?\\)\\]$")

(defun ensime--parse-type-from-scala-name ()
  "Parse the bound variable `S' into a list format."
  ;;
  ;; HERE BE DRAGONS
  ;;
  ;; Anytime we want a data structure like this we should talk to the server.
  ;;
  (dolist (sub '(("^[ ]*=> \\(.*\\)" . "scala.<byname>[\\1]")
                 ("() => \\(.*\\)" . "scala.Function0[\\1]")
                 ("(\\([^,]*\\)) => \\(.*\\)" . "scala.Function1[\\1, \\2]")
                 ("(\\([^,]*\\), \\([^,]*\\)) => \\(.*\\)" . "scala.Function2[\\1, \\2, \\3]")
                 ("(\\([^,]*\\), \\([^,]*\\), \\([^,]*\\)) => \\(.*\\)" . "scala.Function3[\\1, \\2, \\3, \\4]")
                 ("(\\([^,]*\\))" . "scala.Tuple1[\\1]")
                 ("(\\([^,]*\\), \\([^,]*\\))" . "scala.Tuple2[\\1, \\2]")
                 ("(\\([^,]*\\), \\([^,]*\\), \\([^,]*\\))" . "scala.Tuple3[\\1, \\2, \\3]")))
    (setq s (replace-regexp-in-string (car sub) (cdr sub) s)))
  (when (and (< i (length s)) (ensime--match-re ensime--scala-name-re))
    (let* ((path (match-string 1 s))
           (local-base (match-string 2 s))
           (args (when (ensime--parse-one ?\[)
                   (let ((args (ensime--parse-delimited
                                'ensime--parse-type-from-scala-name
                                ensime--parser-commas-ws)))
                     (when args
                       (ensime--parse-one ?\])
                       args)))))
      (list (s-chop-suffix "." path) local-base args))))

(defun ensime--parse-one (char)
  (when (and (< i (length s)) (eq (aref s i) char))
    (incf i)
    t))

(defun ensime--parse-one-or-more (chars)
  (let ((j i))
    (while (and (< j (length s)) (memq (aref s j) chars)) (incf j))
    (when (> j i) (setq i j) t)))

(defun ensime--match-re (re)
  (let ((index (string-match re s i)))
    (when (eq index i)
      (setq i (match-end 0)) t)))

(defvar ensime--parser-commas-ws '(lambda () (ensime--parse-one-or-more (list ?, ? ))))

(defun ensime--parse-delimited (fn delim-fn)
  (let ((parsed nil)
	(result (funcall fn)))
    (while result
      (setq parsed (cons result parsed))
      (setq result (when (funcall delim-fn) (funcall fn))))
    (reverse parsed)))

(defun ensime-declared-as (obj)
  (plist-get obj :decl-as))

(defun ensime-declared-as-str (obj)
  (case (plist-get obj :decl-as)
    (method "method")
    (trait "trait")
    (interface "interface")
    (class "class")
    (object "object")
    (otherwise "type")
    ))

(defun ensime-type-is-arrow-p (type)
  (plist-get type :arrow-type))

(defun ensime-type-param-types (type)
  "Return types of params in first section."
  (let ((section (car (plist-get type :param-sections))))
    (mapcar
     (lambda (p)
       (cadr p))
     (plist-get section :params)
     )))

(defun ensime-param-section-accepts-block-p (section)
  "Returns t if the section has a single functional parameter."
  (let* ((params (plist-get section :params))
	 (arg-type (cadr (car params))))
    (and (= 1 (length params))
	 (or (ensime-type-is-function-p arg-type)
	     (ensime-type-is-by-name-p arg-type)))))

(defun ensime-type-result-type (type)
  (plist-get type :result-type))

(defun ensime-type-type-args (type)
  (plist-get type :type-args))

(defun ensime-member-name (member)
  (plist-get member :name))

(defun ensime-member-type (member)
  (plist-get member :type))

(defun ensime-member-signature (member)
  (plist-get member :signature-string))

(defun ensime-member-pos (member)
  (plist-get member :pos))

(defun ensime-pos-file (pos)
  (plist-get pos :file))

(defun ensime-pos-archive (pos)
  (plist-get pos :archive))

(defun ensime-pos-effective-file (pos)
  (if (plist-get pos :archive)
      (concat
       (ensime-source-jars-dir)
       (file-name-as-directory (file-name-nondirectory (plist-get pos :archive)))
       (plist-get pos :file))
    (plist-get pos :file)))

(defun ensime-pos-offset (pos)
  (plist-get pos :offset))

(defun ensime-pos-line (pos)
  (plist-get pos :line))

(defun ensime-pos-available-p (pos)
  (or (ensime-pos-valid-local-p pos)
      (eq pos t)))

(defun ensime-pos-valid-local-p (pos)
  (and (stringp (ensime-pos-file pos))
       (or (file-exists-p (ensime-pos-file pos))
           (and (stringp (ensime-pos-archive pos))
                (file-exists-p (ensime-pos-archive pos))))
       (or (integerp (ensime-pos-line pos))
           (integerp (ensime-pos-offset pos)))))

(defun ensime-note-file (note)
  (plist-get note :file))

(defun ensime-note-beg (note)
  (plist-get note :beg))

(defun ensime-note-end (note)
  (plist-get note :end))

(defun ensime-note-line (note)
  (plist-get note :line))

(defun ensime-note-message (note)
  (plist-get note :msg))

;; FIXME: why is this function needed, doesn't the caller have everything?
(defun ensime-brief-type-sig (completion-type-sig)
  "Return a formatted string representing the given method signature."
  ;;(ensime-brief-type-sig '(((("foo" "Person"))) "Dude"))
  ;;(ensime-brief-type-sig '(((("bar" "scala.collection.Person[X[Y]]"))) "Dude[Y]"))
  (let* ((sections (car completion-type-sig))
	 (return-type
      ;; FIXME: do we really need to parse this client side?
      ;;        why not just return the cadr of the input?
      (ensime-parse-type-info-from-scala-name
		       (cadr completion-type-sig)))
	 (return-name (ensime-type-name return-type)))
    (if sections
	(format "%s: %s"
		(mapconcat
		 (lambda (section)
		   (format "(%s)"
			   (mapconcat
			    (lambda (param-pair)
			      (let* ((name (car param-pair))
                         (scala-name (cadr param-pair))
                         ;; FIXME do we really need to parse this client side?
                         (tpe (ensime-parse-type-info-from-scala-name scala-name)))
                    (format "%s: %s" name (ensime-type-name tpe))))
			    section ", ")))
		 sections "=>")
		return-name)
      return-name)))


(provide 'ensime-model)

;; Local Variables:
;; End:
