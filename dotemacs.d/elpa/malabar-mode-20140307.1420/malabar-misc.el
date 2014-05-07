;;; malabar-misc.el --- Miscellaneous functions for Malabar
;;
;; Copyright (c) 2009, 2010 Espen Wiborg <espenhw@grumblesmurf.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA.
;;

(require 'cl-lib)
(require 'malabar-util)
(require 'malabar-reflection)
(require 'malabar-import)
(require 'semantic/wisent/javat-wy)

(defun malabar-qualified-class-name-of-buffer (&optional buffer)
  (let ((class (malabar-unqualified-class-name-of-buffer buffer)))
    (malabar-qualify-class-name-in-buffer class buffer)))

(defun malabar-unqualified-class-name-of-buffer (&optional buffer)
  (file-name-sans-extension
   (file-name-nondirectory
    (buffer-file-name (or buffer (current-buffer))))))

(defun malabar-prompt-for-and-qualify-class (prompt &optional class)
  (let* ((class (or class
                    (read-from-minibuffer prompt)))
         (qualified-class (or (malabar-import-find-import class)
                              (malabar-qualify-class-name-in-buffer class)))
         (class-info (malabar-get-class-info qualified-class)))
    (list class qualified-class class-info)))

(defun malabar-goto-start-of-class ()
  (let ((class-tag (malabar-get-class-tag-at-point)))
    (goto-char (semantic-tag-start class-tag))))

(defun malabar-goto-end-of-class ()
  (let ((class-tag (malabar-get-class-tag-at-point)))
    (goto-char (1- (semantic-tag-end class-tag)))))


(defun malabar-first-member-of-class ()
" Returns the tag for the first member of the class or nil if
there are no members.
"
  (car (semantic-tag-type-members
	(car (semantic-brute-find-tag-by-class
	      'type (malabar-semantic-fetch-tags))))))

(defun malabar-goto-tag (tag)
  "Move point to begining of TAG and return the new point.  

When TAG is nil, point remains unchanged and return nil.  "
  (when tag
    (goto-char (semantic-tag-start tag))))

(defun malabar-get-superclass-at-point ()
  (malabar-qualify-class-name-in-buffer (malabar-get-superclass (malabar-get-class-tag-at-point))))

(defun malabar-get-superclass (class-tag)
  (or (car (semantic-tag-type-superclasses class-tag))
       "Object"))
  
(defun malabar-find-method-in-current-class (method-tag)
  (let ((class-tag (malabar-get-class-tag-at-point))
        (method-name (malabar--get-name method-tag))
        (method-argument-types
         (mapcar (lambda (arg)
                   (malabar-qualify-class-name-in-buffer (malabar--get-type arg)))
                 (malabar--get-arguments method-tag))))
    (cl-some (lambda (tag)
            (and (equal method-name
                        (semantic-tag-name tag))
                 (equal method-argument-types 
                        (mapcar (lambda (arg-tag)
                                  (malabar-qualify-class-name-in-buffer
                                   (semantic-tag-type arg-tag)))
                                (semantic-tag-function-arguments tag)))
                 tag))
          (semantic-tag-type-members class-tag))))

(defun malabar-semantic-fetch-tags ()
  (unless (semantic-active-p)  
    (semantic-new-buffer-fcn))
  (let ((tags (semantic-fetch-tags)))
    (mapc (lambda (tag)
            (when (semantic-tag-of-class-p tag 'type)
              (when (equal (semantic-tag-type tag) "interface")
                ;; All interface members are public
                (loop for member in (semantic-tag-type-members tag)
                      do (semantic-tag-put-attribute
                          member :typemodifiers
                          (cl-delete-duplicates (cons "public"
                                                   (semantic-tag-modifiers member))
                                             :test #'equal))))
              (when-let (buffer (semantic-tag-buffer tag))
                (semantic-tag-put-attribute
                 tag :superclasses
                 (mapcar (lambda (c)
                           (malabar-qualify-class-name-in-buffer (malabar--raw-type c)
                                                                 buffer))
                         (semantic-tag-type-superclasses tag))))))
          tags)
    tags))

(defun wisent-malabar-java-setup ()
  ;; HACK: Since we're not loading the old java parser the installer
  ;; function isn't defined; give it a dummy definition
  (load "semantic/wisent/javat-wy.elc") ;; gh-101: no idea why require is not working
  (cl-flet ((wisent-java-wy--install-parser () nil)
         (wisent-java-tags-wy--install-parser () nil)) ;; For Emacs 23.2+
    (wisent-java-default-setup))
  (setq semantic-lex-depth 10)
  (setq semantic-idle-scheduler-idle-time 1)
  (setq semantic-lex-analyzer 'wisent-malabar-java-lexer)
  (wisent-malabar-java-wy--install-parser))

(provide 'malabar-misc)
