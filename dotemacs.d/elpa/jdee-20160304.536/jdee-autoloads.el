;;; jdee-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "beanshell" "../../../../../../.emacs.d/elpa/jdee-20160304.536/beanshell.el"
;;;;;;  "f7af2c600e2352bfc5e1725807856e9b")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/beanshell.el

(autoload 'bsh-script-help "beanshell" "\
Display BeanShell User's Guide.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee.el"
;;;;;;  "905e21250f5a1a6e11f3f2074efb3727")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee.el

(defconst jdee-version "2.4.2" "\
JDEE version number.")

(autoload 'jdee-version "jdee" "\
Get the version of JDEE.

\(fn)" t nil)

(autoload 'jdee-set-global-classpath "jdee" "\
Set the value of `jdee-global-classpath'.
It specifies the -classpath argument for the Java compiler and
interpreter.

\(fn CLASSPATH)" t nil)

(autoload 'jdee-build "jdee" "\
Rebuild the entire project.
This command invokes the function defined by `jdee-build-function'.

\(fn)" t nil)

(autoload 'jdee-mode "jdee" "\
Major mode for developing Java applications and applets.
\\{jdee-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.java\\'" . jdee-mode))

(autoload 'jdee-show-help "jdee" "\
Displays the JDEE User's Guide in a browser.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee-ant" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-ant.el"
;;;;;;  "4326f56bbd5fb9853bbac5cafc4efa01")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-ant.el

(autoload 'jdee-ant-build "jdee-ant" "\
Build the current project using Ant.  If interactive, we try to prompt the
  user for certain variables..

\(fn BUILDFILE TARGET &optional INTERACTIVE-ARGS)" t nil)

(autoload 'jdee-ant-projecthelp "jdee-ant" "\
Display Ant project help for the current project.
This will execute the Ant program with the `-projecthelp' switch to output
available targets with their descriptions for the current buildfile. This
function uses the same rules as `jdee-ant-build' for finding the buildfile.

\(fn BUILDFILE)" t nil)

(autoload 'jdee-ant-show-options "jdee-ant" "\
Show the JDE Ant Options panel.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee-bookmark" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-bookmark.el"
;;;;;;  "41d4a0eaad83e7d148cadd9a4ae35a1a")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-bookmark.el

(autoload 'jdee-bookmark-visit "jdee-bookmark" "\
Visit a class by bookmark name.

\(fn KEY)" t nil)

(autoload 'jdee-bookmark-add "jdee-bookmark" "\
Add the current visited class as a bookmark.

\(fn KEY &optional FQ-CLASS)" t nil)

(autoload 'jdee-bookmark-list "jdee-bookmark" "\
List bookmarks.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee-bsh" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-bsh.el"
;;;;;;  "d96ebb2bbaa86e5dd702a5b99e51dda0")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-bsh.el

(autoload 'jdee-jeval "jdee-bsh" "\
Uses the JDEE's instance of the BeanShell
Java interpreter to evaluate the Java expression EXPR.  If the
BeanShell is not running, the JDEE starts an instance. This function
returns any text output by the Java interpreter's standard out or
standard error pipes.  If EVAL-RETURN is non-nil, this function
returns the result of evaluating the Java output as a Lisp
expression.

NO-PRINT-P, if non-nil, don't wrap JAVA-STATEMENT with a `print'
command yeilding the output.  This is going to need to be true
for most things since unless `show()' was invoked and output
prints out, Emacs has nothing to evaluate or report.

\(fn JAVA-STATEMENT &optional EVAL-RETURN NO-PRINT-P)" t nil)

(autoload 'jdee-bsh-run "jdee-bsh" "\
*Starts the JDEE version of the BeanShell.

\(fn)" t nil)

(autoload 'jdee-bsh-quote-expr "jdee-bsh" "\
Add necessary syntax for a beanshell string (parameter) in the current
buffer.  This is useful for quoting a whole buffer or strings with newlines,
etc.  This gives the outside double quotes as well.

NO-PARAM-P if non-nil, don't split string (surround double quotes) using string
concatentation.  The only way to get quotes in strings is to split them up and
concatenate the quote (') char using the plus (+) operator.  This doesn't do
this syntax change.

NO-QUOTE-WRAP-P, if non-nil, don't add double quotes around the whole statement.

\(fn &optional START END NO-PARAM-P NO-QUOTE-WRAP-P)" t nil)

;;;***

;;;### (autoloads nil "jdee-bug" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-bug.el"
;;;;;;  "c78e331b24606ea0e110c33ba859eb5d")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-bug.el

(autoload 'jdee-bug-minor-mode "jdee-bug" "\
Toggle Jdee-Bug minor mode on or off.
With a prefix argument ARG, enable Jdee-Bug minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{jdee-bug-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'jdee-bug-debug-app "jdee-bug" "\
Runs the debugger on the application in the current source buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee-checkstyle" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-checkstyle.el"
;;;;;;  "50b1ec4817ba70c28533bb78fdfcacb3")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-checkstyle.el

(autoload 'jdee-checkstyle-customize "jdee-checkstyle" "\
Set Java style checking options.

\(fn)" t nil)

(autoload 'jdee-checkstyle "jdee-checkstyle" "\
Checks the Java program in the current buffer.
This command invokes the style checker specified by `jdee-checkstyle-class'
with the options specif2ied by the JDEE customization variables
that begin with `jdee-checkstyle'. If the variable
`jdee-checkstyle-read-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee-compile" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-compile.el"
;;;;;;  "3c3b6aba9cfb439724c92e5d4eb54bd5")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-compile.el

(autoload 'jdee-set-compile-options "jdee-compile" "\
Sets the compile options.
Enter the options as you would on the command line, e.g.,
-depend -verbose.

\(fn OPTIONS)" t nil)

(autoload 'jdee-compile "jdee-compile" "\
Compile the Java program in the current buffer.
This command invokes the compiler specified by `jdee-compiler'
with the options specified by the JDE customization variables
that begin with `jdee-compile'. If the variable
`jdee-read-compile-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled. If `jdee-compiler' specifies the JDE compile
server, this command uses the compile server. Otherwise, it
uses the compiler executable specified by
`jdee-compiler' to compile.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee-custom" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-custom.el"
;;;;;;  "2ed48f0dcb9ff6708e08545373ef4957")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-custom.el

(defalias 'jdee-customize-variable 'jdee-customize-option)

(autoload 'jdee-customize-option "jdee-custom" "\
Customize SYMBOL, which must be a JDEE option variable.

\(fn SYMBOL)" t nil)

;;;***

;;;### (autoloads nil "jdee-db" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-db.el"
;;;;;;  "956551963a8bd26b7c0433a261482913")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-db.el

(autoload 'jdee-db-set-args "jdee-db" "\
Specify the arguments (except -classpath) to be passed to the debugger.

\(fn ARGS)" t nil)

(autoload 'jdee-db-set-app-args "jdee-db" "\
Specify the arguments to be passed to the Java application class.

\(fn ARGS)" t nil)

(autoload 'jdee-debug "jdee-db" "\
Run the debugger specified by `jdee-debugger' on the Java application
whose source resides in the current buffer. This command determines
the main class of the application either from the variable
`jdee-run-application-class' or from the source in the current
buffer. If `jdee-run-application-class' does not specify a class, the
main class is assumed to be the class defined by the current source
buffer. This command creates a command buffer for the debug session.

\(fn)" t nil)

(autoload 'jdee-debug-applet "jdee-db" "\
Runs an applet in the jdb debugger. This function prompts you to enter
the path of an html document that displays the applet. If you
do not enter a path, this function next checks
whether `jdee-run-applet-doc' specifies a document. If so, it displays
that specified document. Next, it checks whether the current directory
contains any html files. If so, it displays the first html file that
it finds. If if cannot find an html file, it signals an error.  This
function runs appletviewer in jdb to permit debugging. On startup, it
sets a breakpoint in the init method of the class specified by
`jdee-run-application-class' or in the class corresponding to the Java
file in the current buffer.

\(fn &optional DOC)" t nil)

;;;***

;;;### (autoloads nil "jdee-ejb" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-ejb.el"
;;;;;;  "2531b0c22a4d8525d77b65d4e04ec3c2")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-ejb.el

(autoload 'jdee-ejb-session-bean-buffer "jdee-ejb" "\
Create a new Java buffer containing an EJB session bean class of the same name.
This command also creates buffers with the EJB Home and EJB Remote interfaces
and the XML Deployment descriptor defined
by the jdee-ejb templates.  This includes naming the files according
to the EJB naming convention.

\(fn EJB-NAME)" t nil)

(autoload 'jdee-ejb-entity-bean-buffer "jdee-ejb" "\
Create a new Java buffer containing an EJB entity bean class of the same name.
This command also creates buffers with the EJB Home and EJB Remote interfaces
and the XML Deployment descriptor defined
by the jdee-ejb templates.  This includes naming the files according
to the EJB naming convention.

\(fn EJB-NAME)" t nil)

;;;***

;;;### (autoloads nil "jdee-font-lock" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-font-lock.el"
;;;;;;  "84b7a3382553298617eb78bbcb75a948")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-font-lock.el

(autoload 'jdee-font-lock-setup-keywords "jdee-font-lock" "\
Setup font lock keywords in `java-font-lock-keywords-4'.
If optional REBUILD flag is non-nil create a new cache of regular
expressions.

\(fn &optional REBUILD)" t nil)

;;;***

;;;### (autoloads nil "jdee-gen" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-gen.el"
;;;;;;  "57fb1608681c078c6e9057206274986a")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-gen.el

(autoload 'jdee-gen-class-buffer "jdee-gen" "\
Create a new Java buffer containing a class of the same name.
This command inserts the class template generated by `jdee-gen-class'.

\(fn FILE)" t nil)

(autoload 'jdee-gen-interface-buffer "jdee-gen" "\
Create a new Java buffer containing an interface of the same name.
This command inserts the interface template generated by `jdee-gen-interface'.
It then moves the point to the location of the first method.

\(fn FILE)" t nil)

(autoload 'jdee-gen-console-buffer "jdee-gen" "\
Create a new Java buffer containing a console class of the same name.
This command inserts the class template generated by `jdee-gen-class'.
It then moves the point to the location to the constructor.

\(fn FILE)" t nil)

(autoload 'jdee-gen-bean-buffer "jdee-gen" "\
Create a new Java buffer containing a Java bean of the same name.
This command inserts the class template generated by `jdee-gen-bean'.
It then moves the point to the location of the constructor.

\(fn FILE)" t nil)

(autoload 'jdee-gen-jfc-app-buffer "jdee-gen" "\
Create a new Java buffer containing a JFC application class.
This command inserts the class template generated by `jdee-gen-jfc-app'.
It then moves the point to the location to the constructor.

\(fn FILE)" t nil)

(autoload 'jdee-gen-buffer "jdee-gen" "\
Create a new Java buffer containing a code template.
This command inserts the specified template at the beginning
of the buffer.

\(fn TEMPLATE FILE)" t nil)

(defvar jdee-gen-equals-trailing-and-operators nil "\
Specifies whether the '&&' operators in a generated equals
method are added at the end of the line or at the beginning.  If
this variable is t, the operator will be added at the end of the
line, else on the next line before the comparison.  With
`jdee-gen-equals-trailing-and-operators' set to nil:

    return (a == o.a)
	&& (b == o.b)
	&& (s == null ? o.s == null : s.equals(o.s));

Or, with `jdee-gen-equals-trailing-and-operators' set to t:

    return (a == o.a) &&
	(b == o.b) &&
	(s == null ? o.s == null : s.equals(o.s));
")

(custom-autoload 'jdee-gen-equals-trailing-and-operators "jdee-gen" t)

(defvar jdee-gen-equals-parens-around-expression nil "\
Specifies whether the generated equals expression should be
surrounded by parentheses.
With `jdee-gen-equals-trailing-and-operators' set to nil:

    return ((a == o.a)
	    && (b == o.b)
	    && (s == null ? o.s == null : s.equals(o.s)));

Or, with `jdee-gen-equals-trailing-and-operators' set to t:

    return ((a == o.a) &&
	    (b == o.b) &&
	    (s == null ? o.s == null : s.equals(o.s)));
")

(custom-autoload 'jdee-gen-equals-parens-around-expression "jdee-gen" t)

(defvar jdee-gen-equals-method-template '("'>" "(when jdee-gen-create-javadoc" "'(l \"/**\" '> 'n" "    \" * Check if this object is equal (equivalent) to another object.\" '> 'n" "    \" */\" '> 'n" "))" "(jdee-gen-method-signature \"public\" \"boolean\" \"equals\" \"Object obj\")" "(jdee-gen-electric-brace)" "\"if (obj == this) return true;\" '> 'n" "\"if ((obj == null) || !getClass().equals(obj.getClass())) return false;\" '> 'n" "'> 'n" "(jdee-gen-equals-return \"obj\" \"o\") '> 'n" "\"}\" '> 'n))") "\
*Template for creating an equals method.
Setting this variable defines a template instantiation command
`jdee-gen-equals-method', as a side-effect.")

(custom-autoload 'jdee-gen-equals-method-template "jdee-gen" nil)

(autoload 'jdee-gen-equals-return "jdee-gen" "\
Generate a body of an appropriate override for the
java.lang.Object#equals(Object) function. This function gets the
list of member variables from`jdee-parse-get-serializable-members'.

The first optional parameter `parm-name' is the parameter name of
the Object argument of the equals function.  Default is \"obj\".

The second optional parameter `var-name' denotes the variable
name used to cast the \"obj\" argument to. The default is \"o\".

The third optional parameter `class' can be a semantic tag, which
is then used instead of the result of `semantic-current-tag'.

Example:
    class Bean {
	int a;
	long b;
	String s;
    }

Result:
    Bean o = (Bean) obj;

    return (a == o.a)
	&& (b == o.b)
	&& (s == null ? o.s == null : s.equals(o.s));

Or, with `jdee-gen-equals-trailing-and-operators' set to t:
    Bean o = (Bean) obj;

    return (a == o.a) &&
	(b == o.b) &&
	(s == null ? o.s == null : s.equals(o.s));

\(fn &optional PARM-NAME VAR-NAME CLASS SUPER-METHOD)" t nil)

(defvar jdee-gen-hashcode-method-template '("'>" "(when jdee-gen-create-javadoc" "'(l " "\"/**\" '> 'n" "\" * Calculate the hash code for this object.\" '> 'n" "\" * \" '> 'n" "\" * <p>The rules laid out in J. Blosh's Effective Java are used\" '> 'n" "\" * for the hash code calculation.</p>\" '> 'n" "\" * \" '> 'n" "\" * @return the hash code.\" '> 'n" "\" * \" '> 'n" "\" * @see java.lang.Object#hashCode\" '> 'n" "\" */\" '> 'n))" "(jdee-gen-method-signature \"public\"\"int\" \"hashCode\" nil)" "(jdee-gen-electric-brace)" "(jdee-gen-hashcode-body) '> 'n" "\"}\" '> 'n '>") "\
*Template for creating a hashCode method.
Setting this variable defines a template instantiation command
`jdee-gen-hashcode-method', as a side-effect.")

(custom-autoload 'jdee-gen-hashcode-method-template "jdee-gen" nil)

(autoload 'jdee-gen-hashcode-body "jdee-gen" "\
Generate a body of a hashCode function.
This function gets the list of member variables of the current
class from `jdee-parse-get-serializable-members'.

The first optional parameter `var-name' denotes the variable name used
to calculate the hash code, the default is \"code\".

The second optional parameter `class' can be a semantic tag, which is
then used instead of the result of `semantic-current-tag'.

\(fn &optional VAR-NAME CLASS)" t nil)

(defvar jdee-gen-tostring-method-template '("'>" "(when jdee-gen-create-javadoc" "'(l " "\"/**\" '> 'n" "\" * Get a string representation of this object.\" '> 'n" "\" * \" '> 'n" "\" * @return a string representation of this object.\" '> 'n" "\" * \" '> 'n" "\" * @see java.lang.Object#toString\" '> 'n" "\" */\" '> 'n))" "(jdee-gen-method-signature \"public\" \"String\" \"toString\" \"\")" "(jdee-gen-electric-brace)" "(jdee-gen-tostring-return) '> 'n" "\"}\" '>") "\
*Template for creating an toString method.
Setting this variable defines a template instantiation
command `jdee-gen-tostring-method', as a side-effect.")

(custom-autoload 'jdee-gen-tostring-method-template "jdee-gen" nil)

(autoload 'jdee-gen-tostring-return "jdee-gen" "\
Generate a body of an appropriate override for the
java.lang.Object#toString function. This gets the member variables
of the current class from semantic via `semantic-current-tag'.

\(fn &optional CLASS)" t nil)

(autoload 'jdee-gen-object-methods "jdee-gen" "\
Generates an equals(), a hashCode() and a toString method.

\(fn)" t nil)

(defvar jdee-gen-exception-buffer-template (list "(open-line 1) (funcall jdee-gen-boilerplate-function)" "(jdee-gen-get-package-statement)" "(progn (require 'jdee-javadoc) (jdee-javadoc-insert-start-block))" "\" * Exception <code>\" (jdee-parse-get-buffer-unqualified-class) \"</code>.\" '> 'n" "\" \" (jdee-javadoc-insert-empty-line)" "\" * Created: \" (current-time-string) '> 'n" "\" \" (jdee-javadoc-insert-empty-line)" "\" \" (jdee-gen-save-excursion (jdee-javadoc-insert 'tempo-template-jdee-javadoc-author-tag))" "\" \" (jdee-gen-save-excursion (jdee-javadoc-insert 'tempo-template-jdee-javadoc-version-tag))" "\" \" (jdee-javadoc-insert-end-block)" "\"public class \"" "(jdee-parse-get-buffer-unqualified-class)" "\" \" (jdee-gen-get-extend-class)" "(jdee-gen-electric-brace)" "'p'n" "'> (jdee-javadoc-insert-start-block)" "\"* Constructs a new <code>\" (jdee-parse-get-buffer-unqualified-class) \"</code> with\" '>'n" "\"* <code>null</code> as its detail message.\" '>'n" "'> (jdee-javadoc-insert-end-block)" "(jdee-gen-method-signature \"public\" nil (jdee-parse-get-buffer-unqualified-class) nil)" "(jdee-gen-electric-brace)" "\"}\"'>'n" "'n" "'> (jdee-javadoc-insert-start-block)" "\"* Constructs a new <code>\" (jdee-parse-get-buffer-unqualified-class) \"</code> with\" '>'n" "\"* the specified detail message.\" '>'n" "'> (jdee-javadoc-insert-empty-line)" "\"* @param message the detail message string.\" '> 'n" "'> (jdee-javadoc-insert-end-block)" "(jdee-gen-method-signature \"public\" nil (jdee-parse-get-buffer-unqualified-class) \"String message\")" "(jdee-gen-electric-brace)" "\"super(message);\" '> 'n" "\"}\" '> 'n" "'n" "'> (jdee-javadoc-insert-start-block)" "\"* Constructs a new <code>\" (jdee-parse-get-buffer-unqualified-class) \"</code> with\" '>'n" "\"* the specified cause and a detail message of\" '> 'n" "\"* <code>(cause == null ? null : cause.toString())</code>\" '> 'n" "\"* (which typically contains the class and detail message of cause).\" '> 'n" "'> (jdee-javadoc-insert-empty-line)" "\"* @param cause the causing throwable. A null value is permitted\" '> 'n" "\"*     and indicates that the cause is nonexistent or unknown.\" '> 'n" "'> (jdee-javadoc-insert-end-block)" "(jdee-gen-method-signature \"public\" nil (jdee-parse-get-buffer-unqualified-class) \"Throwable cause\")" "(jdee-gen-electric-brace)" "\"super(cause == null ? (String) null : cause.toString());\" '> 'n" "\"initCause(cause);\" '> 'n" "\"}\" '> 'n" "'n" "'> (jdee-javadoc-insert-start-block)" "\"* Constructs a new <code>\" (jdee-parse-get-buffer-unqualified-class) \"</code> with\" '>'n" "\"* the specified cause and message.\" '> 'n" "'> (jdee-javadoc-insert-empty-line)" "\"* @param message the detail message string.\" '> 'n" "\"* @param cause the causing throwable. A null value is permitted\" '> 'n" "\"*     and indicates that the cause is nonexistent or unknown.\" '> 'n" "'> (jdee-javadoc-insert-end-block)" "(jdee-gen-method-signature \"public\" nil (jdee-parse-get-buffer-unqualified-class) \"String message,Throwable cause\")" "(jdee-gen-electric-brace)" "\"super(message);\" '> 'n" "\"initCause(cause);\" '> 'n" "\"}\" '> 'n" "\"}\" '>" "(if jdee-gen-comments (concat \" // \" (jdee-parse-get-buffer-unqualified-class)))" "'>'n") "\
*Template for a new exception class.
Setting this variable defines a template instantiation
command `jdee-gen-exception', as a side-effect.")

(custom-autoload 'jdee-gen-exception-buffer-template "jdee-gen" nil)

(autoload 'jdee-gen-exception-buffer "jdee-gen" "\
Create a new Java buffer containing an exception class of the same name.
This command inserts the template generated by `jdee-gen-exception'.
It then moves the point to the location of the first method.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads nil "jdee-help" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-help.el"
;;;;;;  "de505f8f7779dfaa437d1e1eefa4bade")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-help.el

(autoload 'jdee-help-beanshell "jdee-help" "\
Displays the BeanShell documentation.

\(fn)" t nil)

(autoload 'jdee-help-browse-jdk-doc "jdee-help" "\
Displays the JDK doc in a web browser.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee-import" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-import.el"
;;;;;;  "d61d10eff382470467a91b5dbb8bbfc0")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-import.el

(autoload 'jdee-import-organize "jdee-import" "\
Organize import statements of the current Java source buffer.
If optional FORCE is non-nil force reordering even if imports are
already organized.

Imports are organized into groups returned by the function specified
by `jdee-import-group-function'.  Groups are inserted in the order they
are found unless `jdee-import-sorted-groups' requires that they must be
alphabetically sorted.  In each group imports are sorted by name
alphabetically or in reverse order if `jdee-import-reverse-sort-group'
is non-nil.  A blank line is inserted between groups.

Usage:
  \\[jdee-import-organize] group and sort import statements.
  \\[universal-argument] \\[jdee-import-organize] to force reordering.

The current buffer must be in `jdee-mode'.  This command requires a
version of the JDE with the semantic parser.

\(fn &optional FORCE)" t nil)

(autoload 'jdee-import-at-point "jdee-import" "\
Import a class at the current point.
The fully qualified class is received from user input.

\(fn CLASS)" t nil)

;;;***

;;;### (autoloads nil "jdee-java-properties" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-java-properties.el"
;;;;;;  "4cee5e8676c17cc714a257a7a04be648")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-java-properties.el

(autoload 'jdee-java-properties-mode "jdee-java-properties" "\
Major mode for Java properties files buffer.

\(fn)" t nil)

(autoload 'jdee-java-properties-validate "jdee-java-properties" "\
Determine whether or not the properties file conforms to the Sun
java.util.Properties format.  Only the first invalid line is found.

GOTO indicates whether or not to put the point at the first invalid line
found.

\(fn &optional GOTO)" t nil)

;;;***

;;;### (autoloads nil "jdee-javadoc" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-javadoc.el"
;;;;;;  "e06f80f9281eb06284df8a7ffe2cdc06")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-javadoc.el

(autoload 'jdee-javadoc-checker-previous "jdee-javadoc" "\
Go to the previous tag with doc errors.

\(fn)" t nil)

(autoload 'jdee-javadoc-checker-next "jdee-javadoc" "\
Goto the next tag with doc errors.

\(fn)" t nil)

(autoload 'jdee-javadoc-checker-fix "jdee-javadoc" "\
Fix documentation of checked tag.
Used in `jdee-javadoc-checker-report-mode'.

\(fn)" t nil)

(autoload 'jdee-javadoc-checker-quit "jdee-javadoc" "\
Quit the `jdee-javadoc-checker' report buffer.
Used in `jdee-javadoc-checker-report-mode'.

\(fn)" t nil)

(autoload 'jdee-javadoc-customize "jdee-javadoc" "\
Show the jdee-javadoc options panel.

\(fn)" t nil)

(autoload 'jdee-javadoc-autodoc-at-line "jdee-javadoc" "\
Update javadoc comment block for declaration at current line.

Uses the semantic bovinator parser table to find declarations (see
`jdee-javadoc-nonterminal-at-line').

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION.  IF NOT RESULT IS UNCERTAIN.

In the following examples, point is located at the beginning of the
line, before the word 'public' (but it could be anywhere on this
line):

1- Class example:
   -------------

-|-  public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

\\[jdee-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe class <code>MyClass</code> here.
+     *
+     * @author \"David Ponce\" <david.ponce@wanadoo.fr>
+     * @version 1.0
+     * @since 1.0
+     * @see MySuperClass
+     * @see Runnable
+     * @see java.io.Serializable
+     */
     public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

2- Method example:
   --------------

-|-  public
     void   myMethod( int  x,  int y )
       throws Exception
     {
       ...

\\[jdee-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe <code>myMethod</code> method here.
+     *
+     * @param x an <code>int</code> value
+     * @param y a <code>long</code> value
+     * @exception Exception if an error occurs
+     */
     public
     void   myMethod( int  x,  long y )
       throws Exception
     {
       ...

3- Field example:
   --------------

-|-  private static final int SIZE = 10;

\\[jdee-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe constant <code>SIZE</code> here.
+     */
     private static final int SIZE = 10;


`tempo' templates are used for each category of javadoc line.  The
following templates are currently defined and fully customizable (see
`tempo-define-template' for the different items that can be used in a
tempo template):

- - `jdee-javadoc-author-tag-template'
- - `jdee-javadoc-describe-class-template'
- - `jdee-javadoc-describe-constructor-template'
- - `jdee-javadoc-describe-interface-template'
- - `jdee-javadoc-describe-method-template'
- - `jdee-javadoc-describe-field-template'
- - `jdee-javadoc-exception-tag-template'
- - `jdee-javadoc-param-tag-template'
- - `jdee-javadoc-return-tag-template'
- - `jdee-javadoc-version-tag-template'

For example if you customize `jdee-javadoc-describe-class-template'
with the following value:

'(\"* \" (P \"Class description: \"))

you will be asked to enter the class description in the minibuffer.
See also the `jdee-javadoc-field-type', `jdee-javadoc-a' and
`jdee-javadoc-code' helper functions.

\(fn)" t nil)

(autoload 'jdee-javadoc-remdoc-at-line "jdee-javadoc" "\
Remove javadoc comment block for declaration at current line.
Require confirmation if optional NOCONFIRM is non-nil.
Return non-nil if done.
This uses `jdee-javadoc-nonterminal-at-line' to find declarations.

\(fn &optional NOCONFIRM)" t nil)

(autoload 'jdee-javadoc-checkdoc-at-line "jdee-javadoc" "\
Check javadoc comment block of declaration at current line.

Uses the semantic bovinator parser table to find declarations (see
`jdee-javadoc-nonterminal-at-line').

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION.  IF NOT RESULT IS UNCERTAIN.

\(fn)" t nil)

(autoload 'jdee-javadoc-checkdoc "jdee-javadoc" "\
Check doc comments of tags in the current buffer.
Report the next tag with documentation errors.

\(fn)" t nil)

(autoload 'jdee-javadoc-remove "jdee-javadoc" "\
Remove all Javadoc from the region (if one is active) or the buffer.
START, the start position in the buffer.
END, the end position in the buffer.

\(fn START END)" t nil)

(autoload 'jdee-javadoc-enable-menu-p "jdee-javadoc" "\
Return non-nil if corresponding doc menu item is enabled.
That is point is on the first line of a class, method, or field
definition.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "jdee-javadoc-gen" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-javadoc-gen.el"
;;;;;;  "f54aa944dad44bce74b01c91d1043703")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-javadoc-gen.el

(autoload 'jdee-javadoc-make-internal "jdee-javadoc-gen" "\
Generates javadoc for the current project if MAKE-PACKAGES-P
and `jdee-javadoc-gen-packages' are nonnil; otherwise, make doc
for the current buffer. This command runs the
currently selected javadoc's program to generate the documentation.
It uses `jdee-get-jdk-dir' to determine the location of
the currentlyh selected JDK. The variable `jdee-global-classpath' specifies
the javadoc -classpath argument. The variable `jdee-sourcepath'
specifies the javadoc  -sourcepath argument. You can specify all
other javadoc options via JDE customization variables. To specify the
options, select Project->Options->Javadoc from the JDE menu. Use
`jdee-javadoc-gen-packages' to specify the packages, classes, or source
files for which you want to generate javadoc. If this variable is nil,
this command generates javadoc for the Java source file in the current
buffer. If `jdee-javadoc-display-doc' is nonnil, this command displays
the generated documentation in a browser.

\(fn &optional MAKE-PACKAGES-P)" nil nil)

(autoload 'jdee-javadoc-make "jdee-javadoc-gen" "\
Generates javadoc for the current project. This command runs the
currently selected JDK's javadoc program to generate the documentation.
It uses `jdee-get-jdk-dir' to determine the location of the currently
selected JDK. The variable `jdee-global-classpath' specifies the javadoc
-classpath argument. The variable `jdee-sourcepath'
specifies the javadoc  -sourcepath argument. You can specify all
other javadoc options via JDE customization variables. To specify the
options, select Project->Options->Javadoc from the JDE menu. Use
`jdee-javadoc-gen-packages' to specify the packages, classes, or source
files for which you want to generate javadoc. If this variable is nil,
this command generates javadoc for the Java source file in the current
buffer. If `jdee-javadoc-display-doc' is nonnil, this command displays
the generated documentation in a browser.

\(fn)" t nil)

(autoload 'jdee-javadoc-make-buffer "jdee-javadoc-gen" "\
Generates javadoc for the current buffer. This command runs the
currently selected JDK's javadoc program to generate the
documentation. It uses `jdee-get-jdk-dir' to determine the location of the currently
selected JDK.  The variable `jdee-global-classpath' specifies the
javadoc -classpath argument. The variable `jdee-sourcepath' specifies
the javadoc -sourcepath argument. You can specify all other javadoc
options via JDE customization variables. To specify the options,
select Project->Options->Javadoc from the JDE menu. Use
`jdee-javadoc-make' to generate doc for the files and packages
specified by `jdee-javadoc-gen-packages'. If `jdee-javadoc-display-doc'
is nonnil, this command displays the generated documentation in a
browser.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee-junit" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-junit.el"
;;;;;;  "183d06c0b3d785edadf2d622fc3553d1")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-junit.el

(autoload 'jdee-junit-test-class "jdee-junit" "\
Instantiate a test class template.

\(fn)" t nil)

(autoload 'jdee-junit4-test-class "jdee-junit" "\
Instantiate a test class template.

\(fn)" t nil)

(autoload 'jdee-junit-test-class-buffer "jdee-junit" "\
Create a buffer containing a skeleton unit test class having the same name as the
root name of the buffer. This command prompts you to enter the file name
of the test class. It assumes that the file name has the form CLASSTest.java
where CLASS is the name of the class to be tested, e.g., MyAppTest.java. Use
`jdee-gen-junit-add-test-to-suite' to add tests to the test suite. Use of
tests generated with this template requires the JUnit test framework. For
more information, see http://www.junit.org.

\(fn)" t nil)

(autoload 'jdee-junit4-test-class-buffer "jdee-junit" "\
Create a buffer containing a skeleton unit test class having
the same name as the root name of the buffer. This command
prompts you to enter the file name of the test class. It assumes
that the file name has the form CLASSTest.java where CLASS is the
name of the class to be tested, e.g., MyAppTest.java. Use of
tests generated with this template requires the JUnit test
framework. For more information, see http://www.junit.org.

\(fn)" t nil)

(autoload 'jdee-junit-add-test-to-suite "jdee-junit" "\
Instantiate an addTest method.

\(fn)" t nil)

(autoload 'jdee-junit-run "jdee-junit" "\
Starts junit testrunner with buffer corresponding class name.

\(fn)" t nil)

(autoload 'jdee-junit-show-options "jdee-junit" "\
Show the JDE JUnit Options panel.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee-make" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-make.el"
;;;;;;  "127342d4debfd93c853d3ba966a9ab05")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-make.el

(autoload 'jdee-make "jdee-make" "\
Run the make program specified by `jdee-make-program' with the
command-line arguments specified by `jdee-make-args'. If
`jdee-read-make-args' is nonnil, this command also prompts you to enter
make arguments in the minibuffer and passes any arguments that you
enter to the make program along with the arguments specified by
`jdee-make-args'.

\(fn)" t nil)

(autoload 'jdee-make-show-options "jdee-make" "\
Show the JDE Make Options panel.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee-open-source" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-open-source.el"
;;;;;;  "e3ee2871e57b496a30eaca9079e0912f")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-open-source.el

(autoload 'jdee-read-class "jdee-open-source" "\
Select a class interactively.  PROMPT is used to prompt the user for the
first class name, FQ-PROMPT is used only if the class name expands into more
than one fully qualified name.

PROMPT text used to prompt the user for the simple class name, or
\"Class\" as the default.  Don't add the colon/space at the end
of this prompt as a default will be added if it exists.

FQ-PROMPT text used to prompt the fully qualified class name, or
\"Select qualified class\" as the default.  Don't add the
colon/space at the end of this prompt as a default will be added
if it exists.

THIS-CLASS-P, if non-nil, use the current class name if no class name at point
and we are in a JDEE buffer.

CONFIRM-FQ-P, if non-nil, confirm the class name even when there
is only one unique fully qualified class found for the simple
class name (that is the class without the package part in the
name).

NO-CONFIRM-NFQ-P, if non-nil, don't confirm the class to check for fully
qualified classes if it is obtainable from either the point or this class (see
THIS-CLASS-P).  If obtained from the point, then the class name is parsed with
`jdee-parse-class-name' for the input.

VALIDATE-FN is a function to validate the class.  This function takes an
argument the output from `jdee-parse-class-name' given from the first user input
class name query.  The function should raise an error if there is anything
wrong with the class.  If this is `nil', then no validate is done.

When called interactively, select the class and copy it to the kill ring.

\(fn &optional PROMPT FQ-PROMPT THIS-CLASS-P CONFIRM-FQ-P NO-CONFIRM-NFQ-P VALIDATE-FN)" t nil)

(autoload 'jdee-find-class-source "jdee-open-source" "\
*Find the source file for a specified fully qualified class.
Calls `jdee-find-class-source-file' to do the search.
If it finds the source file, it opens the file in a buffer.

\(fn CLASS &optional OTHER-WINDOW)" t nil)

;;;***

;;;### (autoloads nil "jdee-package" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-package.el"
;;;;;;  "9dc01efd67f105edf8272b9849dacc46")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-package.el

(autoload 'jdee-package-update "jdee-package" "\
Create or update the package statement in the current Java source
file buffer based on the file's location relative to the root of
the package directory as specified by one of the entries in
`jdee-package-search-classpath-variables' or `jdee-sourcepath'.
If these variables do not specify the root of the package directory,
this command does nothing. This command signals an error if the
 major mode of the current buffer is not `jdee-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jdee-parse" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-parse.el"
;;;;;;  "b4869e00c792066691bc6ac4b60510e5")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-parse.el

(autoload 'jdee-parse-get-buffer-class "jdee-parse" "\
Get the fully qualified name of the class of this buffer.

NO-PACKAGE-P, if non-`nil', return only the class name (sans
package name), otherwise, include the package name.

If called interactively, add the name in the mini-buffer.

\(fn &optional NO-PACKAGE-P)" t nil)

;;;***

;;;### (autoloads nil "jdee-parse-expr" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-parse-expr.el"
;;;;;;  "f510d671bca14e6a2017832ab67f523f")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-parse-expr.el

(autoload 'jdee-hungarian-to-reverse-camel-notation "jdee-parse-expr" "\
Converts Hungarian (i.e. m_sMyApp) to reverse camel notation (i.e. myApp).
Hungarian notation is used, for example, in most Microsoft visual C++ code
whereas reverse camel notation is used as the Sun Java standard style.

START the start of the buffer region
END is the end of the buffer region
LOCAL-REPLACEMENT-P, if non-nil, replace expecting a local variable
replacement.  This adds a `this.' to each replacment.

\(fn START END LOCAL-REPLACEMENT-P)" t nil)

(autoload 'jdee-camel-to-sql "jdee-parse-expr" "\
Convert from reverse camel notation to the SQL underscore convention.

\(fn START END)" t nil)

(autoload 'jdee-camel-to-c-const "jdee-parse-expr" "\
Convert from reverse camel notation to the C constant convention.

\(fn START END)" t nil)

(autoload 'jdee-camel-to-lisp "jdee-parse-expr" "\
Convert from reverse camel notation to the lisp symbol convention.

\(fn START END)" t nil)

(autoload 'jdee-member-to-attribute "jdee-parse-expr" "\
Convert Java member to a getter or setter syntax.

MEMBER-NAME is member to convret.  This is taken as the current word at point
when called interactively.

GETTERP, if non-nil, make it a getter, otherwise make it a setter.  If
\\[universal-argument] is used while calling interactively, then make it a
setter, otherwise, make a getter.

\(fn MEMBER-NAME GETTERP)" t nil)

;;;***

;;;### (autoloads nil "jdee-project" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-project.el"
;;;;;;  "b9606e015b66ae943f626504849ea63e")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-project.el

(autoload 'jdee-project-create-project "jdee-project" "\
Creates a JDE project.

\(fn)" t nil)

(autoload 'jdee-describe-path "jdee-project" "\
Prints and gives file existance for each path.
PATH-TYPE is either `global classpath' for `jdee-global-classpath' or
`source path' for `jdee-sourcepath'.

\(fn PATH-TYPE &optional BUF)" t nil)

;;;***

;;;### (autoloads nil "jdee-project-file" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-project-file.el"
;;;;;;  "3e8265ae76ddcba60b348cad0f6dffaa")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-project-file.el

(autoload 'jdee-open-project-file "jdee-project-file" "\
Open the project file for the Java source file in the current buffer.

\(fn)" t nil)

(autoload 'jdee-save-project "jdee-project-file" "\
Save source file buffer options in one or more project files.
This command provides an easy way to create and update a project file
for a Java project.  Simply open a source file, set the desired
options, using the JDEE Options menu, then save the settings in the
project file, using this command.  Now, whenever you open a source
file from the same directory tree, the saved settings will be restored
for that file.

\(fn)" t nil)

(autoload 'jdee-create-new-project "jdee-project-file" "\
Create a new JDEE project file in directory NEW-DIR, saving any
current customized variables.  If a project file already exists in the
given directory, the project is simply re-saved.  This functions the
same as `jdee-save-project' when no project files can be found for the
current source file.  But, if there already exist projects somewhere
along the path, this command unconditionally creates a project file in
the directory specified, thus allowing the user to create and maintain
hierarchical projects.

\(fn NEW-DIR)" t nil)

;;;***

;;;### (autoloads nil "jdee-refactor" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-refactor.el"
;;;;;;  "edd7144080d4a3d19bb0f258cb70e44a")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-refactor.el

(autoload 'jdee-rename-class "jdee-refactor" "\
Rename the current class to another class name.  This does a class name
string replace, changes the buffer name, and changes the file name.

\(fn NEW-CLASS-NAME)" t nil)

(autoload 'jdee-replace-fully-qualified-class-at-point "jdee-refactor" "\
Replace the unqualified class at the current point with the fully qualified
class.

\(fn CLASS)" t nil)

;;;***

;;;### (autoloads nil "jdee-run" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-run.el"
;;;;;;  "f117e8eb89399a9e62be9e3fb4c776c8")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-run.el

(autoload 'jdee-run-set-app "jdee-run" "\
Specify the name of the application class to run.

\(fn APP)" t nil)

(autoload 'jdee-run-set-args "jdee-run" "\
Specify arguments to be passed to the Java vm.
This command serves as an alternative to using the JDE Run Options
panel to specify command-line arguments for the Java interpreter.

\(fn ARGS)" t nil)

(autoload 'jdee-run-set-app-args "jdee-run" "\
Specify the arguments to be passed to the Java application class.
This command provides an alternative to using the JDE Run Options panel
to specify command-line arguments to pass to the application when starting
the application.

\(fn ARGS)" t nil)

(autoload 'jdee-run-set-applet-viewer "jdee-run" "\
Sets the viewer to be used to view an applet. The default is
appletviewer.

\(fn VIEWER)" t nil)

(autoload 'jdee-run-set-applet-doc "jdee-run" "\
Specify the doc to be used to view an applet.
This command provides an alternative to using the JDE Options
panel to specifying the applet document.

\(fn DOC)" t nil)

(autoload 'jdee-run "jdee-run" "\
Run the Java application specified by `jdee-run-executable', if
not the null string. Otherwise run the class specified by
`jdee-run-application-class', if non-null; otherwise the class in the
current buffer. Specifying a prefix argument, e.g., C-u C-c C-v C-r,
causes this command to prompt you to enter arguments to be passed to
the application's main method.  Specifying a minus prefix argument,
i.e., C-u - C-c C-v C-r, causes this command to prompt you to also
enter the name of the application's main class.  Note that you can use
`jdee-run-read-app-args' to cause the command to prompt you for
application arguments by default (i.e., without having to specify a
prefix argument). This command creates a comint buffer to allow you to
interact with the program.

\(fn PREFIX)" t nil)

(autoload 'jdee-run-applet "jdee-run" "\
Runs an applet. This function prompts you to enter the path of an
html document that displays the applet. If you enter return without
specifying a document, this function next checks whether
`jdee-run-applet-doc' specifies a document. If so, it displays that
specified document. Next, it checks whether the current directory
contains any html files. If the current directory contains an html
file with the same root name as the Java file in the current buffer,
it displays the file. If not, it displays the first html file that it
finds in the current directory. If if cannot find an html file, it
signals an error.  This function uses the viewer specified by
`jdee-run-applet-viewer' to display the specified document. Note that
if you run two html applet files successively with the same name, you
must kill the buffer created to run the first file before running the
second file. Otherwise, this command will simply redisplay the first
file.

\(fn &optional DOC)" t nil)

;;;***

;;;### (autoloads nil "jdee-stat" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-stat.el"
;;;;;;  "1a3bfb268567bc48cf46e5b602b44f6d")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-stat.el

(autoload 'jdee-stat-loc-report "jdee-stat" "\
Generates a report showing the number of code, comment,
javadoc, and blank lines in the current Java source buffer. Optionally
a total count could be passed to be displayed, as well as the number of
processed files.

\(fn &optional COUNT &optional TOTAL-FILES)" t nil)

(autoload 'jdee-stat-loc-report-project "jdee-stat" "\
Generates a report showing the number of code, comment,
javadoc, and blank lines in all the java files in the current
directory and subdirectories. This method will kill any
buffer containing a java file contained in dir.

\(fn DIR)" t nil)

(autoload 'jdee-stat-loc-report-directory "jdee-stat" "\
Generates a report showing the number of code, comment,
javadoc, and blank lines in all the java files in the current
directory. This method will kill any buffer containing a java file
contained in dir.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads nil "jdee-util" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-util.el"
;;;;;;  "88db940a1a239f7c7b10f526a3f64696")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-util.el

(autoload 'jdee-require "jdee-util" "\
Require FEATURE, either pre-installed or from the distribution.
 That is, first try to load the FEATURE library. Then try to load the
 jdee-FEATURE library from the JDEE's distribution.
 Signal an error if FEATURE can't be found.

\(fn FEATURE)" nil nil)

(autoload 'jdee-exception-goto "jdee-util" "\
Go to the Java source file and line indicated by an exception stack trace.

\(fn)" t nil)

(autoload 'jdee-htmlize-code "jdee-util" "\
Write the current code region as an HTML document.
Line numbers are added as well.

Requires ELPA package `htmlize'.

See `jdee-htmlize-code-destinations'.

\(fn START END &optional NO-LINE-NUMBERS-P)" t nil)

;;;***

;;;### (autoloads nil "jdee-which-method" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-which-method.el"
;;;;;;  "9df17c844fd40c72c3e9ed25f1b0a815")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-which-method.el

(defvar jdee-which-method-mode t "\
Enables the JDE's which method mode.
When which method mode is enabled, the current method name is
displayed in the mode line.")

(custom-autoload 'jdee-which-method-mode "jdee-which-method" t)

;;;***

;;;### (autoloads nil "jdee-xref" "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-xref.el"
;;;;;;  "52a4112db41614975b92811f322eb5c0")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-xref.el

(autoload 'jdee-xref-make-xref-db "jdee-xref" "\
Create a database of caller to callee (and the reverse) from the
classes in `jdee-built-class-path' and store the data in the location
specified by `jdee-xref-db-base-directory'

\(fn)" t nil)

(autoload 'jdee-xref-first-caller "jdee-xref" "\
Put the list of who calls the current function on the stack and
display the first caller.  Subsequent callers are displayed through
`jdee-xref-show-next-caller'.  STRICT should be true if the callers of
interfaces to a function, or calls to a superclass which may result in
a virtual function call to the subclass should not be considered.  In
other words, if STRICT is true, then only calls that are definitely to
the requested function are considered.

\(fn STRICT)" t nil)

(autoload 'jdee-xref-next-caller "jdee-xref" "\
If there are items still on the caller stack, pop the first one off
and show it

\(fn)" t nil)

(autoload 'jdee-xref-display-call-tree "jdee-xref" "\
Display an interactive call tree of which function call the current
  function, which can be expanded outward.  STRICT should be true if
  the callers of interfaces to a function, or calls to a superclass
  which may result in a virtual function call to the subclass should
  not be considered.  In other words, if STRICT is true, then only
  calls that are definitely to the requested function are considered.

\(fn STRICT)" t nil)

(autoload 'jdee-xref-list-uncalled-functions "jdee-xref" "\
Displays a simple list of function that are never called, at least
not directly.  Do not assume that this means this code can never be
reached, since reflection could always call any method.  Use this list
and your best judgement to figure out what are good candidates for
code cleanup.  STRICT should be true if the callers of interfaces to a
function, or calls to a superclass which may result in a virtual
function call to the subclass should not be considered.  In other
words, if STRICT is true, then only calls that are definitely to the
requested function are considered.  This function could take a
while. If it does, you might want to consider increasing
`jdee-xref-cache-size'.

\(fn STRICT)" t nil)

(autoload 'jdee-xref-update "jdee-xref" "\
Update the caller table after a recompile.  This can be called by
the user when they recompile outside of emacs.  It will update the
call list of all files modified in emacs

\(fn &rest IGNORED)" t nil)

(autoload 'jdee-xref-customize "jdee-xref" "\
Display the customization buffer for the xref package.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/elpa/jdee-20160304.536/beanshell.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/efc.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jcomplete.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-annotations.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-ant.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-bookmark.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-bsh.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-bug.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-checkstyle.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-class.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-compile.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-complete.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-custom.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-db.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-dbo.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-dbs.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-ecj-flymake.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-ejb.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-file-util.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-font-lock.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-gen.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-help.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-imenu.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-import.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-java-grammar.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-java-properties.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-javadoc-gen.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-javadoc.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-jdb.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-jdk-manager.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-juci.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-junit.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-make.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-open-source.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-package.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-parse-class.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-parse-expr.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-parse.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-pkg.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-plugins.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-project-file.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-project.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-refactor.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-run.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-stat.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-util.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-which-method.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-widgets.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-wiz.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee-xref.el"
;;;;;;  "../../../../../../.emacs.d/elpa/jdee-20160304.536/jdee.el")
;;;;;;  (22344 38197 594377 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; jdee-autoloads.el ends here
