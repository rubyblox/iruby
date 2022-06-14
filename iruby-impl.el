;;; iruby-impl.el --- implementation support for iRuby -*- lexical-binding: t; -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.

(eval-when-compile
  (require 'cl-macs))

(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)

;; ensure `iruby:class-slot-initargs' is available
(require 'iruby-eieio-utils)
(require 'iruby)

;;
;; utility forms for iruby-impl.el
;;

(require 'iruby-util)

(defsubst iruby:ensure-class (which)
  (cl-etypecase which
    (symbol (find-class which t))
    (eieio--class which)))

(defsubst iruby:class-slots (cls)
  (let ((use-cls
         (cl-etypecase cls
           (symbol (find-class cls))
           (eieio--class cls cls))))
  (cl-map 'list 'cl--slot-descriptor-name
          (cl--class-slots use-cls))))

(defun iruby:get-readline-arg (cmdlist)
  (let* ((output (shell-command-to-string
                  (mapconcat 'identity (append cmdlist (list "--version"))
                             " "))))
    (cl-destructuring-bind (impl version &rest other)
        (split-string (string-trim-right output) "\s+")
      (ignore impl other)
      ;; parsing only the major.minor.patchlevel versions here, for
      ;; compatibility with Emacs `version-to-list'. This may leave
      ;; out any ".pre...." suffix in the irb version string, such that
      ;; `version-to-list' may be unable to parse
      (setq version (or (ignore-errors
                          (mapconcat 'identity
                                     (cl-subseq (split-string version "\\." t)
                                                0 3)
                                     "."))
                        "9.9.9"))
      (cond
        ((ignore-errors (version<= "1.2.0" version)) "--nomultiline")
        (t "--noreadline")))))


(defvar iruby-ruby-version-suffix-re "[0-9.]+"
  "Regular expression for the version suffix of an interactive Ruby.

This variable is used in `iruby-cmd-name-no-version'")


(defun iruby-cmd-name-no-version (exec-cmd)
  "Try to guess the non-version part of the shell command name EXEC-CMD

EXEC-CMD should be provided as an absolute or relative file pathname, a
string. This string may be provided with a version suffix matching the
regular expression in the variable, `iruby-ruby-version-suffix-re'.

Examples:
 (iruby-cmd-name-no-version \"/usr/local/bin/irb27\")
  => \"irb\""
  (string-trim-right (file-name-nondirectory exec-cmd)
                     iruby-ruby-version-suffix-re))

;; (iruby-cmd-name-no-version "/usr/local/bin/irb27")
;; => "irb"


;;
;; main code, iruby-impl.el
;;

(cl-defgeneric iruby:impl-name (datum)
  (:method ((datum null))
    "Return NIL"
    (ignore datum)
    nil)
  (:method((datum string))
    "Return the nondirectory name of DATUM as an implementation name"
    (file-name-nondirectory datum))
  (:method ((datum cons))
    "Determine an implementation name from the shell command list in DATUM"
    (let ((bin (car datum)))
      (cl-typecase bin
        (string (iruby:impl-name bin))
        ;; fallback - no value substitution here
        (t (prin1-to-string bin)))))
  (:method ((datum buffer))
    "Return the `iruby:impl-name' for any iRuby interactive implementation in BUFFER,
else nil."
    (when (buffer-live-p datum)
      (with-current-buffer datum
        (unless (eq major-mode iruby-mode)
          (error "Not an iRuby process buffer; %S" buffer))
        (let ((impl iruby-buffer-interactive-impl))
          (or (when impl
                (iruby:impl-name impl))
              (format "unknown(%s)" (buffer-name datum))))))))

;;; ad hoc tests for iruby:impl-name (buffer)
;; (iruby:impl-name (generate-new-buffer " *TMP*"))
;; (iruby:impl-name (iruby-process-buffer (iruby-proc)))

(cl-defgeneric iruby:impl-bin (impl))
;; NB indirection for deriving the bin from the iruby:impl-name
;; such as when the %iruby:impl-bin on the instance is nil
(cl-defgeneric %iruby:impl-bin (impl))
(cl-defgeneric (setf iruby:impl-bin) (new-value impl))

(cl-defgeneric iruby:impl-requires (impl))
(cl-defgeneric (setf iruby:impl-requires) (new-value impl))

(cl-defgeneric iruby:impl-args (impl))
(cl-defgeneric (setf iruby:impl-args) (new-value impl))

(cl-defgeneric iruby:impl-initial-dir (impl))
(cl-defgeneric (setf iruby:impl-initial-dir) (new-value impl))

(defclass iruby:impl (eieio-named)
  ;; NB this class schema could be translated to a gsettings schema
  ;; for broader IDE applications
  ((name
    :initarg :name
    :accessor iruby:impl-name
    :label "Name"
    :initform "(name required)"
    :type string
    :custom (string :tag "Implementation name")
    :documentation
    "Implementation name, for interactive forms and associative data")
   (bin
    :initarg :bin
    :reader %iruby:impl-bin
    :writer  (setf iruby:impl-bin)
    :label "Command binary name"
    :initform nil
    :type (or string null)
    :custom (choice
             (string :tag "Command name or path")
             (const :tag "Use implementation name" nil))
    :documentation
    "Implementation command name or path

If non-nil, this value should provide a relative or absolute pathname
for an executable file on the filesystem.

If nil, the implementation name will be reused as the implementation
bin name")
  (requires
   :initarg :requires
   :accessor iruby:impl-requires
   :initform nil
   :type list
   :label "Libraries (require)"
   :custom (repeat (string :tag "Library name"))
   :documentation
   "List of libraries to require with this implememtation")
   (args
    :initarg :args
    :accessor iruby:impl-args
    :label "Additional command line arguments"
    :initform nil
    :type list
    :custom (repeat :tag "Arg specifiers"
              (choice
               (string :tag "Literal arg")
               (function :tag "Provider function")))
    :documentation
    "Optional Sequence of arguments and argument providers

Each element may be a string, representing a literal argument value, or
an argument provider function.

Each argument provider function should accept a single argument, the
object representing this Ruby language of the implementation, and should
return a list of strings to use as literal argument values. This list
will be effectively joined with any other elements provided in the args
specifier")
   (initial-dir
    ;; in defcustom buffers, this would show up as an editable lisp form.
    ;;
    ;; in actuality, that lisp form is the initiform for the slot.
    ;;
    ;; the efective slot value type is 'string'
    :initarg :initial-dir
    :accessor iruby:impl-initial-dir
    ;; the progn form may serve to address a compiler warning that may
    ;; occur if the initform was only the Lisp expression `default-directory'
    :initform (progn default-directory)
    ;; :type string ;; eieio might fail on this, also in subclasses
    ;;
    ;; For any Emacs Lisp expr set by the user under a defcustom
    ;; for this class,  TBD how that expr may be evaluated (or not)
    ;; for this slot's value, in the actual instance
    ;; - TBD wrapping with a higher-order reader method,
    ;;   to eval any slot value not a string, if needed
    :custom (sexp :tag "Initial Directory")
    :documentation
    "Initial directory for processes initialized for this Ruby"
    )
   ) ;; slots
  ;; NB not exacctly a CLOS-like syntax in the class option with eieio
  :documentation "Base class for definitions of iRuby implementations"
  :abstract t)

(defsubst iruby:impl-p (obj)
  "Return true if OBJ is an object of type `iruby:impl'

This should return true if OBJ is an instance of the EIEIO class
`iruby:impl' or an instance of some subclass of that class."
  (cl-typep obj 'iruby:impl))


(cl-defmethod iruby:impl-bin ((datum iruby:impl))
  ;; NB applicable for any iruby:impl
  (or (%iruby:impl-bin datum)
      (iruby:impl-name datum)))

(cl-defmethod iruby:impl-bin ((datum string))
  ;; NB applicable only for a registered iruby-language-impl
  (iruby:impl-bin (iruby:get-language-impl datum)))

(cl-defmethod iruby:impl-requires ((datum string))
  (iruby:impl-requires (iruby:get-language-impl datum)))

(cl-defmethod iruby:impl-args ((datum string))
  (iruby:impl-args (iruby:get-language-impl datum)))


(defclass iruby:generic-impl (iruby:impl)
  ()
  :documentation
  "Implementation class for user-provided interactive Ruby exec commands")

(defun iruby:make-generic-impl (cmd)
  "Create an `iruby:generic-impl' for the provided CMD

CMD may be a string or a cons of string values.

If a string, the CMD will be interpreted as providing a shell exec
command, tokenized to a list form with `iruby:split-shell-string'.

If a cons, the CMD will be interprted as providing a literal shell exec
command.

The following conventions may apply for the syntax and interpretation of
the CMD value:

- The first element in the command list should represent a shell command
  name, as a string literal. This may be provided as a relative file
  name accessible under `exec-path' or as an absolute file name
  representing some fileystem object compatible with system exec.

- Any arguments in the command list should be provided as representing
  string values.

- If CMD is provided as a cons, each value in the CMD will be
  passed literally to `start-file-process'.

- If CMD is provided as a string, any quoting in the string will be
  processed as during tokenization to a list form with the function
  `iruby:split-shell-string'

- The resulting command list will be used with `start-file-process' via
  comint. Shell glob patterns in CMD will be interpreted as literal strings"
  (let* ((cmd-exec (cl-typecase cmd
                     (cons cmd)
                     (string (iruby:split-shell-string cmd))))
         (cmd-bin (car cmd-exec))
         (cmd-name (file-name-nondirectory cmd-bin))
         (cmd-args (cdr cmd-exec)))
    (iruby:generic-impl :name cmd-name :bin cmd-bin :args cmd-args)))

(defclass iruby:ruby-language-impl (iruby:impl)
  ;; FIXME provide a subclass integrating with solargraph,
  ;; via eglot (non-interactive)
  ;;
  ;; see also: ssh-agency [melpa] and rake console classes in iRuby
  ()
  :abstract t)

(defclass iruby:ruby-impl (iruby:ruby-language-impl)
  ()
  :documentation
  "Interface for an implementation of the Ruby language")

(defclass iruby:jruby-impl (iruby:ruby-language-impl)
  ()
  :documentation
  "Interface for a JRuby implementation of the Ruby language")

(defun iruby:make-ruby-language-impl (name &rest initargs)
  (apply 'iruby:ruby-impl :name name :object-name name initargs))

(defun iruby:make-jruby-language-impl (name &rest initargs)
  (apply 'iruby:ruby-impl :name name :object-name name initargs))

;; TBD rbenv x impl declarations here


;;
;; defcustom bindings for iruby:language-impl kinds
;;

(defgroup iruby-impl nil
  ;; FIXME move to iruby-impl.el
  "Ruby implementation support for iRuby"
  :tag "iRuby implementations"
  :group 'iruby)


;;;###autoload
(defcustom iruby-ruby-language-impls
  (list (iruby:make-ruby-language-impl "ruby")
        (iruby:make-jruby-language-impl "jruby")
        ;; The following have not been locally tested
        ;;
        ;; TBD if macruby may need a distinct impl subclass
        (iruby:make-ruby-language-impl "macruby")
        ;;
        (iruby:make-ruby-language-impl "rubinius" :bin "rbx"))
  "Definitions for Ruby language support in iRuby

This variable provides a list of Ruby language implementations for
iRuby. Each element of the list is generally an instance of some
subclass of `iruby:ruby-language-impl'.

The fields of each element in this list and the list itself can be
edited in the `customize-option' buffer for this variable. Documentation
would be available in that buffer, as to the syntax and applications of
each item."
  :group 'iruby-impl
  :tag "Ruby language implementations"
  :type '(repeat (choice
                  (object
                   :objecttype iruby:ruby-impl :tag "Ruby implementation"
                   :match  (lambda (widget value)
                             (typep value 'iruby:ruby-impl)))
                  (object
                   :objecttype iruby:jruby-impl :tag "JRuby implementation"
                   :match  (lambda (widget value)
                             (typep value 'iruby:jruby-impl))))))

;;;###autoload
(defcustom iruby-default-implementation "ruby"
  "Which Ruby language implementation to use if none is specified.

This value should denote the name of a Ruby language implemenentation
initialized in the custom variable `iruby-ruby-language-impls'.

By default, the interactive implementations \"pry\" and \"irb\" will use
the default Ruby language implementation denoted here for initailizing
an interactive iRuby process.

For purposes of project configuration, this variable may be bound
withinin a local scope using Emacs Lisp forms such as described in the
Info node `(elisp)Directory Local Variables' and the Info node
`(elisp)File Local Variables'.

See also: `iruby-default-interactive-ruby'

In a detail of the iRuby API: This value is used by the function
`iruby:default-implementation'.  That function which may be used
whether individually or as a symbol, such as for a provider function to
the `:base-ruby' initialization argument of any subclass of the base
class, `iruby:interactive-ruby'. The EIEIO classes `iruby:pry-binding'
and `iruby:irb-binding' are provided as default implementations of this
base class, in iRuby.

The value may then be retrieved from the initialized instance, via the
`iruby:interactive-base-ruby' accessor on that instance.

This would be used generally in the orchestration of the function
`iruby:parse-cmd' for an `iruby:interactive-ruby' object."
  :tag "Default Ruby language implementation"
  :type (cons 'choice (mapcar #'(lambda (impl)
                                  (let* ((name (iruby:impl-name impl))
                                         (bin (%iruby:impl-bin impl))
                                         (tag (if bin
                                                  (format "%s (%s)" name bin)
                                                name)))
                                    (list 'const :tag tag name)))
                              iruby-ruby-language-impls))
  :group 'iruby-impl)


(cl-defgeneric iruby:get-language-impl (datum &optional noerr)
  (:method ((datum string) &optional noerr)
    (or (cl-find datum iruby-ruby-language-impls
          :key #'iruby:impl-name
          :test #'string=)
        (if noerr nil
          (error "No implementation found for name %S" datum))))
  (:method ((impl iruby:impl) &optional noerr)
    (ignore noerr)
    impl))

;; (iruby:get-language-impl "ruby")
;; (iruby:get-language-impl "jruby")
;; (iruby:get-language-impl "thirdth" t)
;; (iruby:get-language-impl (iruby:get-language-impl "jruby"))

;; TBD: iruby:get-tools-impl (cmd) ... (yard/rdoc, erb, rbs, rake, ...)

(defun iruby:default-implementation (&optional context)
  "Return the default `iruby:ruby-language-impl' object.

This function uses the variable `iruby-default-implementation'

See also: `iruby:default-interactive-ruby'"
  (iruby:get-language-impl iruby-default-implementation))


;;
;; interactive ruby definitions
;;

(cl-defgeneric iruby:interactive-base-ruby (datum))
(cl-defgeneric iruby:interactive-base-args (datum))
(cl-defgeneric iruby:interactive-args (datum))

(cl-defgeneric iruby:interactive-binding-expr-list (datum)
  (:method ((datum iruby:generic-impl))
    ;; provide for some guesswork here, based on the initial cmd
    ;; provided for the iruby:generic-impl - similar to the original
    ;; case of an implementation cmd provided as a literal cons of strings
    (iruby:interactive-binding-expr-list
     (cons (iruby:impl-bin datum) (iruby:impl-args datum))))
  (:method ((datum string))
    (iruby:interactive-binding-expr-list (iruby:split-shell-string datum)))
  (:method ((datum cons))
    ;; try to guess a binding form, if called for a Ruby impl provided
    ;; as a cmd string => cmd list
    (let* ((exec-cmd (car datum))
           (exec-name (iruby-cmd-name-no-version exec-cmd))
           (guessed-impl (iruby:get-interactive-ruby exec-name t)))
      (when guessed-impl
        (iruby:interactive-binding-expr-list guessed-impl)))))

;; (iruby:interactive-binding-expr-list "irb27")
;; => (not nil)
;; (iruby:interactive-binding-expr-list "/usr/local/bin/irb27")
;; => (not nil)
;; (iruby:interactive-binding-expr-list "/opt/local/bin/pry2000")
;; => (not nil)

(cl-defgeneric iruby:interactive-completion-list (datum)
  (:method ((datum iruby:generic-impl))
    ;; similar to the binding expression method, provide for some
    ;; guesswork here, based on the initial cmd for the generic impl
    (iruby:interactive-completion-list
     (cons (iruby:impl-bin datum) (iruby:impl-args datum))))
  (:method ((datum string))
    (iruby:interactive-completion-list (iruby:split-shell-string datum)))
  (:method ((datum cons))
    ;; try to guess a completion form, if called for a Ruby impl
    ;; provided as a cmd string => cmd list
    (let* ((exec-cmd (car datum))
           (exec-name (iruby-cmd-name-no-version exec-cmd))
           (guessed-impl (iruby:get-interactive-ruby exec-name t)))
      (when guessed-impl
        (iruby:interactive-completion-list guessed-impl)))))

;; (iruby:interactive-completion-list "irb27")
;; => (not nil)
;; (iruby:interactive-completion-list "/usr/local/bin/irb27")
;; => (not nil)
;; (iruby:interactive-completion-list "/opt/local/bin/pry2000")
;; => (not nil)

(cl-defgeneric iruby:interactive-binding-expr (datum)
  (:method (datum)
    (let ((seq  (iruby:interactive-binding-expr-list datum)))
      (when seq (apply 'concat seq)))))

(cl-defgeneric iruby:interactive-completion-expr (datum)
  (:method (datum)
    (let ((seq  (iruby:interactive-completion-list datum)))
      (when seq (apply 'concat seq)))))

(cl-defgeneric iruby:interactive-use-multiline (datum))
(cl-defgeneric iruby:interactive-use-readline (datum))

(cl-defgeneric iruby:interactive-prompt-mode (datum))

(defclass iruby:interactive-ruby (iruby:impl)
  ((base-ruby
    :initform (progn iruby-default-implementation)
    :initarg :base-ruby
    :accessor iruby:interactive-base-ruby
    :label "Base ruby"
    ;; the 'list' type case here is a workaround for a quirk in the side
    ;; effects of the defclass implementation. In some instances of type
    ;; checking, such as for EIEIO caching during defclass, the intiform
    ;; will have been processed then as a literal Lisp object, rather
    ;; than for its eventual application as a Lisp expression to be
    ;; evaluated to produce some literal Lisp object.
    ;;
    ;; The behaviors of initform handling in EIEO caching during defclass
    ;; may appear to be implemented as mostly orthogonal to the initform
    ;; evaluation during instance initialization.
    ;;
    ;; As the initorm expression is in a list form now, apparently the slot
    ;; value must accept the type 'list' here or it would result in an
    ;; error during defclass, in some EIEIO versions. TBD why this seems
    ;; to be limited to this specific slot.
    ;;
    ;; this is mainly why the variable name in the initform is wrapped
    ;; in a 'progn' form there.
    ;;
    ;; A literal cons type for this slot's value would not be well
    ;; supported in applications with iruby
    ;;
    ;; nil for this slot's value is not well supported either.
    ;;
    ;; With the default initform, once evaluated during instance
    ;; initialization, the slot's value should be neither nil nor a
    ;; cons.
    ;;
    ;; seen in: GNU Emacs 29.0.50
    ;;
    :type (or string iruby:ruby-impl list)
    :custom (choice
              (string :tag "Implementation name")
              (sexp :tag "Implememtation selector form")
              (object :tag "Unique Ruby implementation"
                            :objecttype iruby:ruby-impl)
              (object :tag "Unique JRuby implementation"
                            :objecttype iruby:jruby-impl)
              (const "No base implementation" ni))
    :documentation
    "Designator for which ruby to use with this interactive implementation.

If a function, the function will be called with zero arguments and
should return an instance of an `iruby-language-impl'.

If a string, the string should name an `iruby-language-impl'
accessible to `get-iruby-language-impl'.

If an `iruby-language-impl' object, the provided implementation will be
used singularly with this `iruby:interactive-ruby'

If the value `nil', then no base implementation will be used. The
`iruby:impl-bin' for this implementation should then provide a shell
command such as \"irb\" or \"pry\" that can be used for running this
interactive implementation." )
   (base-args
    :initform nil
    :initarg :base-args
    :accessor iruby:interactive-base-args
    :label "Base ruby args"
    :type list
    :custom (repeat :tag "Arg specifiers"
                    (choice
                     (string :tag "Literal arg")
                     (function :tag "Provider function")))
    :documentation
 "Command line arguments for the `base-ruby'

This slot uses a syntax equivalent to that for the `args' field. Values
derived from the the `args' slot in this class will be provided as
arguments to the interactive iRuby process.

If the `iruby:interactive-base-ruby' for this implementation is non-nil
then  the shell command arguments designated in  this `base-args' slot
will be provided as args to the underlying `base-ruby' implementation,
preceding the normal args terminator \"--\"

If the `iruby:interactive-base-ruby' for this implementation is nil then
the arguments interpolated from this `base-args' slot value will be
prepended to those provided in the `iruby:interactive-args' for the
interactive instance, without the args terminator \"--\".")
   (args
    :accessor iruby:interactive-args
    :label "Optional args for the interactive ruby"
    :documentation
    "In `iruby:interactive-ruby' instances, this slot's value will be
used to provide command args for the interactive ruby, subsequent of the
normal args terminator \"--\"

Similar to the syntax of the `base-args' slot, each element in this
slot's value may be a string, representing a literal argument value, or
an argument provider function.

Each argument provider function should accept a single argument, the
object representing this interactive Ruby, and should return a list of
strings to use as literal argument values. This list will be effectively
joined with any other elements provided in the args specifier")
   (binding
    :initform nil
    :initarg :binding
    :accessor iruby:interactive-binding-expr-list
    :label "Ruby Binding expression"
    :type list
    :custom (repeat :tag "Binding expression (Ruby)" string)
    :documentation "Optional binding expression

This slot's value will be concatenated as a Ruby expression, to be used
in internal eval for the Ruby subprocess

Individual expressions in this list should be delimited with a
semicolon \";\"")
   (completion
    :initform nil
    :initarg :completion
    :accessor iruby:interactive-completion-list
    :label "Ruby completion expression"
    :type list
    :custom (repeat :tag "Completion expression (Ruby)" string)
    :documentation "Optional completion expression

This will be concatenated as a Ruby expression, to be used for symbol
completion in the iRuby buffer

Individual expressions in this list should be delimited with a
semicolon.")
  (use-multiline
   :initform nil
   :initarg :use-multiline
   :accessor iruby:interactive-use-multiline
   :type (or boolean (member :check :omit))
   :custom (choice
             (const :tag "Yes" t)
             (const :tag "No" nil)
             (const :tag "Omit from args"
                    :omit)
             (const :tag "Check implementation dynamically"
                    :check))
   :documentation
   "Configuration for any multiline arg on the interactive ruby

If true, the interactive ruby will be invoked with --readline. While
this may result in any issues in I/O between the Emacs comint buffer and
the interactive Ruby, it's presented here as an option in the interest of
testing.

If nil,  the interactive ruby will be invoked with --nomultiline

If the symbol `:check',  the interactive ruby will be queried for its
version. If that version is available and generally earlier than
1.2.0, then --nomultline will be used. Else, the `use-readline' slot
will be queried. In the latter case - for any interactive Ruby of a
version newer than 1.2.0 - this would be semantically equivalent to
`:omit'

If the symbol `:omit' then no arg will be added to the interactive
cmdline for this slot's value.")
  (use-readline
   :initform nil
   :initarg :use-readline
   :accessor iruby:interactive-use-readline
   :type (or boolean (member :check :omit))
   :custom (choice
             (const :tag "Yes" t)
             (const :tag "No" nil)
             (const :tag "Omit from args"
                    :omit)
             ;; NB the :check
             (const :tag "Check implementation dynamically"
                    :check))
   :documentation
   "Configuration for any readline arg for the interactive ruby

If true, the interactive ruby will be invoked with --readline. While
this may result in any issues in I/O between the Emacs comint buffer and
the interactive Ruby, it's presented here as an option in the interest of
testing.

If nil,  the interactive ruby will be invoked with --noreadline

If the symbol `:check',  the interactive ruby will be queried for its
version. If that version is available and generally more recent than
1.2.0, then --noreadline will be used.

If the symbol `:omit' then no arg will be added to the interactive
cmdline for this slot's value.")
  (prompt-mode
   :initform nil
   :initarg :prompt-mode
   :accessor iruby:interactive-prompt-mode
   :type (or string null)
   :label "Prompt mode"
   :custom (choice
             (const nil :tag "No prompt mode")
             (string :tag "Literal arg")))
   ) ;; slots
  :documentation
  "Base Class for an interactive interface to a Ruby implementation"
  :abstract t)

(cl-defgeneric iruby:make-desktop-load-form (impl)
  ;; This is defined here and not in iruby-desktop.el as in order to
  ;; minimize the number of require calls needed in iruby-desktop.el
  ;;
  ;: This function may be called after after iruby is initialized in the
  ;; Emacs environment. This function would be used during `desktop-save'
  ;; for any iruby process buffer with a non-nil, local `desktop-save-buffer'
  ;; function. The binding would normally be set under `iruby-mode'
  (:method (impl)
    (iruby:make-load-form impl))
  ;; try to find a match onto some registered impl, interactor lists
  (:method ((impl iruby:ruby-language-impl))
    (let* ((impl-name (iruby:impl-name impl))
           (matched (cl-find impl iruby-ruby-language-impls
                      :test (lambda (it other)
                              (or (eq it other)
                                  (let ((other-name (iruby:impl-name other)))
                                    (string= impl-name other-name)))))))
      (if matched
          `(iruby:get-langauge-impl ,impl-name)
        (iruby:make-load-form impl))))

  (:method ((impl iruby:interactive-ruby))
    (let* ((impl-name (iruby:impl-name impl))
           (matched (cl-find impl iruby-interactive-impls
                      :test (lambda (it other)
                              (or (eq it other)
                                  (let ((other-name (iruby:impl-name other)))
                                    (string= impl-name other-name)))))))
      ;; return a selector form for the matched interactive impl, if found
      (if matched
          `(iruby:get-interactive-ruby ,impl-name)
        ;; this avoids any potentially problematic calls
        ;; to cl-call-next-method
        (iruby:make-load-form impl)))))

(cl-defgeneric iruby:irb-binding-tracer (object))
(cl-defgeneric (setf iruby:irb-binding-tracer) (new-value object))


(defclass iruby:irb-binding (iruby:interactive-ruby)
  ;; FIXME rename this class => iruby:irb-cmd
  ;; FIXME rename iruby:interactive-ruby => iruby:interactive-cmd
  ;; TBD implementation for rdbg (rubygem-debug)
  ((tracer
    ;; FIXME this slot's value is almost inaccessible to the user,
    ;; a limitation beside the encapsulation of the API
    :initarg :tracer
    ;; this arg for irb entails a gem dependency
    :intiform nil
    :accessor iruby:irb-binding-tracer
    :type boolean
    :label "IRB tracer"
    :custom (choice
             (const t :tag "Call IRB with --tracer")
             (const nil :tag "Call IRB without --tracer")))
   ;; set local initforms for some inherited slots
   (bin
    :initform "irb")
   (requires
    ;; NB this slot's value should be joined with the impl's requires slot
    ;;value,  when constructing the shell command args list for this irb
    :initform ("irb" "irb/completion"))
   (base-args
    :initform '("-e" "IRB.start"))
   (prompt-mode
    :initform  "--inf-ruby-mode")
   (binding
    :initform  '("IRB.CurrentContext.workspace.binding"))
   (completion
    :initform  (list (concat "IRB::InputCompletor::CompletionProc."
                             "call('%s','%s').compact.each{ |x| puts x }")))
   ) ;; slots
  :documentation "Local IRB command")

(defun iruby:make-irb-cmd (name &rest args)
  (apply 'iruby:irb-binding :name name args))

(defclass iruby:pry-binding (iruby:interactive-ruby)
  ((bin
    :initform "pry")
   (requires
    :initform '("pry"))
   (base-args
    :initform '("-e" "Pry::CLI.start(Pry::CLI.parse_options)"))
   (args
    ;; some defaults for Pry with iRuby
    ;; NB the --no-history option might not be recommended
    :initform ("--no-pager" "--no-color"))
   (use-multiline
    :initform :omit)
   (use-readline
    :initform :omit)
   (binding
    :initform '("Pry.toplevel_binding"))
   (completion
    :initform (list
               (concat
                "proc { |expr| "
                "if defined?(pry_instance.complete); "
                ;;
                ;; NB only the pry_instance case has been tested
                ;; here.
                ;;
                ;; Other alternatives are referenced in inf-ruby
                ;; and may be used for compatibility with earlier
                ;; pry releases
                "puts pry_instance.complete(expr); "
                "elsif defined?(_pry_.complete); "
                "puts _pry_.complete(expr); "
                "elsif defined?(_pry_); "
                "Pry.config.completer.build_completion_proc(binding, _pry_)."
                "call(expr).compact.each{ |x| puts x }; "
                "end; }.call('%s')")))
   ) ;; slots
  :documentation "Local Pry command")


(cl-defmethod iruby:get-language-impl ((datum iruby:interactive-ruby)
                                       &optional noerr)
  (cl-block top
    (let ((base (iruby:interactive-base-ruby datum)))
      (if (null base)
          (cl-return-from top)
        (when (functionp base)
          (setq base (funcall base datum)))
        (when (stringp base)
          (setq base (iruby:get-language-impl base noerr)))
        (cond
          ((iruby:impl-p datum) base)
          (noerr nil)
          (t (error "No iRuby ruby implementation found for name %S" base)))))))


(cl-defgeneric iruby:get-interactive-ruby (datum &optional noerr)
  (:method ((datum iruby:interactive-ruby) &optional noerr)
    datum)
  (:method ((datum string) &optional noerr)
    (or (cl-find datum iruby-interactive-impls
          :key #'iruby:impl-name
          :test #'string=)
        (if noerr nil
          (error "No interactive implementation found for name %S" datum))))
  )

;; (iruby:get-interactive-ruby "irb")
;; (iruby:get-interactive-ruby  "pry")
;; (iruby:get-language-impl  (iruby:get-interactive-ruby "irb"))
;; (iruby:get-language-impl  (iruby:get-interactive-ruby "pry"))

(defun iruby:default-interactive-ruby (&optional prefer)
  "Return a default `iruby:interactive-ruby' object

This function calls `iruby:get-interactive-ruby', providing a
default value for for the optional argument PREFER. If PREFER is nil,
the variable `iruby-default-interactive-ruby' will be used."
  (iruby:get-interactive-ruby
   (or prefer iruby-default-interactive-ruby)))

;; (iruby:default-interactive-ruby)


(cl-defgeneric iruby:get-initial-environment (datum)
  (:method (datum)
    ;; used with `with-iruby-process-environment'
    ;;
    ;; FIXME all of the iruby impls - including those provided as strings
    ;; from the minibuffer - could be implemented under a method
    ;; directly using this generic function
    ;;
    ;; - TBD how much of `run-iruby-new' and
    ;;  `iruby-initialize-impl-bindings'
    ;;  to implement under such method
    ;;
    ;; Presently, iRuby has left the EIEIO definitions
    ;; out of anything directly used by comint - interfacing
    ;; with comint functions mainly by way of a shell command
    ;; derived via `iruby:parse-cmd'.
    ;;
    ;; Of course, some variables set under `run-iruby-new' may
    ;; serve as a sort of way of interfacing with comint,
    ;; albeit essentially by way of buffer-local variables and
    ;; callbacks there.
    (append
     (cons (format "PAGER=%s" iruby-pager)
           ;; http://debbugs.gnu.org/15775
           (cl-remove-if (lambda (binding)
                           (equal (car (split-string binding "=")) "PAGER"))
                         process-environment)))))


(defun iruby:make-pry-cmd (name &rest args)
  (apply 'iruby:pry-binding :name name args))

(cl-defgeneric iruby:parse-cmd (datum)
  (:method ((datum cons))
    (let* ((ptr (list nil))
           (next ptr))
      (dolist (elt datum (cdr ptr))
        ;; perform some value substitution for any function => rest
        ;; forms in DATUM
        (let ((new  (cl-etypecase elt
                      (function (let ((it (funcall elt datum)))
                                  ;; here this allows for a function
                                  ;; that retuns a non-list
                                  (if (listp it)
                                      it
                                    (list it))))
                      (string (list elt)))))
          (iruby:rconc new next)))))

  (:method ((datum string))
    ;; FIXME obsolete
    ;; - enacpsulate any user-provided cmd strings via iruby:make-generic-impl
    ;; - this should not ever be reached then
    ;; - retained temporarily for purpose of illustration
    (iruby:parse-cmd (iruby:get-interactive-ruby datum)))
  (:method ((datum iruby:impl))
    (let* ((bin (iruby:impl-bin datum))
           (req (iruby:impl-requires datum))
           (base-arg-specs (iruby:impl-args datum))
           (arg-base (list bin))
           (args-next arg-base))

      (dolist  (r req)
        (iruby:rconc (list "-r" r) args-next))

       (dolist  (s base-arg-specs)
        (let ((new  (cl-etypecase s
                      (function (funcall s datum))
                      (string (list s)))))
          (iruby:rconc new args-next)))
      arg-base))
  (:method ((datum iruby:interactive-ruby))
    (let* ((base-impl (iruby:interactive-base-ruby datum))
           (cmd (cl-etypecase base-impl
                  (null  (list (iruby:impl-bin datum)))
                  (string (iruby:parse-cmd
                           (iruby:get-language-impl base-impl)))
                  (function
                   (iruby:parse-cmd (funcall base-impl datum)))
                  (iruby:impl (iruby:parse-cmd base-impl))))
           ;; NB no further indirection onto the base impl here
           (addl-base-args (iruby:interactive-base-args datum))
           (req (iruby:impl-requires datum))
           (inter-args (iruby:impl-args datum))
           (ml (iruby:interactive-use-multiline datum))
           (rl (iruby:interactive-use-readline datum))
           (prompt-mode (iruby:interactive-prompt-mode datum))
           (args-next cmd))

      (dolist  (r req)
        (iruby:rconc (list "-r" r) args-next))

      (dolist (addl addl-base-args)
        (let ((new  (cl-etypecase addl
                      (function (funcall addl datum))
                      (string (list addl)))))
          (iruby:rconc new args-next)))

      (when base-impl
        (iruby:rconc (list "--") args-next))

      (when (and (not (eq ml :omit))
                 (not (eq ml :check)))
        (let ((new (list
                    (ecase ml
                      ((t) "--multiline")
                      ;; FIXME jruby's irb does not recognize this arg
                      ;; @ jruby 9.x
                      ((nil) "--nomultiline")))))
          (iruby:rconc new args-next)))

      (when  (not (eq rl :omit))
        (let ((new (list
                    (ecase rl
                      ((t) "--readline")
                      ((nil) "--noreadline")
                      (:check (iruby:get-readline-arg cmd))))))
          (iruby:rconc new args-next)))

      (dolist  (s inter-args)
        (let ((new  (cl-etypecase s
                      (function (funcall s datum))
                      (string (list s)))))
          (iruby:rconc new args-next)))

     (when prompt-mode
       (iruby:rconc (list prompt-mode) args-next))

      cmd))
  )

;;
;; customization support for iruby.el
;;

;;;###autoload
(defcustom iruby-interactive-impls
  ;; creating one of an "irb" and "pry" interface for the default iRuby
  ;; implementation, and one  each of the language implementations
  ;; defined in the previous
  ;;
  ;; Albeit, this will be redundant for two instances in the default
  ;; value, here
  (let ((if '((iruby:make-irb-cmd . "irb")
              (iruby:make-pry-cmd . "pry")))
        (dflt (list nil))
        nxt)
    (setq nxt dflt)
    (dolist (inter if)
      ;; one of each interactive iRuby kind, using the default base-ruby
      (cl-destructuring-bind (initfn . inter-name) inter
        (let ((new (funcall initfn inter-name)))
          (iruby:rconc (list new) nxt))))
    (dolist (impl iruby-ruby-language-impls (cdr dflt))
      (let ((impl-name (iruby:impl-name impl)))
        (dolist (in if)
          ;; one interactive impl for each interactive iRuby kind
          ;; and each defined base-ruby
          (cl-destructuring-bind (initfn . inter-name) in
            (let ((new (funcall initfn (format "%s-%s" impl-name inter-name)
                                :base-ruby impl-name)))
              (iruby:rconc (list new) nxt)))))))
  "Definitions for interactive Ruby applications in iRuby

This variable provides a list of interactive Ruby language
implementations. Each element of the list is generally an instance of
some subclass of `iruby:interactive-ruby'.

The fields of each element in this list and the list itself can be
edited in the `customize-option' buffer for this variable. Documentation
would be available in that buffer, as to the syntax and applications of
each item."
  :group 'iruby-impl
  :tag "Interactive Ruby implementations"
  :type '(repeat (choice
                  (object :objecttype iruby:irb-binding  :tag "IRB"
                   :eieio-show-name t)
                  ;; FIXME the tag actually shown in the custom buffer,
                  ;; for each object under this choice widget, may not
                  ;; match the actual class of that object.
                  ;;
                  ;; To mitigate the possible ambiguity of object class
                  ;; cross-labeling under this choice widget in the
                  ;; custom buffer, this uses the eieio-show-name option
                  ;; for the custom widgets, together with the
                  ;; 'eieio-named' class
                  (object
                   :objecttype iruby:pry-binding :tag "Pry"
                   :eieio-show-name t)
                  )))

(defcustom iruby-default-interactive-ruby "irb"
  "Which interactive Ruby binding to use in iRuby, if none is specified.

This value should denote the `iruby:impl-name' of an iRuby
interactive binding initialized in the custom variable
`iruby-interactive-impls'

To use the default Ruby language implementation with either IRB or Pry,
this custom value should be either \"irb\" or \"pry\" respectively.

Other values may serve to denote the name of any initialized interactive
binding in the custom variable `iruby-interactive-impls'. Each of the
latter values may provide a specific implementation of the class
`iruby:interactive-ruby' e.g  for irb or pry, as to use a specific
`iruby-language-impl' as a base ruby, e.g ruby, jruby, or macruby.

For purposes of project configuration, this variable may be bound
withinin a local scope using Emacs Lisp forms such as described in the
Info node `(elisp)Directory Local Variables' and the Info node
`(elisp)File Local Variables'.

See also: The custom variables `iruby-default-implementation',
`iruby-ruby-language-impls', and `iruby-interactive-impls' "
  :tag "Default interactive Ruby implementation"
  :type (cons 'choice (mapcar #'(lambda (impl)
                                  (let* ((name (iruby:impl-name impl))
                                         (base
                                          (iruby:interactive-base-ruby impl))
                                         (tag (format "%s (%s, %s)"
                                                      name (class-of impl)
                                                      base)))
                                    (list 'const :tag tag name)))
                              iruby-interactive-impls))
  :group 'iruby-impl)

(make-variable-buffer-local
 (defvar iruby-buffer-binding-expr nil
   "ruby expression for binding in eval under the active implementation"))

(make-variable-buffer-local
 (defvar iruby-buffer-completion-expr nil
   "ruby expression for symbol name completion under the active
implementation"))

(defcustom iruby-debug nil
  "If non-nil, enable debugging messages in iRuby

See also: Macro `iruby-debug-info'"
  :tag "iRuby Debugging"
  :type '(choice (const :tag "Enable Debugging Messages" t)
          (const :tag "No Debugging Messages" nil))
  :group 'iruby-impl)

(defmacro iruby-debug-info (msg &rest args)
  "If `iruby-debug' is non-nil, produce a debugging message via 'warn'

MSG will be applied as an Emacs Lisp format string, given any provided ARGS"
  `(when iruby-debug
     ;; FIXME try to use some buffer other than the generic warnings buffer
     ;;
     ;; FIXME also handle when initilizing the ruby (set $DEBUG true)
     (warn ,msg ,@args)))


(defun iruby-read-interactor (&optional prompt)
  "Read the name of an implementation in `iruby-interactive-impls',
returning `iruby-default-implementation' if user has entered no text.

PROMPT will default to the string, \"Ruby Implementation: \""
  (let ((name
         (completing-read (or prompt "Ruby Implementation: ")
                          (mapcar #'iruby:impl-name iruby-interactive-impls)
                          nil t nil nil iruby-default-interactive-ruby)))
    (cl-find name iruby-interactive-impls
      :test #'equal :key #'iruby:impl-name)))

;;; ad-hoc test
;; (iruby-read-interactor)


;;
;; top-level interactive forms and integration with comint
;;
;; these are defined in this file, as due to these source forms having
;; source-level dependencies on earlier code in ruby-impl.el
;;

;;;###autoload
(defun iruby (&optional impl new name dir)
  "Run or switch to a Ruby process.

With prefix argument, prompts for which Ruby implementation to use, from
the list `iruby-interactive-impls'.

Otherwise, if there is an existing Ruby process in an iRuby buffer,
switch to that buffer.

IMPL should be nil, or a string match the name of an implementation in
`iruby-interactive-impls'.

SYNTAX should be nil, or a string matching a ruby-mode syntax in
`iruby-ruby-modes'.

If either value is nil, a reasonable default will be computed for that
value.

To run a ruby implementation not listed in `iruby-interactive-impls',
see also: `run-iruby'"
  (interactive
   ;; interactive bindings for `iruby' dispatching to `run-iruby'
   (let* ((console (unless current-prefix-arg
                     (iruby-find-console-buffer)))
           ;; first preference is to query if current-prefix-arg (see below)
          (use-console
           ;; second preference is to use any accessible console buffer
           (and console (buffer-live-p console)
                (yes-or-no-p (format "Use existing Ruby console? (%s) "
                                     (iruby:impl-name console)))))
          (new-console
           ;; third preference, create a new console, if a project dir
           ;; is accessible from default-directory
           (and (null (or current-prefix-arg use-console))
                (iruby-console-create :match-initialize nil)))

          (active-buff
           ;; fourth preference is to use any non-console iruby impl
           (and (null (or current-prefix-arg use-console new-console))
                (iruby-get-active-buffer :console-ok nil
                                         :live-p t)))

          ;; fifth preference is the fallback case denoted below
          (new (or current-prefix-arg
                   (and (not use-console)
                        (not new-console)
                        (not active-buff))))
          (dir (cond
                 (current-prefix-arg ;; read dir for new ruby
                  (iruby:read-directory "Start Ruby in directory: "))
                 (use-console ;; use the dir for the initialized console buffer
                  (with-current-buffer console default-directory))
                 (new-console ;; use a console dir, if available
                  (cdr new-console))
                 (active-buff ;; use an existing non-console buffer's dir
                  (with-current-buffer active-buff default-directory))
                 (t default-directory)))
          (interactor
           (cond
             ((and new current-prefix-arg)
              (iruby-read-interactor "Run interactive Ruby: "))
             (console (iruby-buffer-impl console))
             (new-console
              (iruby-console-create :start dir))
             (active-buff (iruby-buffer-impl active-buff))
             ;; fallback: use the default interactive ruby
             (t (iruby:default-interactive-ruby))))
          (interactor-impl
           (cl-typecase interactor
             (iruby:impl interactor)
             (t (iruby:make-generic-impl interactor)))))
     (list interactor-impl new (iruby:impl-name interactor-impl) dir)))
  (let ((use-impl (cl-typecase impl
                    (iruby:impl impl)
                    (t
                     (iruby:make-generic-impl impl))))
        (default-directory (or dir default-directory)))
    (run-iruby use-impl new (or name (iruby:impl-name use-impl)))))


;;;###autoload
(defun run-iruby (&optional impl new name)
  "Run an interactive Ruby process, input and output in a buffer.

IMPL may be an `iruby:interactive-ruby' object, a string providing a
shell command, or nil. If nil, the interactive binding named in
`iruby-default-interactive-ruby' will be used.

For an IMPL provided as a shell command, NAME defaults to the
nondirectory filename of the first element in the IMPL specifier -
whether IMPL is provided as a string or a list.

If IMPL is provided as an `iruby:interactive-ruby', NAME will be
derived from the name of the interactive binding.

If a new process is created and there is already a process matching
NAME, the name will be appended with a suffix as with
`generate-new-buffer-name'.

If NEW is nil, then if there is already a process running in a
corresponding buffer, switch to that buffer. If NEW is non-nil or no
corresponding buffer is found, a new process will be created in an iRuby
buffer.

When called interactively, a shell command will be requested for the
interactive Ruby implementation. If an empty shell command is entered,
then the `iruby-default-interactive-ruby' will be used. NAME will be
derived from the provided shell command, or from the default interactive
binding. The value of `current-prefix-arg' will be used as NEW.

Runs the hooks `comint-mode-hook' and `iruby-mode-hook'.

The interactive command `iruby' provides a higher-order wrapper on this
function. `iruby' will call `run-iruby' with function arguments selected
for the interactive or non-interactive call to `iruby'.

Type \\[describe-mode] in the process buffer for the list of commands."
  ;; This function is interactive and named like this for consistency
  ;; with `run-python', `run-octave', `run-lisp' and so on.
  ;; We're keeping both it and `iruby' for backward compatibility.
  (interactive (let ((cmd (read-shell-command "Run ruby command: ")))
                 (when (zerop (length cmd))
                   ;; user entered an empty string. Interpret this as nil,
                   ;; to be handled as such in the main function body
                   (setq cmd nil))
                 (list cmd nil current-prefix-arg)))
  (let* ((buffer
          (cond
            (new nil)
            ((iruby:impl-p impl)
             ;; try to find some existing buffer for the impl
             (catch 'found
               (dolist (elt iruby-process-buffers)
                 (cl-destructuring-bind (p . buff) elt
                   (ignore p)
                     ;; NB using an equal test for purpose of compatibility
                   (when (equal impl (iruby-buffer-impl buff))
                     (throw 'found buff))))))
            ;; try to find some default buffer
            (t  (iruby-get-active-buffer :console-ok nil
                                         :live-p t))))
         (use-impl
          ;; conditionally select an implementation to use,
          ;; if this function will be creating a new iruby process
          (when (or new (not (and buffer
                                  (buffer-live-p buffer)
                                  (iruby-process-running-p buffer))))
            (cl-typecase impl
              (iruby:impl impl)
              (null (iruby:default-interactive-ruby))
              (t (iruby:make-generic-impl impl))))))
    (when use-impl
      ;; create a new iruby process
      (unless name
        (setq name (iruby:impl-name use-impl)))
      (setq buffer (run-iruby-new use-impl name)))
    ;; in all instances, switch to the buffer
    (iruby-switch-to-ruby buffer)))

(defun run-iruby-new (command &optional name)
  "Create a new interactive Ruby process in a new buffer.

COMMAND is the command to call. This value may be provided as a string,
or as a list of a command name and literal arguments, all string values,
or as an instance of a subclass of `iruby:interactive-ruby'. If
provdied as a string, the string will be tokenized with the function
`iruby:split-shell-string' before being provdied to comint.

If COMMAND is provided as an `iruby:interactive-ruby' object, then
the shell command to use for the interactive Ruby process will be
constructed by calling `iruby:parse-cmd' on the provided COMMAND. That
function should return a list of string values. The first element
of the return value should reprsent a command for shell exec in comint,
with subsequent elements representing literal arguments for that shell
command.

NAME will be used for creating a name for the iRuby process buffer. If
NAME is not provided, then this function will use the return value of
the generic function `iruby:impl-name' called on the COMMAND object.

For providing any custom application behaviors for an implementation of
`iruby:interactive-ruby', a method may be defined on each of the
generic functions `iruby:process-pre-init' and `iruby:process-post-init'
as specialized on that class. Those functions will be called on the
COMMAND implementation respectively before and after the new iRuby
process is created. Each of these functions will be called with the
process' buffer as `current-buffer'.

This function will return the newly initilized iRuby process buffer."
  (let* ((impl (cl-typecase command
                 (iruby:impl command)
                 (t (iruby:make-generic-impl command))))
         (commandlist (iruby:parse-cmd impl))
         (name (or name (iruby:impl-name impl))))

    ;; FIXME with the new encapsulation onto iruby:generic-impl for
    ;; user-provided shell cmds, the following can be moved into a
    ;; method specialized on iruby:impl under some generic function

    ;; run pre-init now, before any buffer is created.
    ;;
    ;; if the pre-init function fails, this should avoid creating any new buffer
    (iruby:process-pre-init impl)

    (let* ((buffer-name (generate-new-buffer-name (format "*%s*" name)))
           (buffer (get-buffer-create buffer-name))
           ;; NB this should pick up any current major-mode for computing
           ;; the syntax to use in the iruby buffer
           (syntax (car (iruby-find-syntax)))
           process)

      (with-current-buffer buffer
        (with-iruby-process-environment (impl)
          (comint-exec buffer name
                       (car commandlist) nil (cdr commandlist))
          (setq process (get-buffer-process buffer))

          (set-process-sentinel process
                                (iruby:get-process-sentinel impl))
          (iruby-add-process-buffer process buffer)

          (set-buffer buffer)
          (iruby-mode) ;; may reset any buffer-local variables
          (iruby-initialize-buffer-for command syntax)
          (iruby-remember-ruby-buffer buffer)
          (iruby:process-post-init impl)))

      buffer)))

(provide 'iruby-impl)
