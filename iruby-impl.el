;;; iruby-impl.el --- implementation support for iRuby

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
                             " ")))
         (fields (split-string (string-trim-right output) "\s+"))
         (impl (car fields))
         (version (cadr fields)))
    ;; In the intereset of portability with earlier IRB verisons, this
    ;; is retained from inf-ruby, with adaptations
    ;;
    ;; parsing only the major.minor.patchlevel versions here, for
    ;; compatibility with Emacs `version-to-list'. This may leave
    ;; out any ".pre...." suffix in the irb version string, such that
    ;; `version-to-list' may be unable to parse
    (setq version (or (ignore-errors
                        (mapconcat 'identity
                                   (subseq (split-string version "\\." t) 0 3)
                                   "."))
                      "9.9.9"))
    (cond
      ((ignore-errors (version<= "1.2.0" version)) "--nomultiline")
      (t "--noreadline"))))


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
    "Return the `iruby:impl-name' for a non-nil variable `iruby-buffer-interactive-impl'
in BUFFER, else nil

If the `iruby-buffer-interactive-impl' for BUFFER is non-nil yet the `iruby:impl-name'
for that implementation is nil, this will return a string computed from
the buffer name for BUFFER.

This method should return nil only in instancs when BUFFER is not a live
buffer, or when BUFFER has a null `iruby-buffer-interactive-impl'. Otherwise, this
mehod should return an implementation name for some interactive Ruby
environment in BUFFER"
    (when (buffer-live-p datum)
      (with-current-buffer datum
        (when iruby-buffer-interactive-impl
          (or (iruby:impl-name iruby-buffer-interactive-impl)
              (format "unknown(%s)" (buffer-name buffer))))))))


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
    :initform default-directory
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


(defun iruby:default-implementation (&optional context)
  "Return the default `iruby:ruby-language-impl' object.

This function uses the variable `iruby-default-implementation'

See also: `iruby:default-interactive-ruby'"
  (iruby:get-language-impl iruby-default-implementation))

(cl-defgeneric iruby:get-language-impl (datum &optional noerr)
  (:method ((datum string) &optional noerr)
    (or (cl-find datum iruby-ruby-language-impls
          :key #'iruby:impl-name
          :test #'string=)
        (if noerr nil
          (error "No implementation found for name %S" datum))))
  (:method ((impl iruby:impl) &optional noerr)
    impl))

;; (iruby:get-language-impl "ruby")
;; (iruby:get-language-impl "jruby")
;; (iruby:get-language-impl "thirdth" t)
;; (iruby:get-language-impl (iruby:get-language-impl "jruby"))

;; TBD: iruby:get-tools-impl (cmd) ... (yard/rdoc, erb, rbs, rake, ...)

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


(defun iruby-simple-process-sentinel (process state)
  "A default process sentinel to be installed by `run-iruby-new'

This function will generally be used as a default process sentinel for
iRuby process buffers, such that this function would be returned by
`iruby:get-process-sentinel' for any interactive Ruby implementation not
defining any more specialized method on that EIEIO generic function.

This function may be used with `set-process-sentinel'.

If PROCESS is not a running process and STATE, as a string - once
trimmed of any trailing newline - does not indicate a normal change in
process state, then a warninig will be produced in Emacs, indicating the
change in process state.

This function can be called directly as an Emacs process sentinel
callback, or called from any process-sentinel function that may be
returned by a method on `iruby:get-process-sentinel'."
  (let ((status (process-status process))
        (%state (or (ignore-errors (string-trim-right state))
                    state)))
    ;; parse out some normal exit states, before warn
    (unless (or (memq status '(exit finished))
                (equal %state "hangup")
                (equal %state "killed"))
      (warn "iRuby process %s state changed (%S): %S"
            process status %state))))

(cl-defgeneric iruby:get-process-sentinel (impl)
  "Return a process-sentinel to be used for the interactive Ruby
implementation IMPL.

In application in iRuby, this generic function allows for creating an
Emacs process sentinel function that would provide any behavior
specialized to a Ruby console class or interactive Ruby IMPL.

The effective method on this generic funtion must return a function
accepting two arguments, the first of which would be an Emacs process
and the second, a string depicting any change in the process state, as
denoted in the documentation for `set-process-sentinel'.

The returned function will normally be called with an iRuby process
buffer as `current-buffer', and thus should have access to any variables
that may normally be set to a non-nil value in an iRuby process buffer

The returned function may be implemented as an anonymous lambda or as a
globally defined Emacs Lisp function.

The default method will use  `iruby-simple-process-sentinel'

Advice for Implementors: The second argument received by the
process sentinel function may generally have been terminated with a
newline. `string-trim-right' can be used to to create a string that will
be removed of any trailing newline, before checking for the
string-encoded state change in the second argument of the process
sentinel function. The first argument to the process sentinel function,
i.e an Emacs process object, may be checked with e.g `process-status',
which returns a symbol, or `process-exit-status' which generally returns
a positive integer and may return zero for any running process"
  (:method ((impl string))
    "This method returns the function `iruby-simple-process-sentinel'"
    'iruby-simple-process-sentinel)
  (:method ((impl cons))
    "Return the value of `iruby:get-process-sentinel' called on the first
element of the shell command specifier IMPL"
    (iruby:get-process-sentinel (car impl)))
  (:method ((impl iruby:impl))
    "This method returns the function `iruby-simple-process-sentinel'"
    'iruby-simple-process-sentinel))

(cl-defgeneric iruby:process-pre-init (impl)
  "callback called from `run-iruby-new'"
  (:method ((impl cons))
    (ignore impl))
  (:method ((impl iruby:impl))
    (ignore impl)))

(cl-defgeneric iruby:process-post-init (impl)
  "callback called from `run-iruby-new'"
  (:method ((impl cons))
    (ignore impl))
  (:method ((impl iruby:impl))
    (ignore impl)))


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

(cl-defgeneric iruby:interactive-base-ruby (datum))
(cl-defgeneric iruby:interactive-base-args (datum))
(cl-defgeneric iruby:interactive-args (datum))

(cl-defgeneric iruby:interactive-binding-expr-list (datum)
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
    :initform 'iruby:default-implementation
    :initarg :base-ruby
    :accessor iruby:interactive-base-ruby
    :label "Base ruby"
    :type (or string function iruby:ruby-impl null)
    :custom (choice
              (string :tag "Implementation name")
              (function :tag "Implememtation selector function")
              (object :tag "Unique Ruby implementation"
                            :objecttype iruby:ruby-impl)
              (object :tag "Unique JRuby implementation"
                            :objecttype iruby:jruby-impl)
              (const nil :tag "No base implementation"))
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

This will be concatenated as a Ruby expression, to be used in internal
eval for the Ruby subprocess

Individual expressions in this list should be delimited with a
semicolon")
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
             (const :tag "Omit from cmd args"
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
             (const :tag "Omit from cmd args"
                    :omit)
             ;; NB the :check
             (const :tag "Check implementation dynamically"
                    :check))
   :documentation
   "Configuration for any readline arg on the interactive ruby

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
   :label "Prompt mode (arg)"
   :custom (choice
             (const nil :tag "None")
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
  (:method ((impl string))
    impl)
  (:method ((impl cons))
    impl)
  (:method ((impl iruby:interactive-ruby))

    (let* ((argmap (iruby:class-slot-initargs (eieio-object-class impl)))
           (load-args (list nil))
           (next-args load-args))
      (dolist (arginf argmap)
        (let ((initargs (cdr arginf))
              (slname (car arginf)))
          (when (and initargs
                     (slot-boundp impl slname))
            (let ((new-args (list (car initargs)
                                  (slot-value impl slname))))
              (setf (cdr (last next-args)) new-args)
              (setq next-args new-args)
            ))))
      (cons (class-name (class-of impl))
            (cdr load-args)))))

(defclass iruby:irb-binding (iruby:interactive-ruby)
  ((bin
    :initform "irb")
   (requires
    ;;NB this slot's value should be joined with the impl's requires slot
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
    ;; FIXME this prevents any reuse for "normal command strings"
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
          (iruby-irconc new args-next)))
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


(provide 'iruby-impl)
