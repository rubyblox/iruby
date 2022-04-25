;;; iruby-impl.el --- implementation supoprt for iRuby

(eval-when-compile
  (require 'cl-macs))

(require 'eieio)
(require 'eieio-base)

;;
;; utility forms for iruby-impl.el
;;

(require 'iruby-util)

(defmacro iruby-rconc (newcdr seq)
  (with-symbols-iruby (new lst)
    `(let* ((,new ,newcdr)
            (,lst (last ,new)))
       (rplacd (last ,seq) ,new)
       (setq ,seq ,lst))))


(defun iruby-irb-rl-arg (cmdlist)
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


(defsubst iruby-split-shell-string (str)
  (cond
    ((or (> emacs-major-version 28)
         (and (= emacs-major-version 28) (>= emacs-minor-version 1)))
     (split-string-shell-command str))
    (t (split-string-and-unquote str))))


(defsubst iruby-class-slots (cls)
  (let ((use-cls
         (cl-etypecase cls
           (symbol (find-class cls))
           (eieio--class cls cls))))
  (cl-map 'list 'cl--slot-descriptor-name
          (cl--class-slots use-cls))))



;; (iruby-class-slots (find-class 'iruby-wrapper-binding))

(eval-when ()
  (cl-intersection
   (iruby-class-slots (find-class 'iruby-wrapper-binding))
   (iruby-class-slots (find-class 'iruby-interactive-binding))
   :test #'eq)
)


;;
;; main code, iruby-impl.el
;;

(cl-defgeneric iruby-impl-name (datum)
  (:method ((datum cons))
    "Determine an implementation name from the shell command list in DATUM"
    (let ((bin (car datum)))
      (cl-typecase bin
        (string (file-name-nondirectory bin))
        ;; fallback - no value substitution here
        (t (prin1-to-string bin))))))

(cl-defgeneric iruby-impl-bin (datum))
(cl-defgeneric iruby-impl-requires (datum))
(cl-defgeneric iruby-impl-args (datum))


(defclass iruby-impl (eieio-named)
  ;; NB this class schema could be translated to a gsettings schema
  ;; for broader IDE applications
  ((name
    :initarg :name
    :accessor iruby-impl-name
    :label "Name"
    :initform "(name required)"
    :type string
    :custom (string :tag "Implementation name")
    :documentation
    "Implementation name, for interactive forms and associative data")
   (bin
    :initarg :bin
    :reader %iruby-impl-bin
    :writer  (setf iruby-impl-bin)
    :label "Command binary name"
    :initform nil
    :type (or string null)
    :custom (choice (string :tag "Command name or path")
                    (const :tag "Use implementation name" nil))
    :documentation
    "Implementation command name or path

If non-nil, this value should provide a relative or absolute pathname
for an executable file on the filesystem.

If nil, the implementation name will be reused as the implementation
bin name")
  (requires
   :initarg :requires
   :accessor iruby-impl-requires
   :initform nil
   :type list
   :label "Libraries (require)"
   :custom (repeat (string :tag "Library name"))
   :documentation
   "List of libraries to require with this implememtation")
   (args
    :initarg :args
    :accessor iruby-impl-args
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
   ) ;; slots
  ;; NB not exacctly a CLOS-like syntax in the class option with eieio
  :documentation "Base class for definitions of iRuby implementations"
  :abstract t)


;; insufficient ..
;; (cl-defmethod eieio-object-name-string ((obj iruby-impl))
;;   (iruby-impl-name obj))

(defun iruby-get-default-implementation (&optional context)
  "Return the default `iruby-ruby-language-impl' object.

This function uses the variable `iruby-default-implementation'

See also: `iruby-get-default-interactive-binding'"
  (iruby-get-language-impl iruby-default-implementation))

(cl-defgeneric iruby-get-language-impl (datum &optional noerr)
  (:method ((datum string) &optional noerr)
    (or (cl-find datum iruby-ruby-language-impls
          :key #'iruby-impl-name
          :test #'string=)
        (if noerr nil
          (error "No implementation found for name %S" datum))))
  (:method ((impl iruby-impl) &optional noerr)
    impl))

;; (iruby-get-language-impl "ruby")
;; (iruby-get-language-impl "jruby")
;; (iruby-get-language-impl "thirdth" t)
;; (iruby-get-language-impl (iruby-get-language-impl "jruby"))

(cl-defmethod iruby-impl-name ((datum string))
  (iruby-impl-name (iruby-get-language-impl datum)))

;; (iruby-impl-name "ruby")

(cl-defmethod iruby-impl-bin ((datum iruby-impl))
  ;; NB applicable for any iruby-impl
  (or (%iruby-impl-bin datum)
      (iruby-impl-name datum)))

(cl-defmethod iruby-impl-bin ((datum string))
  ;; NB applicable only for a registered iruby-language-impl
  (iruby-impl-bin (iruby-get-language-impl datum)))

(cl-defmethod iruby-impl-requires ((datum string))
  (iruby-impl-requires (iruby-get-language-impl datum)))

(cl-defmethod iruby-impl-args ((datum string))
  (iruby-impl-args (iruby-get-language-impl datum)))

(defclass iruby-ruby-language-impl (iruby-impl)
  ;; FIXME provide a subclass integrating with solargraph, via eglot
  ;;
  ;; see also: ssh-agency [melpa]
  ()
  :abstract t)

(defclass iruby-ruby-impl (iruby-ruby-language-impl)
  ()
  :documentation
  "Interface for an implementation of the Ruby language")

(defclass iruby-jruby-impl (iruby-ruby-language-impl)
  ()
  :documentation
  "Interface for a JRuby implementation of the Ruby language")

(defun iruby:make-ruby-language-impl (name &rest initargs)
  (apply 'iruby-ruby-impl :name name initargs))

(defun iruby:make-jruby-language-impl (name &rest initargs)
  (apply 'iruby-ruby-impl :name name initargs))

;; TBD rbenv x impl declarations here

(cl-defgeneric iruby:interactive-base-ruby (datum))
(cl-defgeneric iruby:interactive-base-args (datum))
(cl-defgeneric iruby:interactive-args (datum))

(cl-defgeneric iruby:interactive-binding (datum))
(cl-defgeneric iruby:interactive-completion (datum))

(cl-defgeneric iruby:interactive-binding-expr (datum)
  (:method (datum)
    ;; FIXME consider parsing for functions here,
    ;; assuming some implementation object for the
    ;; first arg to each
    (let ((seq  (iruby:interactive-binding datum)))
      (when seq (apply 'concat seq)))))

(cl-defgeneric iruby:interactive-completion-expr (datum)
  (:method (datum)
    ;; FIXME consider parsing for functions here,
    ;; assuming some implementation object for the
    ;; first arg to each
    (let ((seq  (iruby:interactive-completion datum)))
      (when seq (apply 'concat seq)))))

(cl-defgeneric iruby:interactive-use-multiline (datum))
(cl-defgeneric iruby:interactive-use-readline (datum))

(cl-defgeneric iruby:interactive-prompt-mode (datum))

(defclass iruby-interactive-binding (iruby-impl)
  ((base-ruby
    :initform 'iruby-get-default-implementation
    :initarg :base-ruby
    :accessor iruby:interactive-base-ruby
    :label "Base ruby"
    :type (or string function iruby-ruby-impl null)
    :custom (choice
              (string :tag "Implementation name")
              (function :tag "Implememtation selector function")
              (object :tag "Unique Ruby implementation"
                            :objecttype iruby-ruby-impl)
              (object :tag "Unique JRuby implementation"
                            :objecttype iruby-jruby-impl)
              (const nil :tag "No base implementation"))
    :documentation
    "Designator for which ruby to use with this interactive implementation.

If a function, the function will be called with zero arguments and
should return an instance of an `iruby-language-impl'.

If a string, the string should name an `iruby-language-impl'
accessible to `get-iruby-language-impl'.

If an `iruby-language-impl' object, the provided implementation will be
used singularly with this `iruby-interactive-binding'

If the value `nil', then no base implementation will be used. The
`iruby-impl-bin' for this implementation should then provide a shell
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
    "In `iruby-interactive-binding' instances, this slot's value will be
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
    :accessor iruby:interactive-binding
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
    :accessor iruby:interactive-completion
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


(defclass iruby-irb-binding (iruby-interactive-binding)
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
  (apply 'iruby-irb-binding :name name args))

(defclass iruby-pry-binding (iruby-interactive-binding)
  ((bin
    :initform "pry")
   (requires
    :initform '("pry"))
   (base-args
    :initform '("-e" "Pry::CLI.start(Pry::CLI.parse_options)"))
   (args
    ;; some defaults for use with iRuby
    :initform ("--no-pager" "--no-history"))
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


(cl-defmethod iruby-get-language-impl ((datum iruby-interactive-binding)
                                       &optional noerr)
  (cl-block top
    (let ((base (iruby:interactive-base-ruby datum)))
      (if (null base)
          (cl-return-from top)
        (when (functionp base)
          (setq base (funcall base datum)))
        (when (stringp base)
          (setq base (iruby-get-language-impl base noerr)))
        (cond
          ((typep base 'iruby-impl) base)
          (noerr nil)
          (t (error "No iRuby ruby implementation found for name %S" base)))))))


(cl-defgeneric iruby-get-interactive-binding (datum &optional noerr)
  (:method ((datum iruby-interactive-binding) &optional noerr)
    datum)
  (:method ((datum string) &optional noerr)
    (or (cl-find datum iruby-interactive-bindings
          :key #'iruby-impl-name
          :test #'string=)
        (if noerr nil
          (error "No interactive implementation found for name %S" datum))))
  )


;; (iruby-get-interactive-binding "irb")
;; (iruby-get-interactive-binding  "pry")
;; (iruby-get-language-impl  (iruby-get-interactive-binding "irb"))
;; (iruby-get-language-impl  (iruby-get-interactive-binding "pry"))

(defun iruby-get-default-interactive-binding (&optional context)
  "Return the default `iruby-interactive-binding' object.

This function uses the variable `iruby-default-interactive-binding'.

See also: `iruby-get-default-implementation' and
`iruby-get-interactive-cmd'"
  (iruby-get-interactive-binding iruby-default-interactive-binding))

(cl-defgeneric iruby-get-interactive-cmd (datum)
  ;; *FIXME* also provide for environment bindings here/at-point-of-run
  ;;
  ;; see also, in iruby.el: `with-iruby-process-environment'
  (:method ((datum string))
    (iruby-parse-cmd (iruby-get-interactive-binding datum)))
  (:method ((datum iruby-interactive-binding))
    (iruby-parse-cmd iruby-interactive-binding)))


(defun iruby:make-pry-cmd (name &rest args)
  (apply 'iruby-pry-binding :name name args))

(defun iruby-get-cmd (datum &optional noerr)
  (cl-etypecase datum
    (string (or (cl-find datum iruby-interactive-bindings
                  :key #'iruby-impl-name
                  :test #'string=)
                (if noerr
                    nil
                  (error "No implementation found for name %S" datum))))
    (iruby-interactive-binding impl)))

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
          (iruby-rconc new next)))))
  (:method ((datum string))
    ;; FIXME this prevents any reuse for "normal command strings"
    (iruby:parse-cmd (iruby-get-interactive-binding datum)))
  (:method ((datum iruby-impl))
    (let* ((bin (iruby-impl-bin datum))
           (req (iruby-impl-requires datum))
           (base-arg-specs (iruby-impl-args datum))
           (arg-base (list bin))
           (args-next arg-base))

      (dolist  (r req)
        (iruby-rconc (list "-r" r) args-next))

      (dolist  (s base-arg-specs)
        (let ((new  (cl-etypecase s
                      (function (funcall s datum))
                      (string (list s)))))
          (iruby-irconc new args-next)))
      arg-base))
  (:method ((datum iruby-interactive-binding))
    (let* ((base-impl (iruby:interactive-base-ruby datum))
           (cmd (cl-etypecase base-impl
                  (null  (list (iruby-impl-bin datum)))
                  (string (iruby:parse-cmd
                           (iruby-get-language-impl base-impl)))
                  (function
                   (iruby:parse-cmd (funcall base-impl datum)))
                  (iruby-impl (iruby:parse-cmd base-impl))))
           ;; NB no further indirection onto the base impl here
           (addl-base-args (iruby:interactive-base-args datum))
           (req (iruby-impl-requires datum))
           (inter-args (iruby-impl-args datum))
           (ml (iruby:interactive-use-multiline datum))
           (rl (iruby:interactive-use-readline datum))
           (prompt-mode (iruby:interactive-prompt-mode datum))
           (args-next cmd))

      (dolist  (r req)
        (iruby-rconc (list "-r" r) args-next))

      (dolist (addl addl-base-args)
        (let ((new  (cl-etypecase addl
                      (function (funcall addl datum))
                      (string (list addl)))))
          (iruby-rconc new args-next)))

      (when base-impl
        (iruby-rconc (list "--") args-next))

      (when (and (not (eq ml :omit))
                 (not (eq ml :check)))
        (let ((new (list
                    (ecase ml
                      ((t) "--multiline")
                      ((nil) "--nomultiline")))))
          (iruby-rconc new args-next)))

      (when  (not (eq rl :omit))
        (let ((new (list
                    (ecase rl
                      ((t) "--readline")
                      ((nil) "--noreadline")
                      (:check (iruby-irb-rl-arg cmd))))))
          (iruby-rconc new args-next)))

      (dolist  (s inter-args)
        (let ((new  (cl-etypecase s
                      (function (funcall s datum))
                      (string (list s)))))
          (iruby-rconc new args-next)))

     (when prompt-mode
       (iruby-rconc (list prompt-mode) args-next))

      cmd))
  )


;;
;; binding wrappers
;;
;; e.g for bundle exec under the console functions from inf-ruby
;;

(defclass iruby-wrapper-binding (iruby-interactive-binding)
  ((wrapper-base-cmd
    :accessor iruby:wrapper-base-cmd
    :initarg :wrapper-base-cmd
    :type list)))

(cl-defgeneric iruby:initialize-instance-from (inst other)
  (:method ((inst iruby-wrapper-binding) (other iruby-interactive-binding))
    (let ((inst-sl (iruby-class-slots (class-of inst)))
          (other-sl (iruby-class-slots (class-of other))))
      (dolist (use-sl (cl-intersection
                       (iruby-class-slots (find-class 'iruby-wrapper-binding))
                       (iruby-class-slots (find-class 'iruby-interactive-binding))
                       :test #'eq)
               inst)
        (setf (eieio-oref inst use-sl) (eieio-oref other use-sl))))))


(cl-defmethod iruby:parse-cmd ((datum iruby-wrapper-binding))
  (let ((wrapped (when (cl-next-method-p)
                   (cl-call-next-method datum))))
    (append (iruby:wrapper-base-cmd datum)
            wrapped)))


(cl-defgeneric iruby-wrap-binding (wrapper base &optional name)
  (:method ((wrapper string) base &optional name)
    (iruby-wrap-binding (iruby-split-shell-string wrapper) base name))
  (:method ((wrapper list) (base iruby-interactive-binding) &optional name)
    (let* ((bin (car wrapper))
           (inst (iruby-wrapper-binding
                  :wrapper-base-cmd wrapper
                  :name (or name (file-name-nondirectory bin)))))
      (iruby:initialize-instance-from inst base)
      inst)))


(provide 'iruby-impl)


