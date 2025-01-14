;;; iruby-console.el --- project support for iRuby -*- lexical-binding: t; -*-

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
  (require 'cl-macs)
  (require 'cl-generic))

(require 'iruby-util)
(require 'iruby-impl)
(require 'iruby-proc)

;;
;; utility forms
;;

(defsubst iruby-file-contents-match (file regexp &optional match-group)
  (with-temp-buffer
    (insert-file-contents file)
    (when (re-search-forward regexp nil t)
      (if match-group
          (match-string match-group)
        t))))

(defsubst iruby-expand-files (path &optional full)
  "expand any glob patterns in PATH, returning a list of file pathnames

PATH should contain a literal pathname expression or a pathname
expression including glob patterns.

If FULL is non-nil, a list of absolute pathnames will be returned. By
default, pathnames will be returned as relative to PATH.

This function will ensure that any autosave files or symbolic links to
noneixstent files will not be present in the returned list of
pathnames.

The syntax for PATH would be that used in `file-expand-wildcards'"
  (cl-remove-if 'auto-save-file-name-p
                (cl-remove-if-not 'file-exists-p
                                  (file-expand-wildcards path))))

(cl-defun iruby-find-console-buffer (&optional (dir default-directory) impl)
  ;; Caveats & Docs (FIXME needs docstring)
  ;;
  ;; - This assumes DIR exists and is readable, but does not assume as
  ;;   much about the initial-dir in each console buffer
  ;;
  ;; - If IMPL is provided (non-nil) it should be either an iruby:impl
  ;;   or a string, will be used for a name match to each iruby:console,
  ;;   before testing for a filesystem match onto DIR
  ;;
  ;; - If an IMPL is not provided, then the matching console buffer must
  ;;   match only for the impl-initial-dir in the buffer, as
  ;;   representing the same filesystem object as DIR. In this test for
  ;;   file equivalence, each dir must exist and be readable, also
  ;;   stored on the same filesystem device, furthermore having the sime
  ;;   file inode number as accessible in Emacs, for each dir in the
  ;;   test. This test will be limited to all live buffers in
  ;;   `iruby-process-buffers'. The fist buffer with a matching
  ;;   impl-initial-dir and (if provided) matching impl will be returned
  ;;
  ;; - This function does not check for buffers in containing
  ;;   directories.
  ;;
  ;; - This function may be called with a dir as returned from, e.g
  ;;
  ;;    (cdr (iruby-console-create some-dir :match-initialize nil))
  ;;
  ;;   If that value is non-nil, it would provide a value for the DIR
  ;;   argument to this function, representing the first containing
  ;;   project directory of SOME-DIR
  ;;
  (let ((name (cl-typecase impl
                (iruby:impl (iruby:impl-name impl))
                (t impl))))
    (catch 'search
      (cl-dolist (elt iruby-process-buffers)
        (let* ((buff (cdr elt))
               (buff-impl (when (buffer-live-p buff)
                            (iruby-buffer-impl buff))))
          (when buff-impl
            (with-current-buffer buff
              (and (if name
                       (equal name (iruby:impl-name buff-impl))
                     t)
                   (iruby:console-p buff-impl)
                   (let ((buff-dir (iruby:impl-initial-dir buff-impl)))
                     (cond
                       ((iruby:same-file-p buff-dir dir)
                        (throw 'search buff))
                       (t
                        ;; scan all containing directories of the
                        ;; initial DIR for a match to the console impl's
                        ;; initial directory
                        (let ((test-dir dir)
                              (up-dir (file-name-directory (directory-file-name dir))))
                          (while (and (file-readable-p up-dir)
                                      (not (equal test-dir up-dir)))
                            (if (iruby:same-file-p buff-dir up-dir)
                                (throw 'search buff)
                              (setq test-dir up-dir
                                    up-dir
                                    (file-name-directory
                                     (directory-file-name up-dir)))))))))))))))))

;;
;; base classes - iruby-console in EIEIO
;;
;; after the console API in inf-ruby
;;

(cl-defgeneric iruby:proto-impl-base-impl (instance))
(cl-defgeneric (setf iruby:proto-impl-base-impl) (new-value instance))

(defclass iruby:proto-impl (iruby:interactive-ruby)
  ((base-impl
    :accessor iruby:proto-impl-base-impl
    :initarg :base-impl
    :type iruby:impl)))

(cl-defgeneric iruby:initialize-instance-from (inst other)
  (:method ((inst iruby:proto-impl) (other iruby:interactive-ruby))
    (let ((inst-sl (iruby:class-slots (eieio-object-class inst)))
          (other-sl (iruby:class-slots (eieio-object-class other))))
      (dolist (common-sl (cl-intersection inst-sl other-sl :test #'eq)
               inst)
        (setf (eieio-oref inst common-sl) (eieio-oref other common-sl))))))9


(cl-defgeneric iruby:console-class-prefix-cmd (console)
  "Return the class-allocated prefix command for the CONSOLE

The effective method should return a cons of a shell command name and
zero or more shell command arguments, with each element as a string.

This command list will be prefixed to any shell command list for running
an interactive binding for the console.

The command list returned from this function should be of a syntax for
running the console implementation under comint.

The value returned by this generic function will be used for a CONSOLE
that has no prefix-cmd bound for the console instance.

Methods on this generic function should not return nil.

See also:
`iruby:console-prefix-cmd'
`iruby:console-instance-prefix-cmd-p'
`iruby:console-instance-prefix-cmd'")


(cl-defgeneric iruby:console-instance-prefix-cmd (console)
  "If CONSOLE has a bound prefix-cmd value, return the value

Methods on this generic function may err if CONSOLE does not have a
bound slot for the value.

See also:
`iruby:console-prefix-cmd'
`iruby:console-instance-prefix-cmd-p'
`iruby:console-class-prefix-cmd'")


(cl-defgeneric iruby:console-class-append-args (console)
  "Return the class-allocated shell append-args for the CONSOLE.

Methods on this generic function may return nil.

See also:
`iruby:console-prefix-cmd'
`iruby:console-instance-prefix-cmd-p'
`iruby:console-instance-append-args'")

(cl-defgeneric iruby:console-instance-append-args (console)
  "If CONSOLE has a bound append-args value, return the value

Methods on this generic function may err if CONSOLE does not have a
bound slot for the value.

See also:
`iruby:console-append-args'
`iruby:console-instance-append-args-p'
`iruby:console-class-append-args'")

(cl-defgeneric iruby:console-kind (datum)
  "Return the symbolic name for an `iruby:console-test'")


(defclass iruby:console-mixin ()
  ((kind
    :initarg :kind
    :accessor iruby:console-kind
    :type symbol)
   (class-prefix-cmd
    :type list
    :initforml nil
    ;; TBD hopefully the class allocation is inherited by subclases in EIEIO
    :allocation :class
    :accessor iruby:console-class-prefix-cmd
    :documentation
    "Wrapper command and prefix arguments for console instances of this type")
   (prefix-cmd ;; non initform
    :type list
    :initarg :prefix-cmd
    ;; how does this now return no-applicable-method on a subclass of iruby:console-mixin?
    :accessor: iruby:console-instance-prefix-cmd)
   (class-append-args
    :type list
    :initform nil
    :allocation :class
    :accessor iruby:console-class-append-args
    :documentation
    "Wrapper suffix arguments for console instances of this type")
   (append-args ;; no initform
    :type list
    :initarg :append-args
    :accessor: iruby:console-instance-append-args)
   ) ;; slots
  :abstract t)

(cl-defgeneric iruby:console-instance-prefix-cmd-p (console)
  "Return true if CONSOLE can be assumed to hold a unique prefix command.

See also: `iruby:console-prefix-cmd'"
  (:method ((console iruby:console-mixin))
    (slot-boundp console 'prefix-cmd)))

(cl-defgeneric iruby:console-instance-append-args-p (console)
  "Return true if CONSOLE can be assumed to hold an append-args value
unique for the console instance.

See also: `iruby:console-append-args'"
  (:method ((console iruby:console-mixin))
    (slot-boundp console 'append-args)))

(cl-defgeneric iruby:console-prefix-cmd (console)
  "Compute the shell command prefix for the CONSOLE's shell command"
  (:method ((console iruby:console-mixin))
    "If the CONSOLE has a prefix command unique for the instance, return
that value. Else, return the value of the class-allocated prefix command
for the CONSOLE.

The prefix command for the CONSOLE will be used as a prefix to the
underlying interactive implementation accessed by the CONSOLE.

For instance, this method may return the list (\"bundle\" \"exec\") to
ensure that any implementation selected by the user will be run under
that prefix shell command.

This method should return a cons value, i.e a non-empty list. Each
element of the return value should be a string, such that the first
element in the return value represents a shell command and subsequent
elements, arguments for that shell command. This return value will be
prefixed to any command list for each interactive Ruby implementation,
when initializing an iRuby process buffer for the console.

See also
`iruby:console-instance-prefix-cmd'
`iruby:console-class-prefix-cmd'
`iruby:console-append-args'"
    (if (iruby:console-instance-prefix-cmd-p console)
        (iruby:console-instance-prefix-cmd console)
      (iruby:console-class-prefix-cmd console))))

(cl-defgeneric iruby:console-append-args (console)
  "Compute the list of trailing arguments for the CONSOLE's shell command"
  (:method ((console iruby:console-mixin))
    "If the CONSOLE has a append-args unique for the instance, return
that value. Else, return the value of the class-allocated append-args
for the CONSOLE.

The value returned by this method will be used for providing any
trailing arguments after the implementation shell command wrapped by the
prefix command for the CONSOLE. This method may return nil.

See also
`iruby:console-instance-append-args'
`iruby:console-class-append-args'
`iruby:console-prefix-cmd'"
    (if (iruby:console-instance-append-args-p console)
        (iruby:console-instance-append-args console)
      (iruby:console-class-append-args console))))

(cl-defmethod iruby:parse-cmd ((datum iruby:console-mixin))
  (append (iruby:console-prefix-cmd datum)
          (iruby:parse-cmd (iruby:proto-impl-base-impl datum))
          (iruby:console-append-args datum)))


(defclass iruby:console (iruby:console-mixin iruby:proto-impl)
  ()
  :abstract t)


(defclass iruby:gemfile-console (iruby:console)
  ((class-prefix-cmd
    :initform '("bundle" "exec")))
  :abstract t)


(defclass iruby:rails-console (iruby:gemfile-console)
  ;; NB assumption: That every fails project uses a Gemfile
  ;;
  ;; FIXME -env ... see inf-ruby
  ;;
  ;; - used for the original hanmai and rails console types
  ;;
  ;; - inf-ruby provides forms for interactive query of which
  ;;   (rails?) environment to use, from a static list of environments
  ;;   e.g `inf-ruby-console-rails-env', `inf-ruby-console-hanmain-env'
  ;;
  ;; - iruby does not presently implement any method for environment
  ;;   selection for an `iruby:rails-console'
  ()
  :abstract t)


(defclass iruby:gem-console (iruby:console)
  ((class-append-args
    :initform  '("-I" "lib")))
  :abstract t)


(defclass iruby:script-console (iruby:console)
  ()
  :abstract t)

(cl-defgeneric iruby:console-test-console-class (datum)
  (:documentation
    "Return the console class to use for an `iruby:console-test'

An instance of the indicated console class will be created if
`iruby:console-match-p' returns true for this `iruby:console-test'"))

(cl-defgeneric iruby:console-test-test (datum)
  (:documentation
   "Return the intiial test form for an `iruby:console-test'

This value provides either a filename glob expression or a list of test
forms, generally for interpretation in `define-iruby-console'

If a string, the test wil be interpreted as a filename glob pattern
for any one or more files that must exist within or under some project
directory. If the glob pattern matches any one or more files under a
project directory, the test will be considered to have matched for that
project directory.

If a list, each element in the list should have the following syntax:

  (FILE-GLOB &rest PARSE-RE)

In this syntax of the test form, each FILE-GLOB must represent a
filename glob pattern for any one or more files, similar to the simpler
string syntax for the test. In either form of syntax, the FILE-GLOB
represents a relative pathname glob. The glob pattern  may be provdided
as to match any one or more files under some subidrectory of a project
directory, or to match a file directly within a project directory.

When iRuby is parsing for a match onto a project directory, then if a
file name matches for the FILE-GLOB, each regular expression in the list
PARSE-RE must match some section of text in the named file,
in order for the console type to be considered a match.

If no regular expressions are provided in this form, i.e if PARSE-RE is
an empty list, then the test will be assumed to have been passed if a
file exists that matches the FILE-GLOB

In this syntax for the test form, every provided PARSE-RE must match for
at least one file matching the FILE-GLOB in order for the defining
console type to be considered a match onto the contents of a project
directory."
))


(defclass iruby:console-test (iruby:console-mixin)
  ((console-class
    :initarg :console-class
    :accessor iruby:console-test-console-class
    :type (or symbol function))
   (test
    :initarg :test
    :accessor iruby:console-test-test
    :type (or string list))
   ) ;; slots
  )

(defvar iruby-console-tests nil
 "Sequence of iruby:console-test objects

This variable is used in the following forms:
- `iruby-register-console-test' to store an `iruby:console-test'
- `iruby-console-create' to run a provided `match-initialize' function
   on any first matched `iruby:console-test' within an upwards-recursive
   directory walk starting at a provided 'start' directory")

;;; reset the value:
;; (setq iruby-console-tests nil)

(defun iruby-register-console-test (instance)
  ;; FIXME this does not accept any precedence args
  ;; e.g :before symbol / :after symbol / :append t
  ;;
  ;; in effect, this overwrites any existing instance per the instance's
  ;; index in `iruby-console-tests' or else pushes the instance to become
  ;; the first element in `iruby-console-tests'
  ;;
  ;; it should be noted that earlier elements in `iruby-console-tests'
  ;; will receive higher precedence than later elements, for file tests
  ;;
  (let ((kind (iruby:console-kind instance))
        exists)
    (cl-dotimes (n (length iruby-console-tests))
      (when (eq kind (iruby:console-kind  (nth n iruby-console-tests)))
        (setq exists n)
        (cl-return)))
    (cond
      (exists (setf (nth exists iruby-console-tests)
                    instance)
              exists)
      (t (setq iruby-console-tests (cons instance iruby-console-tests))))))

(cl-defgeneric iruby:console-match-p (dir test))

(cl-defmacro define-iruby-console (kind (&rest superclasses) tests
                                           &optional slots
                                   &rest initargs)
  (cl-labels ((make-glob-re-matcher (elt)
                ;; create a test form for an ELT having a form
                ;; (GLOB &rest RE)
                (cl-destructuring-bind (glob . re-tests) elt
                  `(let ((files (file-expand-wildcards ,glob))
                         first-match)
                     (when files
                       (cl-block first-matched
                         (dolist (globbed files first-match)
                           ,(if (null re-tests)
                                `(cl-return-from first-matched globbed)
                              `(cond
                                 (first-match
                                  (cl-return-from first-matched first-match))
                                 (t
                                  (dolist (test (quote ,re-tests))
                                    (cond
                                      ;; NB this is generally used to
                                      ;; parse for gems listed in
                                      ;; Gemfile.lock.
                                      ;;
                                      ;; TMTOWTDI, e.g calling directly to
                                      ;; some Ruby implementation to show
                                      ;; what list of gems are being used in
                                      ;; some Gemfile, once initialized,
                                      ;; then matching on that list of gems
                                      ((iruby-file-contents-match globbed test)
                                       (setq first-match globbed))
                                      (t (setq first-match nil))))))))))))))
    (let ((console-class (intern (format "iruby:%s-console-provider" kind)))
          (test-class (intern (format "iruby:%s-console-test" kind))))
    `(progn
       ;; top-level utility forms
       ;; 1) define the console class
       ;; 2) define the console test class
       ;; 3) define a console test method for the console test class
       ;; 4) create and register an instance of the console test class

       (defclass ,console-class (,@(or superclasses '(iruby:console)))
         ((kind :initform (quote ,kind))
          ,@slots)
         ,@initargs)

       (defclass ,test-class (iruby:console-test)
         ;; set the initform for each inherited slot (stored to each class)
         ;; and specify :class allocation directly (maybe redundant)
         ())

       (cl-defmethod iruby:console-match-p ((dir string) (test ,test-class))
         (let ((default-directory dir))
           ,(cl-typecase tests
              (string `(car (file-expand-wildcards ,tests)))
              (cons
               `(and ,@(mapcar #'make-glob-re-matcher tests)))
              (t (error "Unrecognized test syntax in %S definition: %S"
                        kind tests)))))

       (iruby-register-console-test
        ;; EIEIO typically defines a constructor function of the same
        ;; name as each class
        ;;
        (,test-class :kind (quote ,kind)
                     :test (quote ,tests)
                     :console-class (quote ,console-class)))

       (find-class (quote ,console-class) t)
       ))))


(cl-defgeneric iruby:default-interactor-for (datum)
  (:method (datum)
    (iruby:default-interactive-ruby datum))
  (:method ((datum iruby:console-test))
    (ignore datum)
    ;; used in iruby-console-initialize-matched
    (iruby:default-interactive-ruby iruby-default-interactive-ruby)))

(cl-defun iruby-console-create (&key
                                (start default-directory)
                                (tests iruby-console-tests)
                                (match-initialize
                                 'iruby-console-initialize-matched))
  "Call a function for a matching project directory at or containing START.

The TESTS argument should provide a list of `iruby:console-test' objects.
This list will be applied in descending order of precedence, within an
upwards-recursive directory walk beginning at the directory provided as
START.

For the first matching directory at or containing the START directory,
the MATCH-INITIALIZE function will be called with two arguments, the
matching `iruby:console-test' and the matched directory. The value
returned by the MATCH-INITIALIZE function will be returned from this
function.

A nil value may be provided for MATCH-INITIALIZE, in which case the
return value will be a cons object its car as the matched test and its
cdr as the matched directory.

If no match is found, this function will return nil.

To locate any initialized `iruby:console' for a specific filesystem
directory, see also: `iruby-find-console-buffer'

This function is used by default in the interactive form under the
`iruby' Emacs Lisp command. In the implementation of that interactive
form, if no matching project directory is found then a new iRuby process
will be created with an implementation other than an `iruby:console'
implementation.

By default, this function operates on the list of `iruby:console-test'
objects registered in the variable `iruby-console-tests'. Each of these
`iruby:console-test' objects would typically be implemented with a set
of effective filesystem match clauses, e.g the existence of a file named
Gemfile, as well as an `iruby:console' class to initialize under
instance of a filesystem match.

Each `iruby:console' class may be implemented with behaviors specific to
that console class and subclasses of that console class, e.g procedures
to run before initializing a new iRuby buffer for that console class.

Typically, the `iruby' Emacs Lisp command would represent the top-level
interactive entry to this structural subset of the iRuby API

This API was inspired by the console implementation in inf-ruby"
  (cl-labels ((stop-dir-p (dir)
                ;; NB this itself does not check if dir is "/"
                (string-match-p locate-dominating-stop-dir-regexp dir))
              (match-dir-p (dir)
                (cl-block matching
                  (dolist (test tests)
                    (let ((matched (iruby:console-match-p dir test)))
                      (when matched
                        (cl-return-from matching
                          (wrap-console test dir)))))))
              (wrap-console (matched-test dir)
                (let ((retv
                       (if match-initialize
                           (funcall match-initialize matched-test dir)
                         (cons matched-test dir))))
                  (throw 'matched retv)))
              (check-dir (dir)
                ;; when the parent directory is the same as dir, dir is
                ;; typically a root filesystem directory
                (or (match-dir-p dir)
                    (let ((next (unless (stop-dir-p dir)
                                  (file-name-directory (directory-file-name dir)))))
                      (unless (or (null next) (equal next dir)
                                  ;; ensure nexr dir is readable,
                                  ;; e.g w/ Termux on Android where
                                  ;; /data/data/ is not readable
                                  (not (file-readable-p next)))
                        (check-dir next))))))
    (catch 'matched
      (check-dir (expand-file-name start)))))


(defun iruby-console-initialize-matched (matched-test dir)
  "Create an `iruby:console' object for some project directory.

This function may be used to provide the MATCH-INITIALIZE function for
`iruby-console-create'. In this usage, the MATCHED-TEST should be an
object of type `iruby:console-test', while DIR would be a string
representing a project directory"
 (let* ((class (iruby:ensure-class
                (iruby:console-test-console-class matched-test)))
        (base (iruby:default-interactor-for matched-test))
        ;; (extra-initargs (iruby:console-class-initargs class)) ;; TBD ...
        (instance (make-instance (eieio--class-name class)
                                 :base-impl base
                                 :initial-dir dir)))
   (iruby:initialize-instance-from instance base)
   ;; set an impl name for the console
   (setf (iruby:impl-name instance)
         (format "%s(%s %s)" (iruby:console-kind instance)
                 (file-name-nondirectory (directory-file-name dir))
                 (iruby:impl-name base)))
   instance))


;;
;; default implementations - iruby-console
;;

;; NB the lowest-precedence test should be listed first here


(define-iruby-console gem (iruby:gem-console)
  ;; This defines the EIEIO classes `iruby:gem-console-provider'
  ;; and `iruby:gem-console-test'.
  ;;
  ;; The the console-provider class `iruby:gem-console-provider'
  ;; will have the specified superclasses, i.e only `iruby:gem-console'
  ;; and will be recorded as the console class to be used on a
  ;; successful  test with an instance of the `iruby:gem-console-test'
  ;; class.
  ;;
  ;; Also within this macroexpansion: An instance of the console-test
  ;; class will be initialized using a constructor function of the same
  ;; name as the console-test class.  That instance will then be
  ;; registered with `iruby-register-console-test' as having a kind
  ;; being the symbol 'gem', a set of test forms as provided in the next
  ;; element in this macro form, and a console-class being the
  ;; class-name for the console-provider class i.e
  ;; `iruby:gem-console-provider' in this macroexpansion.
  ;;
  ;; This should serve to ensure that the console-test instance will
  ;; then be available in calls to `iruby-console-create' when called
  ;; with a default TESTS parameter. In applications, then for the first
  ;; project directory found that matches the test  pattern in a call to
  ;; `iruby-console-create' with default TESTS and MATCH-INITIALIZE
  ;; parameters, then an instance of the console-provider class defined
  ;; here will be initialized and returned by the call to
  ;; `iruby-console-create'.
  ;;
  ;; This console-search mechanism is used e.g in the `iruby'
  ;; interactive call, to select a project directory and console class
  ;; to use under some interactive  calls to `iruby'. That would be
  ;; mainly when the user has not specified an iRuby command, then when
  ;; a  project directory can be located, strating at `default-directory'
  ;; when `iruby' is called.
  ;;
  ;; syntax for the test used here: one filename glob, no file parsing
  "*.gemspec"
  ())

(define-iruby-console gemfile (iruby:gemfile-console)
  ;; syntax: one filename glob, no file parsing
  "Gemfile"
  ())

(cl-defmethod iruby:process-pre-init ((impl iruby:gemfile-console-provider))
  ;; this asumes default-directory has been set elsewhere in the call
  ;; stack, as to match the project directory for this console instance
  (let* ((gemfile-attrs (file-attributes "Gemfile"))
         (lock-attrs (when gemfile-attrs
                       (file-attributes "Gemfile.lock"))))
    (when gemfile-attrs
      (let ((gemfile-mtime (file-attribute-modification-time gemfile-attrs))
            (lock-mtime (when lock-attrs
                          (file-attribute-modification-time lock-attrs)))
            (console-name (iruby:impl-name impl))
            install-p)
        (cond
          ((null lock-mtime)
           (setq install-p
                 (unless noninteractive
                   (yes-or-no-p
                    (format "Bundle not installed for %s. Install now?"
                            console-name)))))
          ((and lock-mtime
                (string-greaterp (format-time-string "%s" gemfile-mtime)
                                 (format-time-string "%s" lock-mtime)))
           (setq install-p
                 (unless noninteractive
                   (yes-or-no-p
                    (format "Gemfile lock outdated for %s. Reinstall now?"
                            console-name)))))
          ((null gemfile-attrs) ;; debug case
           (warn "Found no Gemfile for console %s" console-name)))
        (when install-p
          (let* ((buffer (iruby-run-tool impl '("bundle" "install")))
                 (proc (get-buffer-process buffer)))

            (while (eq (process-status proc) 'run)
              (sleep-for iruby-output-wait))

            (unless (zerop (process-exit-status proc))
              ;; prevent the iruby impl from being initialized
              (error "Process exited with error in %s" buffer)
              )
            ))))))

(cl-defgeneric iruby-run-tool (impl cmd)
  "Return a buffer for a new process running CMD for the specified iRuby IMPL

If IMPL represents a project console, it can usally be assumed that the
current `default-directory' represents the project directory for that
console

Methods on this generic function should return the buffer for the newly
created process."
  (:method ((impl t) (cmd string))
    (iruby-run-tool impl (iruby:split-shell-string cmd)))
  (:method ((impl iruby:impl) (cmd cons))
    (let* ((impl-name (iruby:impl-name impl))
           (cmd-name (file-name-nondirectory (car cmd)))
           (proc-name (format "%s @ %s" cmd-name impl-name)))
      (with-iruby-process-environment (impl)
        (let ((buff (generate-new-buffer (format "*%s*" proc-name))))
          ;; FIXME comint handling for this buffer is simply breaking
          ;; mainly in comint-output-filer (Emacs 29 ...)
          (set-buffer buff)
          (comint-mode)
          (insert "pwd: ") ;; FIXME fontify the "pwd" and pathname parts
          (insert default-directory)
          (newline)
          (insert "$ ")
          (insert (mapconcat 'identity cmd " "))
          (newline)
          (setq buff (comint-exec buff proc-name (car cmd) nil (cdr cmd)))
          ;; NB always return a buffer from this generic function
          (pop-to-buffer buff))))))


(define-iruby-console rails (iruby:rails-console)
  ;; syntax of the following test expression is documented for
  ;; `iruby:console-test-test'
  (("Gemfile.lock" "^[[:space:]]*railties\\>")
   ;; TBD what tool uses a 'config' path in rails (??)
   ("config/application.rb" "\\_<Rails::Application\\_>"))
  ;; FIXME compute "-e" <rails_env> ...
  ;; and similar for the hanmai console type
  ((class-prefix-cmd
    :initform '("rails" "console"))))


(define-iruby-console hanmai (iruby:gemfile-console)
  ;; FIXME -env
  ;;
  ;; test syntax here: list of (literal-filename &rest content-parse-re)
  ;;
  ;; NB the literal-filename may provide a literal filename under some
  ;; subdirectory, or a directory name
  ;;
  ;; TBD what tool uses a 'config.ru' path in hanmai (??)
  (("config.ru" "\\_<run Hanami.app\\_>"))
  ())

(define-iruby-console racksh (iruby:gemfile-console)
  (("Gemfile.lock" "^[[:space:]]*racksh\\>")))

(define-iruby-console zeus (iruby:gemfile-console)
  ;; test syntax here: one filename regexp (no Gemfile deps parse)
  ".zeus.sock"
  ((class-prefix-cmd
    :initform '("zeus" "console"))))


(provide 'iruby-console)
