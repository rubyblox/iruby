;;; iruby-console.el --- project support for iruby

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

(require 'iruby-util)
(require 'iruby)


;;
;; utility forms
;;

(eval-when-compile

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

) ;; eval-when-compile


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
  (let* ((dir-attrs (or (file-attributes dir 'integer)
                        (error "Directory not found: %s" dir)))
         (dir-device (file-attribute-device-number dir-attrs))
         (dir-ino (file-attribute-inode-number dir-attrs))
         (name
          (typecase impl
            (iruby:impl (iruby:impl-name impl))
            (t impl))))
    (catch 'search
      (cl-dolist (elt iruby-process-buffers)
        (let ((buff (cdr elt)))
          (when (buffer-live-p buff)
            (with-current-buffer buff
              (and (if name (equal name (iruby:impl-name iruby-buffer-interactor))
                     t)
                   (iruby:console-p iruby-buffer-interactor) ;; in BUFF
                   (let ((dir (iruby:impl-initial-dir iruby-buffer-interactor)))
                     (when (and (file-exists-p dir) (file-readable-p dir))
                       (let* ((o-attrs (file-attributes dir 'integer))
                              (o-device (when o-attrs
                                          (file-attribute-device-number o-attrs)))
                              (o-ino (when o-attrs
                                       (file-attribute-inode-number o-attrs))))
                         (and o-attrs
                              (= o-device dir-device)
                              (= o-ino dir-ino)))
                       (throw 'search buff)))))))))))


;;
;; base classes - iruby-console in EIEIO
;;
;; after the console API in inf-ruby
;;

(defclass iruby-wrapper-binding (iruby:interactive-binding)
  ((wrapper-base-impl
    :accessor iruby:wrapper-base-impl
    :initarg :wrapper-base-impl
    :type iruby:impl)))

(cl-defgeneric iruby:initialize-instance-from (inst other)
  ;; FIXME remove, no longer used
  (:method ((inst iruby-wrapper-binding) (other iruby:interactive-binding))
    (let ((inst-sl (iruby:class-slots (class-of inst)))
          (other-sl (iruby:class-slots (class-of other))))
      (dolist (common-sl (cl-intersection
                             (iruby:class-slots (find-class 'iruby-wrapper-binding))
                             (iruby:class-slots (find-class 'iruby:interactive-binding))
                             :test #'eq)
               inst)
        (setf (eieio-oref inst common-sl) (eieio-oref other common-sl))))))


(cl-defgeneric iruby:console-kind (datum)
  (:documentation
   "Return the symbolic name for an `iruby:console-test'"))


(cl-defgeneric iruby:console-wrapper-prefix-cmd (console)
  (:documentation
   "Return a list of direct wrapper arguments for the console.

See also: `iruby:console-prefix-cmd'"))


(defclass iruby:console-mixin ()
  ((kind
    :initarg :kind
    :accessor iruby:console-kind
    :type symbol)
    (prefix-cmd
    :type list
    :initforml nil
    ;; TBD hopefully the class allocation is inherited by subclases in EIEIO
    :allocation :class
    :accessor iruby:console-wrapper-prefix-cmd
    :documentation
    "Wrapper command and prefix arguments for console instances of this type")
   (append-args
    :type list
    :initform nil
    :allocation :class
    :accessor iruby:console-wrapper-append-args
    :documentation
    "Wrapper suffix arguments for console instances of this type")
   ) ;; slots
  :abstract t)

(cl-defgeneric iruby:console-prefix-cmd (console)
  "Compute a shell command previx for a wrapper console's shell command

The effective method should return a cons of a shell command name and
zero or more shell command arguments, with each element as a string.

This command list will be prefixed to any shell command list for running
an interactive binding for the console.

The command list returned from this function should be of a syntax for
running the console implementation under comint"
  (:method ((console iruby:console-mixin))
    (let ((cmd (iruby:console-wrapper-prefix-cmd console)))
      (append cmd (when (cl-next-method-p)
                    (cl-call-next-method))))))


(cl-defgeneric iruby:console-append-args (console)
  "Compute the list of trailing arguments of the shell command for CONSOLE"
  (:method ((console iruby:console-mixin))
    (let ((class-args (iruby:console-wrapper-append-args console)))
      (append (when (cl-next-method-p)
                (cl-call-next-method))
              class-args))))

(defclass iruby:console (iruby:console-mixin iruby-wrapper-binding)
  () ;; slots
  :abstract t)

(cl-defmethod iruby:parse-cmd ((datum iruby:console))
  (append (iruby:console-prefix-cmd datum)
          (iruby:parse-cmd (iruby:wrapper-base-impl datum))
          (iruby:console-append-args datum)))

(defclass iruby:gemfile-console (iruby:console)
  ((prefix-cmd
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
  ((append-args
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
  ;; FIXME expand the docs here
 "Sequence of iruby:console-test objects")

;;; reset the value:
;; (setq iruby-console-tests nil)

(defun iruby-register-console-test (instance)
  ;; FIXME this does not accept any precedence args e.g :before / :after
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
    (let ((console-class (intern (format "iruby:console-%s" kind)))
          (test-class (intern (format "iruby:console-test-%s" kind))))
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
           ,(typecase tests
              (string `(car (file-expand-wildcards ,tests)))
              (cons
               `(and ,@(mapcar #'make-glob-re-matcher tests)))
              (t (error "Unrecognized test syntax in %S definition: %S"
                        kind tests)))))

       (iruby-register-console-test
        ;; EIEIO typically defines a constructor function of the same
        ;; name as a class
        ;;
        (,test-class :kind (quote ,kind)
                     :test (quote ,tests)
                     :console-class (quote ,console-class)))

       (find-class (quote ,console-class) t)
       ))))


(cl-defgeneric iruby:default-interactor-for (datum)
  (:method (datum)
    (iruby:get-default-interactive-binding datum))
  (:method ((datum iruby:console-test))
    ;; used in iruby-console-initialize-matched
    (iruby:get-default-interactive-binding iruby-default-interactive-binding)))

(cl-defun iruby-console-create (&key
                                (start default-directory)
                                (tests iruby-console-tests)
                                (match-initialize
                                 'iruby-console-initialize-matched))
  ;; NB if called with default initargs and some project directory can
  ;; be found at or containing the START dir, then this function will
  ;; always create and return a newly initialized interactor object.
  ;;
  ;; To locate any initialized interactor for a dir, see also:
  ;; `iruby-find-console-buffer'
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
                ;; when the parent directory is the same as dir, it's at
                ;; the root directory of the filesystem
                (or (match-dir-p dir)
                    (let ((next (unless (stop-dir-p dir)
                                  (file-name-directory (directory-file-name dir)))))
                      ;; for dir "/" next => "/" (*nix pathnames)
                      (unless (or (null next) (equal next dir)
                                  ;; ensure nexr dir is readable,
                                  ;; e.g w/ termux on Android where
                                  ;; /data/data/ is not readable
                                  (not (file-readable-p next)))
                        (check-dir next))))))
    (catch 'matched
      (check-dir (expand-file-name start)))))


(defun iruby-console-initialize-matched (matched-test dir)
  ;; NB DIR may be used in later calls with console-match predicate functions
 (let* ((class (iruby:ensure-class
                (iruby:console-test-console-class matched-test)))
        (base (iruby:default-interactor-for matched-test))
        ;; (extra-initargs (iruby:console-class-initargs class)) ;; TBD ...
        (instance (make-instance (eieio--class-name class)
                                 :wrapper-base-impl base
                                 :initial-dir dir)))
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
  ;; defines `iruby:console-gem' and `iruby:console-test-gem'
  ;;
  ;; syntax: one filename glob, no file parsing
  "*.gemspec"
  ())

(define-iruby-console gemfile (iruby:gemfile-console)
  ;; syntax: one filename glob, no file parsing
  "Gemfile"
  ())


(define-iruby-console rails (iruby:rails-console)
  ;; syntax of the following test expression is documented for
  ;; `iruby:console-test-test'
  (("Gemfile.lock" "^[[:space:]]*railties\\>")
   ;; TBD what tool uses a 'config' path in rails (??)
   ("config/application.rb" "\\_<Rails::Application\\_>"))
  ;; FIXME compute "-e" <rails_env> ...
  ;; and similar for the hanmai console type
  ((prefix-cmd
    :initform ("rails" "console"))))


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
  ((prefix-cmd
    :initform ("zeus" "console"))))


(provide 'iruby-console)
