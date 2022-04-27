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
(require 'iruby-console-util)
(require 'iruby)

;;;###autoload
(defun iruby-file-contents-match (file regexp &optional match-group)
  (with-temp-buffer
    (insert-file-contents file)
    (when (re-search-forward regexp nil t)
      (if match-group
          (match-string match-group)
        t))))

(defun iruby-console-wrapper (cmd name)
  (let ((binding (iruby-get-default-interactive-binding)))
    (iruby-wrap-binding cmd binding name)))

(defun iruby-console-run (cmd name &optional new)
  (when (stringp cmd)
    (setq cmd (iruby-split-shell-string cmd)))
  (let* ((exists (unless new
                   (iruby-find-console-buffer)))
         (exists-cmd (when exists
                       (with-current-buffer exists
                         iruby-buffer-command)))
         (exists-same (equal cmd exists-cmd))
         (wrapper (unless (and exists (not exists-same))
                    (iruby-console-wrapper cmd name))))
    (unless (and exists exists-same))
      (run-iruby-new wrapper name)))

;;;###autoload
(cl-defun iruby-console-auto (&optional (dir default-directory)
                                new)
  "Run the appropriate Ruby console command, or the default iRuby

The command and the directory to run it from are detected
automatically.

If an iRuby buffer exists for DIR, that buffer will be selected, else a
new iRuby process will be created.

If no matching project files can be found, this will run the default
iRuby implementation, with `default-directory' set to DIR.

If called interactively, the selected buffer's `default-directory' will
be used as DIR unless called with a prefix argument. If called with a
prefix argument, the user will be asked to select a directory for the
console"
  (interactive
   (list (iruby-read-directory "Run iRuby console in directory: ")
         current-prefix-arg))
  (let* ((match (iruby-console-walk-dirs dir))
         (type (car match))
         (fun (when type (intern (format "iruby-console-%s" type)))))
    (cond
      ((null match)
       (warn "iruby-console-auto: No console available for %s" dir)
       (iruby))
      ((fboundp fun)
       (setq default-directory (cdr match))
       (funcall fun dir new))
      (t
       (error "Console function not bound: %S" fun)))))


;;;###autoload
(defun iruby-console-zeus (dir &optional new)
  "Run Rails console in DIR using Zeus."
  (interactive (list (iruby-console-read-directory 'zeus)
                     current-prefix-arg))
  (let* ((default-directory (file-name-as-directory dir))
         (wrap-cmd  '("zeus" "console"))
         (binding (iruby-get-default-interactive-binding)))
    (unless (executable-find "zeus")
      (setq wrap-cmd (append '("bundle" "exec") wrap-cmd)))
    (iruby-console-run wrap-cmd "zeus" new)))

(defun iruby-console-rails-p ()
  (and (file-exists-p "Gemfile.lock")
       (iruby-file-contents-match "Gemfile.lock" "^ +railties ")
       (file-exists-p "config/application.rb")
       (iruby-file-contents-match "config/application.rb"
                                  "\\_<Rails::Application\\_>")))
;;;###autoload
(defun iruby-console-rails (dir &optional new)
  "Run Rails console in DIR."
  (interactive (list (iruby-console-read-directory 'rails)
                     interactive-prefix-arg))
  (let* ((default-directory dir)
         (env (iruby-console-rails-env))
         (with-bundler (file-exists-p "Gemfile"))
         (wrap-cmd '("rails" "console" "-e")) ;; TBD "-e" with no arg (??)
         (binding  (iruby-get-default-interactive-binding)))
    (when with-bundler
      (setq wrap-cmd (append '("bundle" "exec") wrap-cmd)))
    (iruby-console-run wrap-cmd "rails" new)))

(defun iruby-console-rails-env ()
  (if (stringp iruby-console-environment) ;; FIXME awkward API
      iruby-console-environment
    (let ((envs (iruby-console-rails-envs)))
      (completing-read "Rails environment: "
                       envs
                       nil t
                       nil nil (car (member "development" envs))))))

(defun iruby-console-rails-envs () ;; FIXME awkward API naming
  (let ((files (iruby-expand-files "config/environments/*.rb")))
    (if (null files)
        (error "No files in %s" (expand-file-name "config/environments/"))
      (mapcar #'file-name-base files))))

(defun iruby-console-hanami-p ()
  (and (file-exists-p "config.ru")
       (iruby-file-contents-match "config.ru" "\\_<run Hanami.app\\_>")))

(defun iruby-console-hanami (dir &optional new)
  "Run Hanami console in DIR."
  (interactive (list (iruby-console-read-directory 'hanami)
                     current-prefix-arg))
  (let* ((default-directory (file-name-as-directory dir))
         (env (iruby-console-hanami-env))
         (with-bundler (file-exists-p "Gemfile"))
         (wrap-cmd '("hanami" "console")))
    (when with-bundler
      (setq wrap-cmd (append '("bundle" "exec") wrap-cmd)))
    (with-iruby-process-environment ((format "HANAMI_ENV=%s" env))
      (iruby-console-run wrap-cmd "hanmai" new))))

(defun iruby-console-hanami-env ()
  (if (stringp iruby-console-environment)
      iruby-console-environment
    (let ((envs '("development" "test" "production")))
      (completing-read "Hanami environment: "
                       envs
                       nil t
                       nil nil (car (member "development" envs))))))

;;;###autoload
(defun iruby-console-gem (dir &optional new)
  "Run IRB console for the gem in DIR.
The main module should be loaded automatically.  If DIR contains a
Gemfile, it should use the `gemspec' instruction."
  (interactive (list (iruby-console-read-directory 'gem)
                     current-prefix-arg))
  (let* ((default-directory (file-name-as-directory dir))
         ;; NB picking the first gemspec file here, if mutiple are available
         (gemspec (car (iruby-expand-files "*.gemspec")))
         (name  (iruby-file-contents-match
                 gemspec "\\.name[ \t]*=[ \t]*['\"]\\([^'\"]+\\)['\"]" 1))
         (args (when (file-directory-p "lib")
                 (list "-I" "lib")))
         (bind (iruby-get-default-interactive-binding))
         wrapper)
    (setq name
          (format "gem(%s)"
                  (cond
                    (name (string-trim name))
                    (t (file-name-sans-extension gemspec))))
          wrapper (iruby-wrap-binding nil bind name))
    (when args
      (setf (iruby:interactive-args wrapper)
            (append (iruby:interactive-args wrapper)
                    args)))
    ;; calling this directly here, due to the args append
    (run-iruby-new wrapper name)))

(defun iruby-console-racksh-p ()
  (and (file-exists-p "Gemfile.lock")
       (iruby-file-contents-match "Gemfile.lock" "^ +racksh")))

(defun iruby-console-racksh (dir &optional new)
  "Run racksh in DIR."
  (interactive (list (iruby-console-read-directory 'racksh)
                     current-prefix-arg))
  (let ((default-directory (file-name-as-directory dir)))
    (iruby-console-run '("bundle" "exec" "racksh") "racksh" new)))

(defun iruby-console-script-p ()
  (and (file-exists-p "Gemfile.lock")
       (or
        (file-exists-p "bin/console")
        (file-exists-p "console")
        (file-exists-p "console.rb"))))

;;;###autoload
(defun iruby-console-script (dir &optional new)
  "Run custom bin/console, console or console.rb in DIR."
  ;; FIXME the console cmd may need additional configuration for use
  ;; under iruby
  (interactive (list (iruby-console-read-directory 'script)
                     current-prefix-arg))
  (let ((default-directory (file-name-as-directory dir))
        (wrap-cmd '("bundle" "exec"))
        name)
    (cond
     ((file-exists-p "bin/console")
      (setq wrap-cmd (append wrap-cmd (setq name "bin/console"))))
     ((file-exists-p "console.rb")
      (setq wrap-cmd (append wrap-cmd (list "ruby" (setq name "console.rb")))))
     ((file-exists-p "console")
      (setq wrap-cmd (append wrap-cmd (list (setq name "console"))))))
    (cond
      (name (iruby-console-run wrap-cmd name new))
      (t (error "Found no console script in %S" dir)))))

;;;###autoload
(defun iruby-console-default (dir &optional new)
  "Run Pry or the default iRuby as a bundler console in DIR"
  (interactive (list (iruby-console-read-directory 'default)
                     current-prefix-arg))
  (let ((default-directory dir))
    (cond
      ((file-exists-p "Gemfile")
       (let* ((project (file-name-nondirectory (directory-file-name dir)))
              (name (format "console (%s)" project)))
         (iruby-console-run '("bundle" "exec") name new)))
      (t
       (error "Unable to run a bundler console in a directory with no Gemfile")))))


;;
;; base classes - iruby-console
;;

;;
;; NB although Emacs Lisp may not in itself implement any concept of
;; package namespaces, this API uses a naming convention similar that
;; for exported symbols in Common Lisp packages, e.g <package>:<name>
;;
;; This may serve to differentiate any functions that may be derived
;; from the API classes for iRuby, in EIEIO - e.g #'iruby:console -
;; juxtaposed to normal Emacs Lisp commands and general API functions,
;; in the iRuby implementation for Emacs Lisp.
;;
;; FIXME the iruby-impl API should be updated for a similar class/method
;; naming convention for iRuby in EIEIO

(cl-defgeneric iruby:console-wrapper-class-args (console)
  (:documentation
   "Return a list of direct wrapper arguments for the console.

See also: `iruby:console-wrapper-args'"))


(defclass iruby:console (iruby-wrapper-binding)
  ((wrap-args
    :type list
    ;; TBD hopefully the class allocation is inherited by subclases in EIEIO
    :allocation :class
    :accessor iruby:console-wrapper-class-args
    :documentation "Direct wrapper arguments for the console type")
   ) ;; slots
  :abstract t)


(defclass iruby:gemfile-console (iruby:console)
  ((wrap-args
    :initform (list "bundle" "exec")))
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
  ((wrap-args
    :initform  '("-I" "lib")))
  :abstract t)


(defclass iruby:script-console (iruby:console)
  ()
  :abstract t)


(cl-defgeneric iruby:console-wrapper-args (console)
  "Generic function for computing a shell command for iruby:console implementations

The effective method should return a cons of a shell command name and
zero or more shell command arguments, with each element as a string.
This command list should be of a syntax for running the console
implementation under comint"
  (:method ((console iruby:console))
    "Default method, returning an empty list"
    (let ((args (iruby:console-wrapper-class-args console)))
      (when (cl-next-method-p)
        (append args (cl-call-next-method)))))
  ;; TBD does EIEIO really implement :around methods ...
  ;; NB Of course not ! So that fairly breaks things here
  )

;; test
;;   (iruby:console-wrapper-args (iruby:console-gem))
;; => FIXME should not be nil

(cl-defgeneric iruby:console-test-tag (datum)
  (:documentation
   "Return a string name for an `iruby:console-test'"))

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

(defclass iruby:console-test ()
  ((tag
    :initarg :tag
    :accessor iruby:console-test-tag
    :type string)
   (console-class
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
  (let ((tag (iruby:console-test-tag instance))
        exists)
    (cl-dotimes (n (length iruby-console-tests))
      (when (equal tag (iruby:console-test-tag  (nth n iruby-console-tests)))
        (setq exists n)
        (cl-return)))
    (cond
      (exists (setf (nth exists iruby-console-tests)
                    instance)
              exists)
      (t (setq iruby-console-tests (cons instance iruby-console-tests))))))

(cl-defgeneric iruby:console-match-p (dir test))

(cl-defmacro define-iruby-console (type (&rest superclasses) tests
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
                           (cond
                             ((null re-tests)
                              (setq first-match globbed))
                             (first-match
                              (cl-return-from first-matched first-match))
                             (t
                              (dolist (test re-tests)
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
                                  (t (setq first-match nil)))))))))))))
    (let ((console-class (intern (format "iruby:console-%s" type)))
          (test-class (intern (format "iruby:console-test-%s" type))))
    `(progn
       ;; top-level utility forms
       ;; 1) define the console class
       ;; 2) define the console test class
       ;; 3) define a console test method for the console test class
       ;; 4) create and register an instance of the console test class

       (defclass ,console-class (,@superclasses)
         ;; FIXME cannot just pass-through the :wrap-args list
         ((type :initform (quote ,type)))
         ,@(mapcan (lambda (args)
                     (cl-destructuring-bind (s . literals) args
                       (list s (cons 'quote literals))))
                   initargs))

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
                        type tests)))))

       (iruby-register-console-test
        ;; EIEIO typically defines a constructor function of the same
        ;; name as a class
        ;;
        (,test-class :tag ,(prin1-to-string type)
                     :test (quote ,tests)
                     :console-class (quote ,console-class))
       )))))


;;
;; default implementations - iruby-console
;;

;; NB the lowest-precedence test should be listed first here


(define-iruby-console gem (iruby:gem-console)
  "*.gemspec" ;; one glob, no Gemfile deps parse
  )

(define-iruby-console gemfile (iruby:gemfile-console)
  "Gemfile" ;; one filename, no Gemfile deps parse
  )

(define-iruby-console rails (iruby:rails-console)
  ;; syntax of the following test expression is documented for
  ;; `iruby:console-test-test'
  (("Gemfile.lock" "^[[:space::]]*railties\\>")
   ;; TBD what tool uses a 'config' path in rails (??)
   ("config/application.rb" "\\_<Rails::Application\\_>"))
  ;; FIXME compute "-e" <env> ...
  ;; and similar fo rthe hanmai console type
  (:wrap-args "rails" "console"))


(define-iruby-console hanmai (iruby:gemfile-console)
  ;; FIXME -env
  ;;
  ;; test syntax here: list of (literal-filename &rest content-parse-re)
  ;;
  ;; NB the literal-filename may provide a literal filename under some
  ;; subdirectory, or a directory name
  ;;
  ;; TBD what tool uses a 'config.ru' path in hanmai (??)
  (("config.ru" "\\_<run Hanami.app\\_>")))


(define-iruby-console racksh (iruby:gemfile-console)
  (("Gemfile.lock" "^[[:space::]]*racksh\\>")))

(define-iruby-console zeus (iruby:gemfile-console)
  ;; test syntax here: one filename regexp (no Gemfile deps parse)
  ".zeus.sock"
  (:wrap-args "zeus" "console")
  )


(provide 'iruby-console)
