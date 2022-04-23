;;; iruby.el --- Run a Ruby process in a buffer (a fork on inf-ruby)

;; Copyright (C) 1999-2008 Yukihiro Matsumoto, Nobuyoshi Nakada

;; Author: Yukihiro Matsumoto
;;         Nobuyoshi Nakada
;;         Cornelius Mika <cornelius.mika@gmail.com>
;;         Dmitry Gutov <dgutov@yandex.ru>
;;         Kyle Hargraves <pd@krh.me>
;;         Sean Champ <spchamp@users.noreply.github.com>
;; Maintainer: Sean Champ <spchamp@users.noreply.github.com>
;; URL: https://github.com/rubyblox/iruby
;; Created: 8 April 1998
;; Keywords: languages ruby
;; Version: 3.0.3

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This Emacs Lisp library was originally developed as inf-ruby.el
;;
;; To prevent incompatibility with Emacs Lisp software that may expect
;; an interface for inf-ruby.el, after a fork, the library was renamed
;; to iruby.el and top-level symbols were renamed correspondingly.
;;
;;  ruby-<suffix> => iruby-<suffix>
;;  inf-ruby-<suffix> => iruby-<suffix>
;;  run-ruby<suffix> => run-iruby<suffix>
;;
;; The iruby.el version string will continue from the original
;; inf-ruby.el version at the time of the fork, i.e inf-ruby.el 2.5.2
;;
;; ** Summary **
;;
;; iruby provides a REPL buffer connected to a Ruby subprocess.
;;
;; ** Usage - Interactive evaluation with irb **
;;
;; The commands `iruby' and `run-iruby' will launch an interacive irb
;; process buffer or display any existing irb process buffer.
;;
;; The `iruby' command, if called with an interactive prefix argument,
;; will prompt the user to select an existing implementation under
;;`iruby-implementations'.
;;
;; With an interactive prefix argument, the `run-iruby' command will
;; prompt the user to enter a shell command string for launching an irb
;; process.
;;
;; If called interactively with no prefix argument, either command will
;; use `iruby-default-implementation'.
;;
;; ** Usage - Key Bindings, Menus, and Hooks ***
;;
;; * `iruby-mode-map' will inherit key bindings from `comint-mode-map'
;;   when iruby.el is evaluated. This keymap provides bindings for the
;;   interative irb process buffer
;;
;; * `iruby-minor-mode-map' provides a keyamp for `iruby-minor-mode'
;;
;; * `iruby-minor-mode-menu' provides an interactive iruby menu
;;
;; * `iruby-mode-hook' and `comint-mode-hook' will be used for the
;;   interactive irb process buffer
;;
;; The following documentation illustrates how to use iruby with hooks
;; provided in other minor modes
;;
;; ** Usage - Additional Features **
;;
;; (FIXME needs documentation)
;;
;; ** Installation **
;;
;; If you're installing manually, you'll need to:
;; * drop the file somewhere on your load path (perhaps ~/.emacs.d)
;; * Add the following lines to your .emacs file:
;;
;;    (autoload 'iruby "iruby" "Run an inferior Ruby process" t)
;;    (add-hook 'ruby-mode-hook 'iruby-minor-mode)
;;
;; Or, for enh-ruby-mode:
;;
;;    (add-hook 'enh-ruby-mode-hook 'iruby-minor-mode)
;;
;; Installation via ELPA interface does the above for you
;; automatically.
;;
;; Additionally, consider adding
;;
;;    (add-hook 'compilation-filter-hook 'iruby-auto-enter)
;;
;; to your init file to automatically switch from common Ruby compilation
;; modes to interact with a debugger.
;;
;; To call `iruby-console-auto' more easily, you can, for example,
;; replace the original `iruby' binding:
;;
;;   (eval-after-load 'iruby
;;     '(define-key iruby-minor-mode-map
;;        (kbd "C-c C-s") 'iruby-console-auto))

;;; Code:

(require 'comint)
(require 'compile)
(require 'ruby-mode)
(require 'thingatpt)

(eval-when-compile
  (require 'cl)
  (defvar rspec-compilation-mode-map)
  (defvar ruby-compilation-mode-map)
  (defvar projectile-rails-server-mode-map))


(defgroup iruby nil
  "Run Ruby process in a buffer"
  :group 'languages)

(cl-defmacro with-iruby-widget-validate ((var &optional
                                              (message "Invalid value: %S"))
                                         &body test-forms)
  (let ((widget (make-symbol "%widget")))
    `(lambda (,widget)
       (let ((,var (widget-value ,widget)))
         (unless (progn ,@test-forms)
           (widget-put ,widget
                       :error (format ,message ,var))
           ,widget)))))

(defcustom iruby-load-file-history-limit history-length
  "File name history liimit for `iruby-load-file'.

If zero, no file name history will be stored under `iruby-load-file'.

The default initial value is derived from `history-length'"
  :type `(integer
          :validate
          ,(with-iruby-widget-validate (len "Unable to parse history limit: %S")
             (and (integerp len) (or (zerop len) (plusp len)))))
  ;; TBD :group 'iruby-files
  :group 'iruby)

(defcustom iruby-use-project-directories t
  "configuration for `iruby-get-prevailing-buffer'

This variable determines whether to check for buffers specific to
individual project directories under `iruby-get-prevailing-buffer'."
  :type 'boolean
  :group 'iruby)

(defgroup iruby-ui nil
  "iRuby user interface support"
  :group 'iruby)


(defcustom iruby-app-name "iRuby"
  "Label for iruby-minor-mode in various user interface elements.

This value will be used as a sufix for `iruby-minor-mode' in the
modeline, there prefixed with the value of `iruby-minor-mode-prefix.'

This value is also used for menu entries in `iruby-minor-mode', such
that will use the value of this variable when iruby.el is evaluated.
When this custom value is updated, the updated value may not be
reflected in those menu entries until Emacs is restarted"
  :group 'iruby-ui
  :type 'string)

(defcustom iruby-minor-mode-prefix " "
  "Prefix delimiter to use for `iruby-app-name' in `iruby-minor-mode'

This value will be used under an `iruby-minor-modeline-format' that
would call `iruby-minor-modeline-default-label' in the format's
evaluation block.

This value should generally contain at least a whitespace character or
printable delimiter"
  :group 'iruby-ui
  :type 'string)

(defun iruby-minor-modeline-default-label (&optional buffer)
  "Return a modeline string for `iruby-minor-mode'

This function is used in the default value for the customization option,
`iruby-minor-modeline-format'

See also: `iruby-minor-mode-prefix', `iruby-app-name'"
  (when buffer (set-buffer buffer))
  (let ((impl (cond
                (iruby-buffer (iruby-process-impl iruby-buffer))
                ((eq major-mode 'iruby-mode)
                 (iruby-process-impl (current-buffer))))))
    (cond
      (impl (concat iruby-minor-mode-prefix iruby-app-name ":" impl))
      (t (concat iruby-minor-mode-prefix iruby-app-name)))))


;; ensure that any local :eval blocks will be parsed for the modeline
;; in `iruby-minor-modeline-format'
(put 'iruby-minor-modeline-format 'risky-local-variable t)

(defcustom iruby-minor-modeline-format
  '(:eval (iruby-minor-modeline-default-label))
  "Expression for producing a modeline label for `iruby-minor-mode'

As a literal expresion, this value's format must be compatible with
the variable `mode-line-format'. The value as interpreted under
`mode-line-format' will be used as the modeline indicator for
`iruby-minor-mode'"
  :group 'iruby-ui
  ;; Nearly a syntax for `mode-line-format', in a custom widget:
  :type '(choice
          (string :tag "Literal string" :value " iRuby")
          (symbol :tag "Variable name" :value iruby-minor-mode-prefix)
          (cons :tag "Format indicator"
           (choice :tag "Format head"
            (const :tag "Eval tag [:eval]"
                   :value :eval)
            (const :tag "Property eval tag [:propertize]"
                   :value :propertize)
            (symbol :tag "Conditional variable"
                    :value t
                    :match (lambda (wdgt s)
                             (not (or (eq s :eval)
                                      (eq s :propertize)))))
            (integer :tag "Bounding limit" :value 1))
           (choice
            :tag "Format rest"
            (list :tag "Emacs Lisp form"
                  :match (lambda (wdgt l) (null (cadr l)))
                  (sexp))
            (list
              :tag "Conditional form"
              :validate
              (lambda (widget)
                (let* ((toplevel (widget-value
                                  (widget-get (widget-get widget :parent)
                                              :parent)))
                       (first (and (consp toplevel) (car toplevel))))
                  (when  (or (not (typep first 'symbol))
                             (eq first :eval)
                             (eq first :propertize))
                    (widget-put widget :error
                                (format "conditional CAR must be a symbol not :eval or :propertize: %S"
                                        first))
                    widget)))
              (sexp :tag "Conditional first alternative expression"
                    ;; NB the initial :value form
                    ;; will not be evaluated under defcustom
                    :value (iruby-minor-modeline-default-label (current-buffer)))
              (sexp :tag "Conditional second alternative expression"
                    :value nil))))))


(defcustom iruby-show-last-output t
  "If non-nil, show results in the minibuffer after iruby-send commands"
  :type 'boolean
  :group 'iruby-ui)

(defcustom iruby-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :type 'boolean
  :group 'iruby-ui)

(defgroup iruby-language nil
  "Ruby language support for iRuby"
  :group 'iruby)

(defcustom iruby-ruby-modes
  '(("erm" enh-ruby-mode
     enh-ruby-mode-syntax-table enh-ruby-mode-abbrev-table nil)
    ("ruby-mode" ruby-mode
     ruby-mode-syntax-table ruby-mode-abbrev-table nil))
  "Mode definitions for Ruby language support in iRuby

This variable provides a table of information about major modes for Ruby
langauge support available to iRuby. The syntax for entries in this table
may be illustrated under the `customize-option' buffer for this
variable.

Each element should be of the following general syntax, as a list:
 (\"name\" feature s a &optional m)
where
 name: a string name for the mode, unique in this table
 feature: a symbol denoting a feature for the mode
 s: a symbol denoting a syntax table for the mode
 a: a symbol denoting an abbrev table for the mode
 m: nil or a symbol denoting the mode's major-mode function

The feature symbol should be usable under `require'. A call to require
that feature symbol should result in the definition of the syntax
table, abbrev table, and major-mode functions defined in this list.

If m is nil, then the feature symbol will be reused as the major-mode
function.

Indivdual elements in this table may be accessed with the following
functions:

`iruby-ruby-syntax-table'
`iruby-ruby-abbrev-table'
`iruby-ruby-mode-function'
`iruby-source-modes'
`iruby-find-syntax'

Among other features, this allows for reusing the syntax table and
abbrev table from any major mode defined here, within any individual
iRuby buffer.

See also: `edit-abbrevs'"
  :group 'iruby-language
  :type '(repeat (list :tag "Mode definition"
                  (string :tag "Mode name")
                  (symbol :tag "Feature symbol (require)")
                  (symbol :tag "Mode syntax table (variable)")
                  (symbol :tag "Mode abbrev table (variable)")
                  (choice :tag "Mode"
                   (const :tag "Use feature symbol" :value nil)
                   (symbol :tag "Mode function name"))
                  )))

(defcustom iruby-default-ruby-syntax "erm"
  "Ruby language mode for input completion support in Ruby buffers

This value must match a key value in the associative list,
`iruby-ruby-modes'"
  :group 'iruby-language
  :type `(choice ,@(mapcar (lambda (item) `(const ,(car item)))
                           iruby-ruby-modes)))

(make-variable-buffer-local
 (defvar iruby-ruby-syntax nil
   "If non-nil, the Ruby syntax for the current Ruby buffer.

See also: `iruby-default-ruby-syntax'"))

(cl-defun iruby-find-syntax (&optional
                               (name (or iruby-ruby-syntax
                                         iruby-default-ruby-syntax))
                               noerr)
  (let ((elt (cl-assoc name iruby-ruby-modes :test #'equal)))
    (or elt noerr
        (error "iRuby syntax not found: %S" name))))

(cl-defun iruby-ruby-syntax-table (&optional (syntax
                                              (or iruby-ruby-syntax
                                                  iruby-default-ruby-syntax)))
  (destructuring-bind (name feature stx-table abbrev-table &optional mode)
      (iruby-find-syntax syntax)
    (require feature)
    (symbol-value stx-table)))

;;; ad-hoc test
;; (type-of (iruby-ruby-syntax-table "erm"))
;; => char-table

(cl-defun iruby-ruby-abbrev-table (&optional (syntax
                                              (or iruby-ruby-syntax
                                                  iruby-default-ruby-syntax)))
  ;; see also: `edit-abbrevs'
  (destructuring-bind (name feature stx-table abbrev-table &optional mode)
      (iruby-find-syntax syntax)
    (require feature)
    (symbol-value abbrev-table)))


;;; ad-hoc test
;; (type-of (iruby-ruby-abbrev-table "ruby-mode"))
;; => vector

(cl-defun iruby-ruby-mode-function (&optional (syntax
                                               (or iruby-ruby-syntax
                                                   iruby-default-ruby-syntax)))
  ;; see also: `edit-abbrevs'
  (destructuring-bind (name feature stx-table abbrev-table &optional mode)
      (iruby-find-syntax syntax)
    (require feature)
    (or mode feature)))

;; (iruby-ruby-mode-function "erm")
;; => enh-ruby-mode
;; (iruby-ruby-mode-function "ruby-mode")
;; => ruby-mode

;;;###autoload
(defun iruby-source-modes ()
  "Used to determine if a buffer contains Ruby source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a ruby source file by `iruby-load-file'.
Used by these commands to determine defaults."
  ;; This was originally defined as a variable under inf-ruby,
  ;; i.e inf-ruby-source-modes.
  ;;
  ;; For supporting the customizable definition of ruby syntaxes in
  ;; iruby, the variable was redefined as a function
  (remove-if-not
   #'fboundp
   (mapcar #'(lambda (elt)
               (destructuring-bind (name feature stx-table abbrev-table &optional mode)
                   elt
                 (or mode feature)))
           iruby-ruby-modes)))

;; (iruby-source-modes)


(defgroup iruby-impl nil
  "Ruby implementation support for iRuby"
  :group 'iruby)


(defcustom iruby-ruby-irb-args '("-r" "irb" "-r" "irb/completion"
                                   "-e" "IRB.start" "--")
  "List of arguments for ruby, when running irb via ruby

Generally, this list should iniclude the string \"--\" as a trailing
element. Arguments after that string here would be provided as arguments
to irb, as under a direct call to ruby"
  :type '(repeat string)
  :group 'iruby)


(defcustom iruby-implementations
  ;; FIXME needs a custom widget type, this ostensibly simple syntax
  '(("irb"
     (:command iruby-build-irb-cmd "--inf-ruby-mode")
     (:binding "IRB.CurrentContext.workspace.binding")
     ;;; FIXME needs docs for the args to the format string & application
     (:completion "IRB::InputCompletor::CompletionProc.call('%s','%s').compact.each{ |x| puts x }"))

    ;; NB concerning behaviors of `iruby-build-irb-cmd':
    ;;
    ;; insofar as for implementations defined here with defcustom ...
    ;;
    ;; When launching irb via a "ruby" cmd, e.g "ruby" or "ruby27" etc,
    ;; the ruby cmd will receive arguments in `iruby-ruby-irb-prefix',
    ;; via `impl-cmd-list'. The args listed for irb, as here,
    ;; will then be added after the trailing "--", such that should be
    ;; in `iruby-ruby-irb-prefix', subsequetnly returned by
    ;; `impl-cmd-list'
    ;;
    ;; FIXME it's functionally normal, though nontrivial to document
    ;;
    ;; NB it may not become any more trivial if trying to define any iruby
    ;; implementations with eieio classes & instances
    ;;
    ("ruby"
     ;; FIXME add and implement (:after_init [form]+)
     ;;  such that can be used to disable history recording in IRB/...
     ;; FIXME TBD flagging the ruby process as running under iruby,
     ;;  such that can be used for dispatching in irbrc, for arbitrary user code
     (:command iruby-build-irb-cmd "--inf-ruby-mode")
     (:binding "IRB.CurrentContext.workspace.binding")
     (:completion "IRB::InputCompletor::CompletionProc.call('%s','%s').compact.each{ |x| puts x }"))

    ("jruby"
     (:command "jruby -S irb --prompt default --noreadline -r irb/completion")
     (:binding) ;; FIXME needs test
     (:completion) ;; FIXME needs test
     )

    ("rubinius"
     (:command "rbx -r irb/completion")
     (:binding) ;; FIXME needs test
     (:completion) ;; FIXME needs test
     )

    ("yarv"
     (:command "irb1.9 -r irb/completion")
     (:binding) ;; FIXME needs test
     (:completion) ;; FIXME needs test
     )

     ("macruby"
     (:command "macirb -r irb/completion")
     (:binding) ;; FIXME needs test
     (:completion) ;; FIXME needs test
     )

    ("pry"
     (:command "pry")
     (:binding "Pry.toplevel_binding")
     (:completion) ;; FIXME needs test
     )
    )
  "An alist mapping Ruby implementation names to Irb commands.
CDR of each entry must be a string, a function, a list of strings or
functions.

For any function in the CDR: The function must accept the implementation
name as a first argument and must return a list of string values. The
return value of that function will then be joined with any preceding and
subsequent values provided here in the implementation CDR here. The
elements of the subsequent argument list will then be effectively
concatenated with the space character as a separator, before being
passed to comint.

If a function is listed as the first element in the CDR here, the
first element of that funtion's return value should indicate the name of
the shell command to use for launching the implementation, with any
subsequent values to be interprted as command line arguments for that
shell command."
  :type '(repeat (cons :tag "Implementation definition"
                  (string :tag "Implementation name")
                  (list :tag "-- Definition fields"
                   ;; FIXME custom is not parsing this
                   (cons :tag "---- Shell command definition"
                         (const :command)
                         (choice :tag "Command and arguments"
                                 string function
                                 (repeat (choice string function))
                            ))
                   (cons :tag "---- Workspace binding expression"
                         (const :binding)
                         (repeat string))
                   (cons :tag "---- Completion expression"
                         (const :completion)
                         (repeat string))
                   )))
  :group 'iruby-impl)


(defcustom iruby-default-implementation "ruby"
  "Which Ruby implementation to use if none is specified."
  :type `(choice ,@(mapcar (lambda (item) (list 'const (car item)))
                           iruby-implementations))
  :group 'iruby-impl)


(defun iruby-impl-props (impl)
  ;; utility function for parsing the data structures under
  ;; iruby-implementations
  (let ((impl-decl (assoc impl iruby-implementations)))
    (cond
      (impl-decl
       (cdr impl-decl))
      (t
       (error "Implementation not found; %s" impl)))))

;; (iruby-impl-props iruby-default-implementation)

;; (assq :command (iruby-impl-props iruby-default-implementation))

(defun iruby-build-impl-cmd (&optional impl)
  ;; parse a command specifier provided under iruby-implementations,
  ;; given an implementation name onto the same
  (let* ((%impl (or impl iruby-default-implementation))
         (implprops (iruby-impl-props %impl))
         (cmd (cdr (assq :command implprops))))
    (cl-labels ((join (argv)
                  (cl-etypecase argv
                    (string (split-string-and-unquote argv))
                    (list argv)
                    ))
                (parse (impl elt)
                  (cl-typecase elt
                    (cons
                     (join (mapcan #'(lambda (%elt)
                                       (join (parse impl %elt)))
                                   elt)))
                    (null
                     (error "Unknown ruby implementation: %s" impl))
                    (symbol (funcall elt impl))
                    (string (split-string-and-unquote elt))
                    (t (error "Unknown command syntax for implementation %S: %S"
                              impl elt)))))
      (parse %impl (copy-list cmd)))))

;; (iruby-build-impl-cmd)
;; (iruby-build-impl-cmd "jruby")
;; (iruby-build-impl-cmd "n/a")

(defun iruby-build-irb-cmd (name &optional rest-args)
  ;; NB This will resuse the implementation name as the command name for
  ;; the implemmentation.
  ;;
  ;; The implementation name would generally be provided as a CAR in
  ;; some element of `iruby-implementations', with this function's
  ;; name then provided as the CDR or the first element in the
  ;; CDR of that element of `iruby-implementations'. Thus, the first
  ;; element of the value returned by this function would be used
  ;; as the name of the ruby or irb implementation to launch
  ;; under comint.
  ;;
  ;; If the implementation name in NAME does begin with either of the
  ;; strings  "irb" or "ruby", this will return the cons of NAME and
  ;; REST-ARGS
  ;;
  ;; This function will accept e.g "irb27" or "ruby-dev" as an
  ;; implementation name, then returning a list of command line argument
  ;; values in a manner similar to either the "irb" or "ruby" case.
  ;;
  ;; FIXME this custom configuration does not allow for providing an
  ;; implementation name with version suffix, independent of one of:
  ;;
  ;; A) a complete implementation description for the versioned impl,
  ;;   under `iruby-implementations' - or
  ;;
  ;; B) a complete command string or list (absent of binding delcs,
  ;;    etc) as the `iruby-default-implementation' and independent
  ;;    to the following function - or
  ;;
  ;; C) a versioned implementaiton named beginning with "irb" or "ruby",
  ;;    such that this implementation name would be provided
  ;;    independent of 'customize-option' as the value of
  ;;    `iruby-default-implementation'.
  ;;
  ;;    In effect, this defines an implementation class for each of the
  ;;    "ruby and "irb" implementations
  (let ((cmd-name name))
    (cond
      ((string-match "^irb" cmd-name)
       (cons cmd-name (cons (iruby-irb-rl-arg (cons cmd-name rest-args))
                            rest-args)))
      ((string-match "^ruby" cmd-name)
       ;; NB ensuring that any rest-args will appear after the args for
       ;; launching irb via ruby
       (let ((%prefix (cons cmd-name iruby-ruby-irb-args)))
         (append %prefix (list (iruby-irb-rl-arg %prefix))
                 rest-args)))
      (t (warn "iruby-build-irb-cmd: Unknown implementation %s" name)
         (cons cmd-name rest-args)))))

(defun iruby-irb-rl-arg (cmdlist)
  (let* ((output (shell-command-to-string
                  (mapconcat 'identity (append cmdlist (list "--version"))
                             " ")))
         (fields (split-string (string-trim-right output) "\s+"))
         (impl (car fields))
         (version (cadr fields)))
    (unless (string= impl "irb")
      (error "Unknown irb implementation: %s" output))
    ;; parsing only the major.minor.patchlevel versions here, for
    ;; compatibility with Emacs `version-to-list'. This may leave
    ;; out any ".pre...." suffix in the irb version string, such that
    ;; `version-to-list' may be unable to parse
    (setq version (mapconcat 'identity
                             (subseq (split-string version "\\." t) 0 3)
                             "."))
    (cond
      ((ignore-errors (version<= "1.2.0" version)) "--nomultiline")
      (t "--noreadline"))))


(defun iruby-get-impl-binding-expr (impl)
  "Return the Ruby binding expression for the iRuby implementation `impl'

If no binding expression is configured, returns nil"
  (let ((props (iruby-impl-props impl)))
    (mapconcat #'identity (cdr (assq :binding props))
               ";")))

(defun iruby-get-impl-completion-expr (impl)
  "Return the Ruby completion expression for the iRuby implementation `impl'

If no completion expression is configured, returns nil"
  (let ((props (iruby-impl-props impl)))
    (mapconcat #'identity (cdr (assq :completion props))
               ";")))

(make-variable-buffer-local
 (defvar iruby-impl-binding-expr nil
   "ruby expression for binding in eval under the active implementation"))

(make-variable-buffer-local
 (defvar iruby-impl-completion-expr nil
   "ruby expression for symbol name completion under the active
implementation"))

(defcustom iruby-console-environment 'ask
  "Envronment to use for the `iruby-console-*' commands.
If the value is not a string, ask the user to choose from the
available ones.  Otherwise, just use the value.

Currently only affects Rails and Hanami consoles."
  :group 'iruby
  :type '(choice
          ;; "the available ones" ??
          (const ask :tag "Ask the user")
          (string :tag "Environment name")))

(defgroup iruby-proc nil
  "iRuby process configuration"
  :group 'iruby)

(defcustom iruby-pager "cat"
  ;; FIXME find a way to pipe to xterm/other
  "Pager for Ruby process environments

This value should be a string providing a shell command. This shell
command will be used as the value for the PAGER environment variable in
Ruby process buffers.

The first whitespace-delimited element in this string should be a
literal shell command name or absolute path for a shell command name"
  :type `(string
          :validate
          ,(with-iruby-widget-validate (cmd "Command not found, in %s")
             (ignore-errors (executable-find (car (split-string-and-unquote cmd))))))
  :group 'iruby-proc)


(defcustom iruby-output-wait 0.005
  "Number of seconds for asynchronous delay in `iruby-show-last-output'

This value will provide a delay before displaying the result of
the last evaluation under  `iruby-send-region', when both
`iruby-show-last-output' and `iruby-threads-p' are non-nil.

Under such a configuration, `iruby-send-region' will create a separate
thread such that will call `iruby-show-last-output' after this number of
seconds of delay. This may serve to ensure that the iruby process has
produced any complete output, before `iruby-show-last-output' is
evaluated.  Moreover, it may serve to ensusure that the environment in
which `iruby-show-last-output' is called will be the current environment
as subsequent of the evaluation of `iruby-send-region' in the main Emacs
thread.

This value should be a positive number."
  :group 'iruby-proc)

(defcustom iruby-restart-timeout 1.25
  "Number of seconds for timeouts when closing or restarting a Ruby buffer.

This value represents a measure of seconds for timeout in `iruby-close-process'.
In that function, this value will be applied for each successive call as
to close the Ruby process

This timeout will be used similarly in `iruby-restart-process'

This value should be a positive number or zero."
  :group 'iruby-proc)


(defvar iruby-threads-p (featurep 'threads)
  "If non-nil, threads are available in this Emacs.

See also: `iruby-output-wait'")

(defconst iruby-prompt-format
  (concat
   (mapconcat
    #'identity
    '("\\(^%s> *\\)"                      ; Simple
      "\\(^(rdb:1) *\\)"                  ; Debugger
      "\\(^(byebug) *\\)"                 ; byebug
      "\\(^\\(irb([^)]+)"                 ; IRB default
      "\\([[0-9]+] \\)?[Pp]ry ?([^)]+)"   ; Pry
      "\\(jruby-\\|JRUBY-\\)?[1-9]\\.[0-9]\\(\\.[0-9]+\\)*\\(-?p?[0-9]+\\)?" ; RVM
      "^rbx-head\\)")                     ; RVM continued
    "\\|")
   ;; Statement and nesting counters, common to the last four.
   " ?[0-9:]* ?%s *\\)")
  "Format string for the prompt regexp pattern.
Two placeholders: first char in the Simple prompt, and the last
graphical char in all other prompts.")

(defvar iruby-first-prompt-pattern (format iruby-prompt-format ">" ">")
  "First prompt regex pattern of Ruby interpreter.")

(defvar iruby-prompt-pattern (format iruby-prompt-format "[?>]" "[\]>*\"'/`]")
  "Prompt regex pattern of Ruby interpreter.")

(defvar iruby-mode-hook nil
  "Hook for customizing `iruby-mode'.")

(cl-defmacro with-symbols-iruby ((&rest symbols) &body body)
  ;; an alternative to the common `with-gensym' pattern,
  ;; this does not use gensym.
  `(let ,(mapcar #'(lambda (s)
                     `(,s (make-symbol ,(concat "%" (symbol-name s)))))
                 symbols)
     ,@body))

;; e.g
;; (with-symbols-iruby (a b)
;;   `(let ((,a 1) (,b 2))
;;      (list* ,a ,b (quote ,a) (quote ,b))))

;; FIXME move the following, up to the menu def, into iruby-ui.el

(cl-defmacro define-keymap-iruby (name (&optional inherit)
                                          docs &rest key-literals)
  ;; returns as a composed keymap including
  ;; B) any values in 'inherit', for the second arg to the composed-keyamp form
  ;; C) any value for 'super', for the third arg
  ;; or an initially sparse keymap, if neither inherit nor super is provided
  ;;
  ;; expressions in key-literals will be used to set bindings on the
  ;; keymap, before the keymap is returned.
  (with-symbols-iruby (map bind keyexpr cmd keyspec)
    `(defvar ,name
       (let ((,map (make-sparse-keymap ,(symbol-name name))))
         ,@(when inherit `((set-keymap-parent ,map ,inherit)))
         (cl-dolist (,bind (quote ,key-literals) ,map)
           (cl-destructuring-bind (,keyexpr ,cmd) ,bind
             (let ((,keyspec (if (stringp ,keyexpr)
                                 (kbd ,keyexpr)
                               ,keyexpr)))
               (define-key ,map ,keyspec ,cmd)))))
       ,docs)))


(define-keymap-iruby iruby-mode-map (comint-mode-map)
  "Keymap for `iruby-mode'.

This keymap inherits bindings from `comint-mode-map'"
    ;;; common ...
    ("C-c C-l" iruby-load-file)
    ("C-c C-z" iruby-switch-to-inf)
    ("C-c C-s-z" iruby-switch-to-default-buffer) ;; ? TBD
    ;;; &rest
    ("RET" iruby-send-or-stage-input)
    ("C-x C-e" iruby-send-last-sexp)
    ("TAB" completion-at-point)
    ("C-x C-q" iruby-maybe-switch-to-compilation)
    ("ESC <up>" iruby-previous-prompt)
    ("ESC <down>" iruby-next-prompt))

(define-keymap-iruby iruby-minor-mode-map ()
"Keymap for `iruby-minor-mode'"
    ;;; common ...
  ("C-c C-l" iruby-load-file)
  ("C-c C-z" iruby-switch-to-inf)
  ("C-c C-s-z" iruby-switch-to-default-buffer) ;; ? TBD
    ;;; &rest
  ("C-M-x"   iruby-send-definition)
  ("C-x C-e" iruby-send-last-sexp)
  ("C-c C-b" iruby-send-block)
  ("C-c M-b" iruby-send-block-and-go)
  ("C-c C-x" iruby-send-definition)
  ("C-c M-x" iruby-send-definition-and-go)
  ("C-c C-r" iruby-send-region)
  ("C-c M-r" iruby-send-region-and-go)
  ("C-c C-z" iruby-switch-to-inf)
  ("C-c C-l" iruby-load-file)
  ("C-c C-s" iruby))


(easy-menu-define iruby-minor-mode-menu iruby-minor-mode-map
  "iRuby Minor Mode Menu"
  `(,iruby-app-name
    :label iruby-app-name
    ["Send definition" iruby-send-definition t]
    ["Send last expression" iruby-send-last-sexp t]
    ["Send block" iruby-send-block t]
    ["Send region" iruby-send-region t]
    "--"
    ["Load file..." iruby-load-file t]
    "--"
    ["Start REPL" iruby t]
    ["Switch to REPL" iruby-switch-to-inf t]))


(defvar iruby-load-file-history nil
  "History data for interactive comint forms with `iruby-load-file'

The bounds of this history table under `iruby-load-file' may be
configured with `iruby-load-file-history-limit'")

(defconst iruby-error-regexp-alist
  '(("^SyntaxError: \\(?:compile error\n\\)?\\([^\(].*\\):\\([1-9][0-9]*\\):" 1 2)
    ("^\tfrom \\([^\(].*\\):\\([1-9][0-9]*\\)\\(:in `.*'\\)?$" 1 2)))

;;;###autoload
(defun iruby-setup-keybindings ()
  "[Deprecated] Key bindings are initailized when iruby.el is loaded

See also: `iruby-mode-map' and `iruby-minor-mode-map'"
  (warn "`iruby-setup-keybindings' is deprecated, please don't use it anymore.")
  (warn "If you're using `iruby' from Git, please look up the new usage instructions."))

(make-obsolete 'iruby-setup-keybindings 'add-hook "2.3.1")


(defvar iruby-warnings-once nil
  "Session-local storage for `iruby-warn-once'")

(defun iruby-warn-once (message &rest format-args)
  "Call `warn' with `message' and `format-args' unless a similar warning
message has already been produced under `iruby-warn-once'

This function is used in `iruby-completion-at-point', to ensure that the
user is notified at most once when the local iRuby implementation does
not have any completion support enabled in iRuby"
  (let ((msg (apply #'format-message message format-args)))
    (unless (member msg iruby-warnings-once)
      ;; NB ensuring that the actual format-args are passed to the
      ;; warning - using the locally produced msg only for purposes
      ;; of caching
      (push msg iruby-warnings-once)
      (apply #'warn message format-args))))



(defvar iruby-warnings-once nil
  "Session-local storage for `iruby-warn-once'")

(defun iruby-warn-once (message &rest format-args)
  "Call `warn' with `message' and `format-args' unless a similar warning
message has already been produced under `iruby-warn-once'

This function is used in `iruby-completion-at-point', to ensure that the
user is notified at most once when the local iRuby implementation does
not have any completion support enabled in iRuby"
  (let ((msg (apply #'format-message message format-args)))
    (unless (member msg iruby-warnings-once)
      ;; NB ensuring that the actual format-args are passed to the
      ;; warning - using the locally produced msg only for purposes
      ;; of caching
      (push msg iruby-warnings-once)
      (apply #'warn message format-args))))


;;;###autoload
(define-minor-mode iruby-minor-mode
  "Minor mode for interacting with the inferior process buffer.

The following commands are available:

\\{iruby-minor-mode-map}"
  ;; NB the :lighter form will not be evaluated - will be passed as a
  ;; literal form for the corresponding CADR in `minor-mode-alist'.
  ;;
  ;; The literal form must be of a format compatible onto `mode-line-format'

  :lighter iruby-minor-modeline-format ;; was working ..
  ;;:lighter " i?"
  :keymap iruby-minor-mode-map)


(make-variable-buffer-local
 (defvar iruby-buffer nil
   "When non-nil, the iruby process buffer to use for this buffer.

If nil, `iruby-default-ruby-buffer' may be used.

See also: `iruby-get-prevailing-buffer', `iruby-proc'"))

(make-variable-buffer-local
 (defvar iruby-buffer-command nil "The command used to run Ruby shell"))

(defvar iruby-buffer-impl-name nil
  "Implementation name for a Ruby process buffer")
(make-variable-buffer-local 'iruby-buffer-impl-name)

(defun iruby-initialize-impl-bindings (&optional impl)
  ;; shared forms for `iruby-mode' (e.g under `run-iruby-new')
  ;; and `iruby-restart-process'
  ;;
  ;; This is implemented here rather than in iruby-mode, as iruby-mode
  ;; does not presently receive an implementation name
  (let ((proc (get-buffer-process (current-buffer)))
        (use-impl (or impl iruby-buffer-impl-name)))
    (cond
      (proc
       (setq iruby-buffer-command (process-command proc))
       (cond
         (use-impl
          (setq
           iruby-impl-binding-expr (ignore-errors
                                     (iruby-get-impl-binding-expr use-impl))
           iruby-impl-completion-expr (ignore-errors
                                        (iruby-get-impl-completion-expr use-impl))))
         (t (warn "Unknown Ruby implementation for %s (nil)" (current-buffer)))))
      (t (warn "Unable to initialize buffer %s for iRuby (no process)"
               (current-buffer))))))

(define-derived-mode iruby-mode comint-mode 'iruby-app-name
  "Major mode for interacting with an inferior Ruby REPL process.

A simple IRB process can be fired up with \\[iruby].

To launch a REPL with project-specific console instead, type
\\[iruby-console-auto].  It recognizes several
project types, including Rails, gems and anything with `racksh'
in their Gemfile.

Customization: When entered, this mode runs `comint-mode-hook' and
`iruby-mode-hook' (in that order).

You can send text to the inferior Ruby process from other buffers containing
Ruby source.

    `iruby-switch-to-inf' switches the current buffer to the ruby process buffer.
    `iruby-send-definition' sends the current definition to the ruby process.
    `iruby-send-region' sends the current region to the ruby process.
    `iruby-send-definition-and-go' and `iruby-send-region-and-go'
        switch to the ruby process buffer after sending their text.

Commands:
`RET' after the end of the process' output sends the text from the
    end of process to point.
`RET' before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
`DEL' converts tabs to spaces as it moves back.
`TAB' completes the input at point. IRB, Pry and Bond completion is supported.
`C-M-q' does `TAB' on each line starting within following expression.
Paragraphs are separated only by blank lines.  # start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

The following commands are available:

\\{iruby-mode-map}"
  :group 'iruby
  :syntax-table (iruby-ruby-syntax-table)
  :abbrev-table (iruby-ruby-abbrev-table)
  :interactive nil
  (setq comint-prompt-regexp iruby-prompt-pattern) ;; TBD. see next @ read-only
  (ruby-mode-variables) ;; NB ruby-mode dep. See alt enh-ruby-mode.el
  (when (bound-and-true-p ruby-use-smie)
    (set (make-local-variable 'smie-forward-token-function)
         #'iruby-smie--forward-token)
    (set (make-local-variable 'smie-backward-token-function)
         #'iruby-smie--backward-token))

  (set (make-local-variable 'comint-delimiter-argument-list)
        '(?\| ?& ?< ?> ?\( ?\) ?\; ?\"))

  (add-hook 'comint-preoutput-filter-functions 'iruby-preoutput-filter nil t)
  (add-hook 'comint-output-filter-functions 'iruby-output-filter nil t)

  (add-to-list 'kill-buffer-hook 'iruby-drop-process)

  (unless (boundp 'desktop-save-buffer)
    (make-variable-buffer-local
     ;; ensure the variable is defined as buffer-local,
     ;; so that this does not become a global default value
     ;; once desktop.el is loaded
     (defvar desktop-save-buffer nil
       "Buffer-local configuration for `desktop-save'.
This variable is normally initialized by the Emacs desktop library,
once that feature has become available in this Emacs session")))

  (setq desktop-save-buffer 'iruby-desktop-misc-data)

  (setq comint-get-old-input 'iruby-get-old-input
        comint-use-prompt-regexp nil)
  (set (make-local-variable 'compilation-error-regexp-alist)
       iruby-error-regexp-alist)
  (set (make-local-variable 'comint-prompt-read-only) iruby-prompt-read-only)
  (when (eq system-type 'windows-nt)
    (setq comint-process-echoes t))
  (add-hook 'completion-at-point-functions 'iruby-completion-at-point nil t)
  (compilation-shell-minor-mode t))

;; adapted from replace-in-string in XEmacs (subr.el)
(defun iruby-remove-in-string (str regexp)
  "Remove all matches in STR for REGEXP and returns the new string."
  (let ((rtn-str "") (start 0) match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
            start (match-end 0)
            rtn-str (concat rtn-str (substring str prev-start match))))
    (concat rtn-str (substring str start))))


(defun iruby-previous-nonblank-output (point &optional non-current)
  "move point backwards over output fields from any blank continued
input prompt

This function provides a utility for `iruby-previous-prompt'"
  (interactive "d")
  (let ((at point)
        start end
        match)
    (goto-char at)
    (when (setq match (text-property-search-backward 'field 'output t non-current))
      (goto-char at) ;; reset after the property search moved point
      (setq start (prop-match-beginning match)
            end (prop-match-end match))
      (cond
        ((string-whitespace-p (buffer-substring-no-properties start end))
         (iruby-previous-nonblank-output start t))
        (t (goto-char end))))))

(defun iruby-next-nonblank-output (point &optional non-current)
  (interactive "d")
  (let ((at point)
        start end
        match)
    (goto-char at)
    (when (setq match (text-property-search-forward 'field 'output t non-current))
      (goto-char at) ;; reset after the property search moved point
      (setq start (prop-match-beginning match)
            end (prop-match-end match))
      (cond
        ((string-whitespace-p (buffer-substring-no-properties start end))
         (iruby-next-nonblank-output start t))
        ;; NB this does not differentiate between prompt output and
        ;; other output
        ((= start at) ;; ?
         (iruby-next-nonblank-output end)
         ;; (goto-char end)
         )
        (t
         (goto-char start))))))

(defun iruby-input-start (point)
  "If POINT is within an input area, move point to the end of the
previous prompt preceding POINT - similarly, the beginning of the
input area.

If POINT is not within an input area, point will be moved into the
previous input area, immediately after the end of the previous prompt.

This function assumes that the subprocess' prompt is configured to not
display additional printable text during continued input, such as with
the --inf-ruby-mode prompt under irb.

See also: `iruby-input-end', `iruby-input-at-point'"
  (interactive "d")
  (let ((at point)
        (start point)
        match)
    (goto-char at)
    (cond
      ((or (zerop at) (= at (point-min)))
       ;; start of buffer
       ;; - go forward to end of prompt, no further processing
       (when (setq match (text-property-search-forward 'field 'output t))
         (goto-char (prop-match-end match))))
      ((= 1 (line-number-at-pos at))
       (case (get-text-property at 'field)
         (output ;; assumption: point is at a prompt (first line of buffer)
          (when (setq match (text-property-search-forward 'field nil 'eq t))
            (goto-char (prop-match-beginning match))))
         (t ;; assumption: point is in input (first line of buffer)
          (when (setq match (text-property-search-backward 'field nil 'eq t))
            (goto-char (prop-match-beginning match))))))
      (t
       (let ((field (get-text-property at 'field)))
         ;; (when (eq field 'output)
         ;;   ;; scan backwards over output
         ;;   (when (setq match (text-property-search-backward 'field 'output 'eq))
         ;;     (goto-char (setq start (prop-match-beginning match)))))

         (when (setq match (text-property-search-backward 'field nil 'eq))
           ;; scan to the start of the nearest previous input
           (goto-char (setq start (prop-match-beginning match))))

         ;; scan backwards over continued input prompts,
         ;; assuming the subprocess prompt is configured to be an empty string
         (iruby-previous-nonblank-output start)
         (point)
         )))))


(defun iruby-input-end (point)
  (interactive "d")
  ;; FIXME does not actually move point past the current input,
  ;; except with new input not yet reprinted by comint
  ;;
  ;; nonetheless useful for `iruby-input-at-point'
  (iruby-input-start point)
  (iruby-next-nonblank-output (point))
  (let ((match (text-property-search-backward 'field 'boundary 'eq)))
    (when match (goto-char (prop-match-beginning match))))
  (point))


(defun iruby-previous-prompt (point)
  (interactive "d")
  (let ((at point)
        end)
    (when (and (null (get-text-property at 'iruby-prompt))
               (setq end (previous-single-property-change at 'iruby-prompt)))
      ;; backwards into previous prompt
      (goto-char (setq at end)))
    (when (and (get-text-property at 'iruby-prompt)
               (setq end (previous-single-property-change at 'iruby-prompt))
               (setq end (previous-single-property-change end 'iruby-prompt)))
      (goto-char end)
      (setq at end))
    ;;; this would cause it to behave strangely when point begins on an
    ;;; input field
    ;; (setq end (next-single-property-change at 'iruby-prompt))
    (when end
      (goto-char end)
      ;; move to an input field
      (when (and end
                 (get-text-property end 'field)
                 (setq end (next-single-property-change end 'field)))
        (goto-char end)))
    ))


(defun iruby-next-prompt (point)
  (interactive "d")
  (let ((at point)
        end)
    (when (and (get-text-property at 'field)
               (setq end (next-single-property-change at 'field)))
      ;; move forward out of any non-input field
      (goto-char end))

    (when (and (null (get-text-property at 'iruby-prompt))
               (setq end (next-single-property-change at 'iruby-prompt)))
      ;; backwards into previous prompt
      (goto-char (setq at end)))
    ;;; this would cause it to behave strangely when point begins on an
    ;;; input field
    ;; (setq end (next-single-property-change at 'iruby-prompt))
    (when end
      (goto-char end)
      ;;; move to an input field
      (when (and end
                 (get-text-property end 'field)
                 (setq end (next-single-property-change end 'field)))
        (goto-char end))
      )))



(defun iruby-input-at-point (point)
  "Retrieve any input at point

See also: `iruby-input-start', `iruby-input-end',
`iruby-get-old-input'"
  (interactive "d")
  (save-excursion
    (save-restriction
      (let (start match end expr)
        (iruby-previous-prompt point)
        (setq start (point))
        (cond
          ((and (setq match (text-property-search-forward 'iruby-prompt nil))
                (goto-char (prop-match-beginning match))
                (setq match (text-property-search-backward 'field 'output)))
           (setq end (prop-match-end match)))
          (t (setq end start)))
        (setq expr (string-trim (buffer-substring-no-properties start end)))
        (when (interactive-p) (message "At point: %S" expr))
        expr))))

(defun iruby-get-old-input ()
  "Return a string from some previous section of input history
in an iruby process buffer

This function is used for iruby process buffers as the value
of `comint-get-old-input'. Thus, this function may be used by
`comint-previous-input', and as such, may be used by
`comint-send-input', to determine some section of earlier
input to re-use under a certain condition of `point`.

See also `iruby-send-or-stage-input'"
  (save-excursion
    (save-restriction
      (let* ((at (point))
             (at-field  (get-text-property at 'field))
             (at-prompt (get-text-property at 'iruby-prompt))
             start end next-end match expr)
        (cond
          (at-prompt
           ;'; mov to end of current prompt
           (when (setq match
                       (text-property-search-forward 'iruby-prompt t 'eq))
             (goto-char (prop-match-end match))))
          (t ;; move to end of previous prompt
           (when (setq match
                       (text-property-search-backward 'iruby-prompt t nil))
             (goto-char (prop-match-end match)))))
        ;; return input at point
        (iruby-input-at-point (point)))
      )))

(defun iruby-stage-old-input ()
  "Utility function for `iruby-send-or-stage-input'"
  ;; TBD may not completely capture the first line of input,
  ;; instead capturing everything after point there.
  (interactive)
  (save-restriction
    (let ((str (iruby-get-old-input)))
      ;; (warn "OLD STR %S" str)
      (goto-char (iruby-strip-pending-input))
      ;; (goto-char (point-max)) ;; ?
      (insert str))))

(defun iruby-send-or-stage-input ()
  "Send or stage input in an `iruby-mode' buffer.

If point is within a previous input area, that input will be copied to
the most recent prompt, to allow editing before send. Any pending input
at the projmpt will be stored and presented after the next input
prompt, in the buffer.

Otherwise, any input at the prompt will be sent to the iRuby process."
  ;;
  ;; FIXME may not be compat. with ruby 2.7?
  ;; - needs debug & test w/ ruby 2.7 [sol]
  ;; - FIXME update lang/ruby-platform port
  ;;   to provide packages named e.g ruby30-platform
  ;;   or ruby-platform (for that matching a default Ruby version on
  ;;   the build host)
  (interactive)
  (let* ((initial (point))
         (buffer-end (goto-char (point-max)))
         (prompt-end (progn (iruby-input-start buffer-end)
                            (point))))
    (goto-char initial)
    (cond
      ((<= prompt-end initial buffer-end)
       ;; initial point was within the last input area in the buffer
       ;;
       ;; NB this is reached for continued input, as well as for whole
       ;; expressions on the current input line
       (comint-send-input))
      (t
       (iruby-stage-old-input)))))


;;
;; -----
;;

(defun iruby-buffer-in-directory (dir)
  (setq dir (expand-file-name dir))
  (catch 'buffer
    (dolist (buffer (mapcar #'cdr iruby-process-buffers))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (string= (expand-file-name default-directory) dir)
            (throw 'buffer buffer)))))))

(defun iruby-read-impl (&optional prompt)
  "Read the name of an implementation in `iruby-implementations',
returning `iruby-default-implementation' if user has entered no text.

PROMPT will default to the string, \"Ruby Implementation: \""
  (let* ((txt
          (completing-read (or prompt "Ruby Implementation: ")
                           (mapc #'car iruby-implementations)
                           nil t)))
    (if (and (stringp txt) (zerop (length txt)))
        iruby-default-implementation
      txt)))

;;;###autoload
(defun iruby (&optional impl)
  "Run an inferior Ruby process in a buffer.
With prefix argument, prompts for which Ruby implementation
\(from the list `iruby-implementations') to use.

If there is a Ruby process running in an existing buffer, switch
to that buffer. Otherwise create a new buffer."
  (interactive (list (if current-prefix-arg
                         (iruby-read-impl)
                       iruby-default-implementation)))
  (let ((%impl (or impl iruby-default-implementation)))
    (let ((command (iruby-build-impl-cmd %impl)))
      (run-iruby command %impl current-prefix-arg))))


(defun iruby-get-prevailing-buffer (&optional no-filter-live)
  "Return the active iRuby process buffer

This function performs the following checks, in squence,
returning the first available buffer:
- the current buffer, if an `iruby-mode' buffer
- any non-nil binding for `iruby-buffer'
- if `iruby-use-project-directories' is true,
  then the first buffer matched to a file for `iruby-console-match'
  beginning at the present `default-directory'
- lastly, `iruby-default-ruby-buffer'

If `no-filter-live' is non-nil, then the iRuby process for the first
matched buffer must not be a closed process. Otherwise, a buffer may
match irrespective of whether the buffer's iRuby process is running"
  ;; NB This is a utility function, used in:
  ;; - `run-iruby'
  ;; - `iruby-proc'
  ;;    subsq, in `iruby-send-region' ...
  ;;
  ;; This function provides an alternate implementation after the
  ;; original `inf-ruby-buffer' function
  (cl-flet ((check-buffer (buffer)
              (when buffer
                (if no-filter-live
                    buffer
                  (when (process-live-p (iruby-buffer-process buffer))
                    buffer)))))
    (or (when (eq major-mode 'iruby-mode)
          (current-buffer))
        (check-buffer iruby-buffer)
        (when iruby-use-project-directories
          (let ((project-dir (locate-dominating-file default-directory
                                                     #'iruby-console-match)))
            (when project-dir
              (check-buffer (iruby-buffer-in-directory project-dir)))))
        (check-buffer iruby-default-ruby-buffer)
        (cl-block last
          (dolist (elt iruby-process-buffers)
            (let ((b (cdr elt)))
              (when (check-buffer b)
                (cl-return-from last b)))))
        )))

;;;###autoload
(defun run-iruby (&optional command name always)
  "Run an inferior Ruby process, input and output in a buffer.

If there is a process already running in a corresponding buffer,
switch to that buffer. Otherwise create a new buffer.

The consecutive buffer names will be:
`*NAME*', `*NAME*<2>', `*NAME*<3>' and so on.

COMMAND defaults to the default entry in `iruby-implementations'.

NAME defaults to the nondirectory filename of the first element in the
command string

If called interactively with a prefix argument, the user will be
prompted to enter a shell command for launching the irb
process. Otherwise under interactive evaluation, the shell command for
`iruby-default-implementation' will be used.

Runs the hooks `comint-mode-hook' and `iruby-mode-hook'.

Type \\[describe-mode] in the process buffer for the list of commands.

If `always' is non-nil, this will launch a new ruby process whether or
not a ruby process is already running for the implementation denoted in
the `command' value"
  ;; This function is interactive and named like this for consistency
  ;; with `run-python', `run-octave', `run-lisp' and so on.
  ;; We're keeping both it and `iruby' for backward compatibility.
  (interactive (list (let ((cmd (if current-prefix-arg
                                    (read-shell-command "Run irb: ")
                                  ;; NB read-shell-command would return
                                  ;; "" on no input
                                  "")))
                       (if (zerop (length cmd))
                           (iruby-build-impl-cmd iruby-default-implementation)
                         cmd))))
  (let* ((%command (or command (iruby-build-impl-cmd)))
         (%name (or name (file-name-nondirectory
                          (car (split-string-and-unquote %command)))))
         (buffer (iruby-get-prevailing-buffer)))
    (run-iruby-or-pop-to-buffer %command %name buffer always)))

(defun iruby-process-sentinel (process state)
  "Process sentinel installed by `run-iruby-new'

This function will display a warning after any change of state other
than exit, hangup, or finished for a ruby subprocess initialized with
`run-iruby-new'"
  (let ((status (process-status process)))
    ;; NB the second arg delivered to the process sentinel
    ;; will normally be an informative string.
    ;;
    ;; The "hangup" state may generally indicate a normal exit
    (unless (or (memq status '(exit finished))
                (and (stringp state) (string= state "hangup")))
      (warn "iRuby process %s state changed (%S): %S"
            process status state))))

(defun iruby-get-last-output (&optional proc)
  "Return the last output from the ruby process PROC as a string.

If PROC is nil, the value returned by `iruby-proc' will be used
as PROC

Known limitation: This function requires that any prompt string
in the ruby process, including any empty string, will have been
displayed within a single line of text, beginning at start of
line."
  (let* ((proc (or proc (iruby-proc t)
                   (error "No iruby-proc found")))
         (buff (and proc (iruby-process-buffer proc)))
         (mark (process-mark proc)))
    (save-excursion
     (save-restriction
        (set-buffer buff)
        (goto-char mark)
        ;; Assuming  a single-line prompt, it should be sufficient to
        ;; move to the previous line's end of line, here. This may in
        ;; effect move point backwards past the prompt.
        (previous-line)
        (end-of-line)
        (let* ((eol (point))
               (prop (get-text-property eol 'field)))
          (when (eq prop 'output)
            ;; point was moved into an output field area, i.e
            ;; not into an input field area. Thus, point is at
            ;; the end of a section of output, in this branch
            (cl-do ((at eol (1- at)))
                   ;; NB this scans backwards for an input field,
                   ;; such that would have a null 'field' text property
                   ;; under comint. This is in lieu of potentially
                   ;; inconsistent behaviors under some calls to
                   ;; text-property-search-* functions in Emacs
                   ((null (get-text-property at 'field))
                    ;; return value
                    (buffer-substring-no-properties (+ at 1) eol)))
            ;; FIXME now to parse the text, pop up any backtrace
            ;; navigation buffer, etc
            ))))))


(defun iruby-show-last-output (&optional proc)
  "Display the most recent output from the ruby process PROC in the
minibuffer. If no output except a prompt has been produced since the
last input to the ruby process, this function will display a message
indicating the absence of any new output text.

The process returned by `iruby-proc' will be used as the default PROC

See also: `iruby-get-last-output', `iruby-print-result'"
  (interactive)
  (let* ((%proc (or proc (iruby-proc)))
         (whence (buffer-name (iruby-process-buffer %proc)))
         (last (iruby-get-last-output %proc)))
    (if last
        (message "%s: %s" whence last)
      (message "%s: No output" whence)
      )))


(defun iruby-process-impl (whence)
  (iruby-buffer-short-name
   (etypecase whence
     (process (iruby-process-buffer whence))
     (buffer whence))))

(defun iruby-buffer-short-name (whence)
  (cl-block self
    (let ((name (etypecase whence
                  (string whence)
                  (buffer (cond
                            ((buffer-live-p whence)
                             (iruby-buffer-short-name (buffer-name whence)))
                            (t (cl-return-from self nil))))
                  (process (or (iruby-buffer-short-name (iruby-process-buffer whence))
                               (cl-return-from self nil))))))
      (cond
        ((string-match "^\\*\\(.*\\)\\*\\(<.*>\\)?$" name)
         (concat (match-string-no-properties 1 name)
                 (match-string-no-properties 2 name)))
        (t name)))))

;; (iruby-buffer-short-name (caar iruby-process-buffers))
;; e.g => "ruby"

;; (iruby-buffer-short-name "*ruby*")
;; => "ruby"

;; (iruby-buffer-short-name "*ruby*<1>")
;; => "ruby<1>"

;;
;; utility forms (iruby-proc)
;;

(cl-defmacro with-iruby-process-environment ((&rest bindings)
                                             &rest body)
  "evaluate BODY in a lexical environment with a process environment for iRuby

Each element in BINDINGS should be provided as a literal string or a
form that will evalute to either a string or nil value. Each non-nil
value will be prepended to a modified form of the current value of
`process-environment', overriding any values bound in the latter. The
modified `process-environment' value will have a PAGER binding prepended
for `iruby-pager', previous to prepending any values from BINDINGS.

The value used for the effective `process-environment' in this form
should be inherited by any subprocess initialized by Emacs, in
evaluation of the BODY forms. This macro itself will not store the
`process-environment' value, external to that lexical environment."
  `(let ((process-environment
          (append
           (remove-if 'null (list ,@bindings))
           (cons (format "PAGER=%s" iruby-pager)
                 ;; http://debbugs.gnu.org/15775
                 (cl-remove-if (lambda (binding)
                                 (equal (car (split-string binding "=")) "PAGER"))
                               process-environment)))))
     ,@body))

;; (with-iruby-process-environment () (shell-command-to-string "echo -n $PAGER"))
;; => string value of iruby-pager

;;
;; interactive forms (for iruby-proc)
;;

(defvar iruby-process-history nil
  "History list for `iruby-read-process'")

(defun iruby-read-process (&optional prompt require-live)
  (cond
    ((null iruby-process-buffers)
     (error "No active iRuby processes"))
    ((cdr iruby-process-buffers)
     (let* ((prompt (or prompt "iRuby Process: "))
            (require-live nil)
            (table (mapcan #'(lambda (elt)
                               (let ((p (car elt)))
                                 (when (or (null require-live)
                                           (process-live-p p))
                                   (let ((name (iruby-buffer-short-name p)))
                                     (when name
                                       (list (cons name p)))))))
                           iruby-process-buffers))
            (default (caar table))
            (selected
             (completing-read (format "%s (default: %s): " prompt default)
                              table nil t nil 'iruby-process-history default)))
       (cdr (assoc selected table)))
     )
    (t
     ;; only one process registered
     (caar iruby-process-buffers))))

(defun iruby-read-process-interactive (&optional prompt require-live)
  (list (if current-prefix-arg
            (iruby-read-process prompt require-live)
          (iruby-buffer-process
           (iruby-get-prevailing-buffer (not require-live))))))

(defun iruby-switch-to-process (&optional process)
  "If the iRuby process buffer denoted by `process' is displayed in
a buffer on the current frame, then switch to that buffer. Else, switch
to the iRuby process buffer, using the window of the current buffer.

If called interactively, the default Ruby process will be used unless
called with an interactive prefix argument, in which case the user will
be queried to select a process to switch to.

See also:
`iruby-switch-to-process-other-window'
`iruby-switch-to-process-other-frame'"
  (interactive
   (iruby-read-process-interactive  "Switch to iRuby Process:"))
  (cl-block top
    (let ((buff (etypecase process
                  (process (iruby-process-buffer process))
                  (buffer process)
                  (null
                   (let ((it (iruby-get-prevailing-buffer)))
                     (cond
                       (it it)
                       (t
                        (warn "iruby-switch-to-process: nil is not a process")
                        (cl-return-from top)))))))
          (frame (selected-frame))
          window)
    (cl-block window-found
      (walk-windows (lambda (wn)
                      (when (eq (window-buffer wn) buff)
                        (setq window wn)
                        (return-from window-found)))
                    nil frame))
    (cond
      (window (select-window window))
      (t (pop-to-buffer buff))))))

(defun iruby-switch-to-process-other-window (process)
  "If the iRuby process buffer denoted by `process' is displayed in
a buffer on the current frame, then switch to that buffer, else create
and switch to a new window for the iRuby process buffer

See also: `iruby-switch-to-process'"
  (interactive (list (iruby-read-process "Switch to iRuby Process: ")))
  (switch-to-buffer-other-window (iruby-process-buffer process)))

(defun iruby-switch-to-process-other-frame (process)
  "If the iRuby process buffer denoted by `process' is displayed in
a buffer on the current frame, then switch to that buffer, else create
and raise a new frame for the iRuby process buffer

See also: `iruby-switch-to-process'"
  (interactive (list (iruby-read-process "Switch to iRuby Process: ")))
  (switch-to-buffer-other-frame (iruby-process-buffer process)))

(defun iruby-use-process (process buffer)
  "Select an iRuby process to use for the specified source buffer.

The `process' should be an Emacs process object, such that may be
determined with `iruby-read-process'.

If `buffer' is the symbol `t', this will set the global iRuby process
under `iruby-default-ruby-buffer'.

Otherwise, `buffer' should denote an active source buffer, typically a
buffer in some Ruby source file mode. In this case, the `process' will
be selected for storing the iRuby buffer of that process in the value of
`iruby-buffer' in the provided `buffer'.

This function may be called interactively. If called with an interactive
prefix argument, the user will be queried as to whether to set the value
globally, or for which buffer to bind to the specified process. If
called without an interactive prefix argument, this will select an iRuby
process for the current buffer.

Assuming that the function `iruby-ensure-desktop-support' has been
called, the global and buffer-local process bindings configured with
this function will be stored by `desktop-save' and restored by
`desktop-load'."
  (interactive (list (iruby-read-process "Use process: ")
                     (if current-prefix-arg
                         (or (yes-or-no-p "Set globally? ")
                             (read-buffer "Switch process for buffer: "))
                       (current-buffer))))
  (let* ((buffproc (iruby-buffer-process buffer))
         (procbuff (when buffproc
                     (iruby-process-buffer buffproc))))
    (cond
      ((eq buffer t)
       (unless (process-live-p process)
         (warn "Using extant process %s globally" process ))
       (setq iruby-default-ruby-buffer
             (iruby-process-buffer process)))
      ((eq buffer procbuff)
       (error "Cannot change the iRuby process for process buffer %s" buffer))
      ((and buffproc (eq process buffproc))) ;; no-op
      (t (with-current-buffer buffer
           (unless (process-live-p process)
             (warn "Using extant process %s in buffer %s" process buffer))
           (setq iruby-buffer (iruby-process-buffer process)))))))

(defun iruby-process-status (whence)
  "If a process is associated with `whence', then return the process
status for that process, else nil.

`whence' may be a string denoting a buffer name, or a buffer object, or
an Emacs process object

See also: `iruby-process-running-p'"
  (let ((proc (cl-etypecase whence
                (string (iruby-process-running-p (get-buffer whence)))
                (buffer (when (buffer-live-p whence)
                          (get-buffer-process whence)))
                (process whence))))
    (when proc
      (process-status proc))))

(defun iruby-process-running-p (whence)
  "Return true if a process is associated with `whence' and that process
is in a 'run' state

`whence' may be a string denoting a buffer name, or a buffer object, or
an Emacs process object

See also: `iruby-process-status'"
  (eq (iruby-process-status whence) 'run))

(defvar iruby-process-buffers nil
  "Internal storage for buffer/process mapping in iRuby

See also: functions `iruby-buffer-process' and `iruby-process-buffer'

For interactive forms, see also: `iruby-read-process'")


(defun iruby-process-buffer (process)
  "Alternative to `process-buffer', using `iruby-process-buffers'

This function may differ with relation to `process-buffer', insofar as
for behaviors onto a closed process.

See also: `iruby-buffer-process'"
  (cdr (assq process iruby-process-buffers)))

(defun iruby-buffer-process (buffer)
  "Alternative to `get-buffer-process', using `iruby-process-buffers'

Unlike `get-buffer-process', this function should return any closed
iRuby process originally stored for the buffer under `iruby-process-buffers'.
Applications may then access the process object normally, for data such
as the original `process-command' or the present `process-status'

This function is generally for interactive forms and is not a drop-in
replacement for `get-buffer-process'.

See also: `iruby-process-buffer', `iruby-restart-process',
`iruby-switch-to-process'"
  (car (rassq buffer iruby-process-buffers)))


(defun iruby-add-process-buffer (process buffer)
  (setq iruby-process-buffers (cons (cons process buffer)
                                    iruby-process-buffers)))

(defun iruby-remove-process-buffer (whence)
  (let* ((proc (etypecase whence
                (process whence)
                (buffer (iruby-buffer-process whence))))
         (elt (assq proc iruby-process-buffers)))
    (when elt
      (setq iruby-process-buffers (delq elt iruby-process-buffers)))))

(defun iruby-drop-process ()
  ;; NB this function was defined originally for kill-buffer-hook,
  ;; and will operate only on the current buffer.
  ;;
  ;; This will not close the buffer, as it was designed to be called
  ;; when a buffer is being closed.
  ;;
  ;; For purposes of user interface support, closed processes will
  ;; generally remain in `iruby-process-buffers' until removed with this
  ;; function, via `kill-buffer-hook'.
  ;;
  ;; Once a buffer is removed from `iruby-process-buffers', it will
  ;; no longer be available via `iruby-read-process' and any interactive
  ;; forms calling the same.
  ;;
  (when (eq major-mode 'iruby-mode)
    (let* ((whence (current-buffer))
           (proc (iruby-buffer-process whence))
           (procbuff (when proc (iruby-process-buffer proc))))
      (when (and proc (eq procbuff whence))
        (iruby-close-process proc)
        (iruby-remove-process-buffer whence)
        (when (eq whence iruby-default-ruby-buffer)
          (iruby-remember-ruby-buffer
           (caar (cl-remove-if-not 'buffer-live-p
                                   iruby-process-buffers
                                   :key 'car))))
        ))))

(defun iruby-close-process (process)
  "Attempt to close the Ruby process provided as `process'

If `process' is in a run state, this function will call each of the
following functions, with a timeout `iruby-restart-timeout' in each
successive call.
- `processs-send-eof'
- `interrupt-process'
- `kill-process'

After each function has returned, the `process' will be checked to
determine the process' state. If the process is no longer in a run
state, `iruby-close-process' will then return.

If the process is still in a run state, then the next function will be
called.

If the process is in a run state after the last function in that
sequence, then a warning will be raised.

This function will not modify `iruby-process-buffers'. If a process is
closed interactively with `iruby-close-process', it may normally be
restarted using `iruby-restart-process'."
  (interactive
   (list (if (or current-prefix-arg (null iruby-buffer))
             (iruby-read-process "Close iRuby process: ")
           (iruby-buffer-process (iruby-get-prevailing-buffer t)))))
  ;; This function itself will not modify `iruby-process-buffers'.
  ;;
  ;; Thus, a process that has been closed via this function may still
  ;; be accessible to user interface forms such as `iruby-read-process'
  ;;
  ;; Any calling function may modify `iruby-process-buffers'. This is
  ;; currently performed with such as the following:
  ;;
  ;; - `iruby-restart-process' will modify `iruby-process-buffers',
  ;;   there removing the closed process and storing the newly
  ;;   initialized process such as to be accessible for later read forms.
  ;;
  ;; - When an `iruby-mode' buffer is closed and `kill-buffer-hook' has
  ;;   been normally configured to call this callback in that buffer,
  ;;   then `iruby-drop-process' will close the process and remove the
  ;;   process from `iruby-process-buffers'.
  ;;
  ;; By not removing the process from `iruby-process-buffers' here, this
  ;; should serve to ensure that a process closed in this function can be
  ;; normally restarted by the user, such as via `iruby-restart-process'
  (let ((buff (iruby-process-buffer process)))
    (cl-macrolet ((when-process (&body body)
                    (let ((change-state-form
                           `(when (iruby-process-running-p process)
                              (with-timeout (iruby-restart-timeout)
                                ,@body)))
                          (check-form
                           `(unless (iruby-process-running-p process)
                              (cl-return-from close))))
                      (cond
                        (iruby-threads-p
                         (let ((thr (make-symbol "%thread")))
                           `(let ((,thr (make-thread
                                         (lambda () ,change-state-form)
                                         "iruby-restart(close)")))
                              (thread-join ,thr)
                              ,check-form)))
                        (t `(progn ,change-state-form ,check-form))))))

      (with-current-buffer buff
        (cl-block close
          (when-process
           (process-send-eof process))
          (when-process
           ;; if process is still running, interrupt
           (interrupt-process process))
          (when-process
           ;; if process is still running, send a process kill signal
           (kill-process process))
          (when-process
           ;; else fail with warning
           ;;
           ;; In an Emacs without threads available, this might be
           ;; reached even if the process has closed.
           (warn "Unable to ensure process closed: %s" process)))
        ))))


(defun iruby-restart-process (process)
  ;; FIXME this is now not retaining the implementation
  "Restart an iRuby process

After the process is closed with `iruby-close-process', the process will
be restarted with the process' original process command.

This process buffer and comint input history will be retained for the
new process.  Some local variables will be reset for `iruby-mode'.

The iRuby buffer name used for the initial process will be retained for
the new process. The new process should thus be accessible under the
original buffer name, for the functions `iruby-read-process'
`iruby-read-process-interactive' and for evaluation in Ruby source
buffers.

This function may be called interactively, in which case the Ruby buffer
will be seleted by `iruby-read-process-interactive'"
  (interactive
   (iruby-read-process-interactive "Restart iRuby process: "))
  (let ((buff (iruby-process-buffer process))
        (cached-data (assq process iruby-process-buffers)))
    (with-current-buffer buff
      (let ((cmd (process-command process))
            (impl iruby-buffer-impl-name)
            (multibyte-p enable-multibyte-characters)
            (locals (cl-remove 'enable-multibyte-characters
                               (buffer-local-variables buff)
                               :key 'car :test 'eq))
            newbuff proc)
        (iruby-close-process process)
        ;; This calls `comint-exec' directly, without reinitilizing the
        ;; buffer as within `run-iruby-new'
        (with-iruby-process-environment ()
          (setq newbuff
                (comint-exec buff (process-name process)
                             (car cmd) nil (cdr cmd))
                proc (get-buffer-process newbuff)))
        (set-buffer newbuff)
        (let ((iruby-default-ruby-syntax (or iruby-ruby-syntax
                                             iruby-default-ruby-syntax)))
          (iruby-mode)
          ;; reset local variables after iruby-mode
          (dolist (bind locals)
            (set (car bind) (cdr bind)))
          (when multibyte-p
            (set-buffer-multibyte multibyte-p))
          (iruby-initialize-impl-bindings impl))
        (setf (car cached-data) proc)))))


(defun run-iruby-new (command &optional name)
  "Create a new inferior Ruby process in a new buffer.

COMMAND is the command to call. This value may be provided as a string
or as a list of a command name and literal arguments. If provdied as a
string, the string will be tokenized with `split-string-and-unquote'
when provdied to comint.

NAME will be used for creating a name for the buffer. If NAME is not
provided, the nondirectory part of the first element in COMMAND will be
used"
  (let* ((commandlist
          (cl-etypecase command
            (string (split-string-and-unquote command))
            (cons command)))
         (name (or name (file-name-nondirectory (car commandlist))))
         (buffer (current-buffer))
         process
         (buffer-name (generate-new-buffer-name (format "*%s*" name)))
         (process-environment process-environment))

    (with-iruby-process-environment ()
      (setq buffer
            (apply 'make-comint-in-buffer
                   name buffer-name
                   (car commandlist) nil (cdr commandlist))
            process (get-buffer-process buffer)))

    (set-process-sentinel process 'iruby-process-sentinel)
    (iruby-add-process-buffer process buffer)

    (set-buffer buffer)
    (iruby-mode) ;; may reset any buffer-local variables
    (setq iruby-buffer-impl-name name)
    (iruby-initialize-impl-bindings name)

    (iruby-remember-ruby-buffer buffer)

    ;; NB this returns a buffer object
    (iruby-switch-to-process (get-buffer-process buffer))
    ))


(defun run-iruby-or-pop-to-buffer (command &optional name buffer always)
  ;; NB used in
  ;; - `run-iruby'
  ;; - `iruby-console-run'
  (if (or always
          (not (and buffer
                    (buffer-live-p buffer)
                    (iruby-process-running-p buffer))))
      (run-iruby-new command name)
    (iruby-switch-to-process (iruby-buffer-process buffer))
    (unless (and (string= iruby-buffer-impl-name name)
                 (equal iruby-buffer-command command))
      (warn (concat "Found an iRuby buffer, but it was created using "
                    "a different command for %s. Previous: %S")
             iruby-buffer-impl-name
             iruby-buffer-command))))

(defun iruby-proc (&optional noerr)
  "Return the inferior Ruby process for the current buffer or project.

See also: `iruby-get-prevailing-buffer'"
  ;; NB this API uses buffers as a primary point of reference.
  ;;
  ;; Although Emacs process objects are the primary point of I/O here,
  ;; Emacs buffers may typically have a longer lifetime than Emacs
  ;; process objects
  (let ((buffer (if (eq major-mode 'iruby-mode)
                    (current-buffer)
                  (iruby-get-prevailing-buffer))))
    (or (iruby-buffer-process buffer)
        (unless noerr
          (error "No current iRuby process.")))))

;; These commands are added to the iruby-minor-mode keymap:

(defconst iruby-send-terminator "--iruby-%x-%x"
  "Template for irb here document terminator.
Must not contain ruby meta characters.")

(defun iruby-bounds-of-thing (kind)
  "A workaround for syntactic quirks of `bounds-of-thing-at-point'

This function may return nil, or a cons whose CAR and CDR represent the
bounds of a thing at point of type `kind', in the current buffer"
  (let* ((bounds (bounds-of-thing-at-point kind))
         (second (cdr bounds)))
    (when (consp second)
      (setf (cdr bounds) (car second)))
    bounds))


(defun iruby-send-string (proc str &optional file line)
  "Send the provided string for evaluation as a ruby expression
in the ruby process buffer.

FILE and LINE, if non-nil, will be provided respectively as the
file and line number values for source locations under 'eval' in
the ruby process.

If FILE is nil, the string value \"(Unknown)\" will be used. The
effective FILE value will then be provided to the Ruby process
within single quotes.

LINE, if non-nil, must represent an integer value. If LINE
is nil, the value zero will be used.

See also:
 `iruby-send-region',`iruby-send-definition',`iruby-send-block'
 `iruby-show-last-output'"
  (let ((term (format iruby-send-terminator (random)
                      (car (time-convert nil t)))))
    (save-excursion
      (save-restriction
        (let ((m (process-mark proc))
              (hdr
               ;; NB reuse 'term' as a header/footer marker for the
               ;; reply, and a key onto a callback form in `iruby-preoutput-filter'
               (format "eval <<'%s', %s , '%s', 0x%x;\n"
                       ;; FIXME needs test for nil impl binding expr
                       term (or iruby-impl-binding-expr "nil")
                       (or file "(Unknown)")
                       (or line 0)))
              (tlr
               (concat "\n" term "\n; puts \n")))
          (set-buffer (marker-buffer m))
          (goto-char m)
          (insert "\n")
          (set-marker m (point))
          (comint-send-string proc hdr)
          (comint-send-string proc str)
          (comint-send-string proc tlr)
          )))))

;; - test - expecting a string representation of a ruby symbol
;;
;; (iruby-send-string (iruby-proc) "def a; end")
;; (iruby-get-last-output)
;;
;;
;; - test - expecting a warning from the sub-ruby on redefinition of a
;;   constant
;;
;; (dotimes (n 2 nil) (iruby-send-string (iruby-proc) "module ABC; D=:EF; end"))
;; (iruby-get-last-output)
;;
;; ^ FIXME this second test may show a quirk in iruby-get-last-output.
;;   Here, iruby-get-last-output will capture the output after both of
;;   the expressions to the Ruby process, together with the prompt
;;   string conctatenated before the second expression. It may be a
;;   side effect of I/O synchronization with the ruby subprocess and
;;   Emacs ...


(defun iruby-print-result ()
  "Print the result displayed under the last evaluation in the ruby
subproess to the current buffer.

This function will add an initial newline after the position at `point'
then printing the text of the result within the comment syntax for the
current buffer. `point' will not be advacned by this function.

See also: `iruby-show-last-output', `iruby-get-last-output'"
  (interactive)
  (save-excursion
    (let ((start (point)))
      (newline)
      (princ (iruby-get-last-output (iruby-proc))
             (current-buffer))
      (comment-region start (point)))))


(defun iruby-preoutput-filter (output)
  "Pre-output hook function for iRuby process buffers

This function is ordinarily called in an iruby-mode buffer,
as feature of comint output processing. The value
of `output' will be the string that was received by comint.
The value returned by this function will be applied for
further output processing by comint, as the text to be
inserted to the iruby-mode buffer

See also: `comint-preoutput-filter-functions' [variable]"
  (let* ((match-start (string-match iruby-prompt-pattern output))
         ;; FIXME match-end 0 ... DNW  later, but 1 DNW here
         (match-end (when match-start (match-end 0))))
    (when match-start
      (put-text-property match-start match-end 'iruby-prompt t output))
    output))

(add-to-list 'text-property-default-nonsticky '(iruby-prompt . t))

(defun iruby-output-filter (output)
  "Post-output hook function for iRuby process buffers

This function is ordinarily called in an iruby-mode buffer,
after the next intput prompt has been displayed. The value
of `output' will be the string that was received by comint.

See also: `comint-output-filter-functions' [variable]"
  (when iruby-pending-input
    ;; ensure any pending input is carried over
    (insert iruby-pending-input)
    (setq iruby-pending-input nil)))


(defun iruby-send-region (start end &optional file-name line process)
  "Send a region of text from the current buffer to the ruby process.

When called interactively, this function operates on any region
in the current buffer.

If a FILE-NAME and LINE are provided, these will be sent to the
ruby process for association with the evaluated code. Otherwise,
the first non-nil value of `buffer-file-name' or the return value
of `buffer-name' will be used as the file name, with the line
number of point in the current buffer

If `iruby-show-last-output' is enabled and `iruby-threads-p' is
non-nil, this function will call `iruby-show-last-output' in
a separate thread, after a delay in seconds configured in
`iruby-output-wait'. The output value, `true' would indicate
successful load of the file, within the ruby process"
  (interactive
   (let ((st (point))
         (end (condition-case nil
                  (mark)
                (mark-inactive (point-max)))))
     (list* st end nil nil (iruby-read-process-interactive
                            "Send region to Ruby" t))))
 (let ((proc (or process (iruby-proc))))

    (save-excursion
      (save-restriction
        (iruby-send-string proc (buffer-substring-no-properties start end)
                           (or file-name buffer-file-name (buffer-name))
                           (or line (1+ (line-number-at-pos (min start end) t))))
        (when (and iruby-show-last-output iruby-threads-p)
          ;; NB using multithreading for async i/o and to allow for
          ;; update/check of variables available to the current thread
          (let* ((wsecs (or (and (numberp iruby-output-wait)
                                 (plusp iruby-output-wait)
                                 iruby-output-wait)
                            (default-value 'iruby-output-wait)))
                 (thr (make-thread
                       (cl-coerce `(lambda ()
                                     (sleep-for ,wsecs)
                                     (iruby-show-last-output ,proc)
                                    ;;; too much for a short ":[]="
                                    ;;; a whole output buffer ...
                                     ;;
                                     ;; (set-buffer (get-buffer-create "*frob*"))
                                     ;; (terpri)
                                     ;; (princ (iruby-get-last-output ,proc))
                                     )
                                  'function)
                       ;; thread name, e.g for the `list-threads' cmd
                       (format "iruby output monitor %s"
                               (mapconcat #'number-to-string (current-time) "-")))))
            ))))))

;; NB the Emacs debugger may not be activated under an error outside
;; of the Emacs main thread. Any "last error" from a (??) thread
;; may be read and cleared with:
;; (thread-last-error t)


(defun iruby-send-definition ()
  "Send the current definition to the inferior Ruby process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (ruby-beginning-of-defun)
      (iruby-send-region (point) end))))


(make-variable-buffer-local
 (defvar iruby-pending-input nil
   "If non-nil, an expresion to append after the next input prompt

This variable's value may be set during `iruby-send-last-sexp' and
will be handled in `iruby-output-filter'"))


(defun iruby-strip-pending-input (&optional buffer)
  "Remove and store any pending input in an iRuby buffer

When called interactively, the prevailing iRuby buffer will be used
unless with an interactive prefix argument, in which case the user will
be queried for a process buffer.

This function will store the pending input in `iruby-pending-input' such
that the latest value of this variable will be presented after the next
prompt is displayed in the iRuby buffer.

Returns the value of point at the end of the last prompt in the buffer,
subsequent of the trimmed input.

See also: `iruby-send-last-sexp'"
  (interactive (list (if interactive-prefix-arg
                         (iruby-read-process "Strip pending input for")
                       (iruby-get-prevailing-buffer))))
  (let ((use-buffer (or buffer (iruby-get-prevailing-buffer))))
    (with-current-buffer use-buffer
      (when buffer
        (unless (eq major-mode 'iruby-mode)
          (error "Not an iruby-mode buffer: %s" buffer)))
      (save-mark-and-excursion
        (save-restriction
          (let* ((buffer-end (point-max))
                 (prompt-end
                  (progn (iruby-input-start buffer-end)
                         (point))))
            (unless (= prompt-end buffer-end)
              (let ((input (buffer-substring-no-properties prompt-end buffer-end)))
                (cond
                  ((string-whitespace-p input)
                   (setq iruby-pending-input ""))
                  (t
                   ;; (warn "Staging %S" input)
                   (setq iruby-pending-input input)))
                (delete-region prompt-end buffer-end)))
            (point-max)))))))


(defun iruby-send-last-thing (thing point)
  "Send the previous thing at `point' to the inferior Ruby process.
The expression will be sent as new input in the process' comint buffer.

If point is within an expression, this will send the expression up to
its last point, as determined by `(bounds-of-thing-at-point thing)'

If an expression was being input in the comint buffer before this
function, the expression will be in effect moved to the new prompt
displayed after the evaluation has returned."
  ;; FIXME this does not handle partial input that has already been
  ;; partially sent
  (save-mark-and-excursion
      (let ((min (point-min))
            (cur point)
            bounds)
        (goto-char point)
        (cl-loop
           ;; ensure that there are bounds for an expression at point,
           ;; walking backwards by each character in the buffer,
           (cond
             ((setq bounds (iruby-bounds-of-thing thing))
              ;; found an expr
              (cl-return))
             ((= cur min) ;; beginning of buffer
              (setq bounds (cons min point))
              (cl-return))
             (t
              (backward-char)
              (setq cur (point)))))
        ;; send the substring for the expression,
        ;; ensuring that any pending input is stored
        (cl-destructuring-bind (start . end) bounds
          (let ((str (buffer-substring-no-properties start end))
                (buff (iruby-get-prevailing-buffer)))
            (with-current-buffer buff
              (goto-char (iruby-strip-pending-input buff))
              (insert str)
              (comint-send-input)))))))

(defun iruby-send-last-sexp (point)
  (interactive "d")
  (iruby-send-last-thing 'sexp point))

(defun iruby-send-last-definition (point)
  (interactive "d")
  (iruby-send-last-thing 'defun point))

(defun iruby-send-block (point)
  "Send the current block at POINT to the inferior Ruby process."
  (interactive "d")
  (save-excursion
    (goto-char point)
    (ruby-end-of-block)
    (end-of-line)
    (let ((end (point)))
      (ruby-beginning-of-block)
      (iruby-send-region (point) end))))


(defvar iruby-default-ruby-buffer nil
  "The last buffer we switched to `iruby' from.

This variable may be used as a default when `iruby-process' is nil in
the current buffer")


(defun iruby-remember-ruby-buffer (buffer)
  (setq iruby-default-ruby-buffer buffer))


(defun iruby-switch-to-inf (eob-p)
  "Switch to the ruby process buffer.
With argument, positions cursor at end of buffer.

This function updates the value of `iruby-default-ruby-buffer'

See also: `iruby-use-process'"
  (interactive "P")
  (let ((buffer (iruby-get-prevailing-buffer)))
    (if buffer
        (progn
          (pop-to-buffer buffer)
          (iruby-remember-ruby-buffer buffer))
      (error "Found no iRuby process")))
  (when eob-p
    (push-mark)
    (iruby-input-start (point-max))))


(defun iruby-switch-to-default-buffer ()
  "Switch to the default Ruby buffer.

See also: `iruby-use-process', `iruby-switch-to-process'"
  (interactive)
  (if (and iruby-default-ruby-buffer
           (buffer-live-p iruby-default-ruby-buffer))
      (pop-to-buffer iruby-default-ruby-buffer)
    (message "Default Ruby buffer is unavailable")))


(defun iruby-send-region-and-go (start end)
  "Send the current region to the inferior Ruby process.
Then switch to the process buffer."
  (interactive "r")
  (iruby-send-region start end)
  (iruby-switch-to-inf t))


(defun iruby-send-definition-and-go ()
  "Send the current definition to the inferior Ruby.
Then switch to the process buffer."
  (interactive)
  (iruby-send-definition)
  (iruby-switch-to-inf t))


(defun iruby-send-block-and-go ()
  "Send the current block to the inferior Ruby.
Then switch to the process buffer."
  (interactive)
  (iruby-send-block)
  (iruby-switch-to-inf t))


(defun iruby-load-file (file-name &optional process)
  "Load a Ruby file into the inferior Ruby process.

The interactive form will store history data in the variable
`iruby-load-file-history' when `iruby-load-file-hiistory-limit' is
non-zero.

The file will be loaded by the active Ruby process for the current
buffer, or that for the global iRuby environment. This proceses may be
selected previous to `iruby-load-file', with the command
`iruby-use-process'

When called interactively, a prefix numeric argument to this command
specifies that the user should be queried for a Ruby process to use in
loading the file."
  (interactive
   (let* ((file-name-history (mapcar #'car iruby-load-file-history))
          (comint-src
           (comint-get-source "Load Ruby file: " (car iruby-load-file-history)
                              ;; T because LOAD needs an exact name
                              (iruby-source-modes) t)))
     (unless (or (zerop iruby-load-file-history-limit)
                 (equal comint-src (car iruby-load-file-history)))
       (add-to-history 'iruby-load-file-history comint-src
                       iruby-load-file-history-limit))
     (cons (car comint-src)
           (iruby-read-process-interactive "Load file in ruby"))))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (let ((file (expand-file-name file-name))
        (proc (or process (iruby-proc))))
    (with-temp-buffer
      ;; FIXME move  the "loading" message into an output callback.
      ;; or use a fontified insert call in the buffer, here
      ;;
      ;; when specified here, it simply adds another line of input
      ;; to the external process
      (insert (format "STDERR.puts(%%q(## loading %s));" file))
      (insert (format "load(%%q(%s));" (iruby-escape-single-quoted file)))
      (iruby-send-region (point-min) (point-max) nil nil proc))))


(defun iruby-send-buffer ()
  "Send the current buffer to the inferior Ruby process."
  (interactive)
  (save-restriction
    (widen)
    (iruby-send-region (point-min) (point-max))))


(defun iruby-send-buffer-and-go ()
  "Send the current buffer to the inferior Ruby process.
Then switch to the process buffer."
  (interactive)
  (iruby-send-buffer)
  (iruby-switch-to-inf t))


(defun iruby-send-line ()
  "Send the current line to the inferior Ruby process."
  (interactive)
  (save-restriction
    (widen)
    (iruby-send-region (point-at-bol) (point-at-eol))))


(defun iruby-send-line-and-go ()
  "Send the current line to the inferior Ruby process.
Then switch to the process buffer."
  (interactive)
  (iruby-send-line)
  (iruby-switch-to-inf t))


(defun iruby-escape-single-quoted (str)
  "Escape single quotes, double quotes and newlines in STR."
  (replace-regexp-in-string "'" "\\\\'"
    (replace-regexp-in-string "\n" "\\\\n"
      (replace-regexp-in-string "\\\\" "\\\\\\\\" str))))


(defun iruby-completions (prefix)
  "Return a list of completions for the Ruby expression starting with EXPR."
  ;; NB for `iruby-completion-at-point' under buffer's
  ;; `completion-at-point-functions'
  (let* ((proc (iruby-proc))
         (line (buffer-substring
                (save-excursion (move-beginning-of-line 1)
                                (point))
                (point)))
         (expr (iruby-completion-expr-at-point))
         (prefix-offset (- (length expr) (length prefix)))
         (previous-filter (process-filter proc))
         (kept "")
         completions)
    ;;;; (warn "Completions. prefix %S expr %S" prefix expr)

    (unwind-protect
         (progn
           (set-process-filter proc
                               (lambda (proc string)
                                 (setq kept (concat kept string))
                                 ;; ensure that the string is not displayed:
                                 nil))
           (let ((completion-snippet
                  (format
                   (concat
                    iruby-impl-completion-expr
                    "; nil;\n"

                    ;; FIXME  The following needs tests under impl-specific
                    ;; :completion exprs
                    ;;
                    ;; supporting only IRB/Ruby, for now

                    ;; "proc { |expr, line|"
                    ;; "  require 'ostruct';"
                    ;; "  old_wp = defined?(Bond) && Bond.started? && Bond.agent.weapon;"
                    ;; "  begin"
                    ;; "    Bond.agent.instance_variable_set('@weapon',"
                    ;; "      OpenStruct.new(:line_buffer => line)) if old_wp;"
                    ;; "    if defined?(_pry_.complete) then"
                    ;; "      puts _pry_.complete(expr)"
                    ;; "    elsif defined?(pry_instance.complete) then"
                    ;; "      puts pry_instance.complete(expr)"
                    ;; "    else"
                    ;; "      completer = if defined?(_pry_) then"
                    ;; "        Pry.config.completer.build_completion_proc(binding, _pry_)"
                    ;; "      elsif old_wp then"
                    ;; "        Bond.agent"
                    ;; "      elsif defined?(IRB::InputCompletor::CompletionProc) then"
                    ;; "        IRB::InputCompletor::CompletionProc"
                    ;; "      end and puts completer.call(expr).compact"
                    ;; "    end"
                    ;; "  ensure"
                    ;; "    Bond.agent.instance_variable_set('@weapon', old_wp) if old_wp "
                    ;; "  end "
                    ;; "}.call('%s', '%s')\n"

                    )
                   (iruby-escape-single-quoted expr)
                   (iruby-escape-single-quoted line))))
            ;;; (warn "in completions section. using %s" completion-snippet)
             (process-send-string proc completion-snippet)
             (while (and (not (string-match iruby-prompt-pattern kept))
                         ;; how now :: ?
                         (accept-process-output proc 2)))
             (setq completions (butlast (split-string kept "\r?\n") 2))
             ;; Subprocess echoes output on Windows and OS X.
             (when (and completions (string= (concat (car completions) "\n") completion-snippet))
               (setq completions (cdr completions)))))
      ;; ensure:
      (set-process-filter proc previous-filter))

    ;;; (warn "At end, kept: %S" kept)
    ;;; (warn "At end, completions: %S" completions)
    (mapcar
     (lambda (str)
       (substring str prefix-offset))
     completions)))


(defconst iruby-ruby-expr-break-chars " \t\n\"\'`><,;|&{(")

(defun iruby-completion-bounds-of-prefix ()
  "Return bounds of expression at point to complete."
  (let ((iruby-ruby-expr-break-chars ;; NB this is the constant's sole usage
         (concat iruby-ruby-expr-break-chars ".")))
    (iruby-completion-bounds-of-expr-at-point)))


(defun iruby-completion-bounds-of-expr-at-point ()
  "Return bounds of expression at point to complete."
  (let ((s (char-syntax (following-char))))
    (save-excursion
      (when (eq s ?.)
        (backward-char))
      (let ((bounds (iruby-bounds-of-thing 'sexp)))
        ;; (warn "Bounds %S" bounds)
        (when (and (eq s ?.) bounds)
          ;; when completing at punctuation
          (let ((max (point-max))
                (end (cdr bounds)))
            (unless (= max end)
              (setf (cdr bounds) (1+ end)))))
        bounds))))


(defun iruby-completion-expr-at-point ()
  "Return expression at point to complete."
  (let ((bounds (iruby-completion-bounds-of-expr-at-point)))
    (and bounds
         (buffer-substring-no-properties (car bounds) (cdr bounds)))))



(defun iruby-completion-at-point ()
  "Retrieve the list of completions and prompt the user.
Returns the selected completion or nil."
  (cond
    (iruby-impl-completion-expr
     (let ((bounds (iruby-completion-bounds-of-prefix)))
       (when bounds
         (list (car bounds) (cdr bounds)
               (if (fboundp 'completion-table-with-cache)
                   (completion-table-with-cache #'iruby-completions)
                 (completion-table-dynamic #'iruby-completions))))))
    (t
     (iruby-warn-once "Completion not configured for implementation %s"
                      iruby-buffer-impl-name))))



(defvar iruby-orig-compilation-mode nil
  "Original compilation mode before switching to `iruby-mode'.")

(defvar iruby-orig-process-filter nil
  "Original process filter before switching to `iruby-mode'.")

(defvar iruby-orig-error-regexp-alist nil
  "Original `compilation-error-regexp-alist' before switching to `iruby-mode.'")


(defun iruby-switch-from-compilation ()
  "Make the buffer writable and switch to `iruby-mode'.
Recommended for use when the program being executed enters
interactive mode, i.e. hits a debugger breakpoint."
  (interactive)
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (let ((mode major-mode)
        (arguments compilation-arguments)
        (orig-mode-line-process mode-line-process)
        (orig-error-alist compilation-error-regexp-alist))
    (iruby-mode)
    (make-local-variable 'iruby-orig-compilation-mode)
    (setq iruby-orig-compilation-mode mode)
    (set (make-local-variable 'compilation-arguments) arguments)
    (set (make-local-variable 'iruby-orig-error-regexp-alist)
         orig-error-alist)
    (when orig-mode-line-process
      (setq mode-line-process orig-mode-line-process)))
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (make-local-variable 'iruby-orig-process-filter)
      (setq iruby-orig-process-filter (process-filter proc))
      (set-process-filter proc 'comint-output-filter))
    (when (looking-back iruby-prompt-pattern (line-beginning-position))
      (let ((line (match-string 0)))
        (delete-region (match-beginning 0) (point))
        (comint-output-filter proc line)))))

(defun iruby-maybe-switch-to-compilation ()
  "Switch to compilation mode this buffer was in before
`iruby-switch-from-compilation' was called, if it was.
Otherwise, just toggle read-only status."
  (interactive)
  (if iruby-orig-compilation-mode
      (let ((orig-mode-line-process mode-line-process)
            (proc (get-buffer-process (current-buffer)))
            (arguments compilation-arguments)
            (filter iruby-orig-process-filter)
            (errors iruby-orig-error-regexp-alist))
        (unwind-protect
             (funcall iruby-orig-compilation-mode)
          (setq mode-line-process orig-mode-line-process)
          (set (make-local-variable 'compilation-arguments) arguments)
          (set (make-local-variable 'compilation-error-regexp-alist) errors)
          (when proc
            (set-process-filter proc filter))))
    (toggle-read-only)))

;;;###autoload
(defun iruby-switch-setup ()
  "Modify `rspec-compilation-mode' and `ruby-compilation-mode'
keymaps to bind `iruby-switch-from-compilation' to `С-x C-q'."
  (eval-after-load 'rspec-mode
    '(define-key rspec-compilation-mode-map (kbd "C-x C-q")
       'iruby-switch-from-compilation))
  (eval-after-load 'ruby-compilation
    ;; NB available in the rinari src tree, or separately via melpa/...
    '(define-key ruby-compilation-mode-map (kbd "C-x C-q")
       'iruby-switch-from-compilation))
  (eval-after-load 'projectile-rails
    '(define-key projectile-rails-server-mode-map (kbd "C-x C-q")
       'iruby-switch-from-compilation)))

(defvar iruby-console-patterns-alist
  '((".zeus.sock" . zeus)
    (iruby-console-rails-p . rails)
    (iruby-console-hanami-p . hanami)
    (iruby-console-script-p . script)
    ("*.gemspec" . gem)
    (iruby-console-racksh-p . racksh)
    ("Gemfile" . default))
  "Mapping from predicates (wildcard patterns or functions) to type symbols.
`iruby-console-auto' walks up from the current directory until
one of the predicates matches, then calls `iruby-console-TYPE',
passing it the found directory.")

(defvar iruby-breakpoint-pattern "\\(\\[1\\] pry(\\)\\|\\((rdb:1)\\)\\|\\((byebug)\\)"
  "Pattern found when a breakpoint is triggered in a compilation session.
This checks if the current line is a pry or ruby-debug prompt.")

(defun iruby-console-match (dir)
  "Find matching console command for DIR, if any."
  (catch 'type
    (dolist (pair iruby-console-patterns-alist)
      (let ((default-directory dir)
            (pred (car pair)))
        (when (if (stringp pred)
                  (file-expand-wildcards pred)
                (funcall pred))
          (throw 'type (cdr pair)))))))

;;;###autoload
(defun iruby-console-auto ()
  "Run the appropriate Ruby console command.
The command and the directory to run it from are detected
automatically."
  (interactive)
  (let* ((dir (locate-dominating-file default-directory
                                      #'iruby-console-match))
         (type (iruby-console-match dir))
         (fun (intern (format "iruby-console-%s" type))))
    (unless type (error "No matching directory found"))
    (funcall fun dir)))

(defun iruby-console-rails-p ()
  (and (file-exists-p "Gemfile.lock")
       (iruby-file-contents-match "Gemfile.lock" "^ +railties ")
       (file-exists-p "config/application.rb")
       (iruby-file-contents-match "config/application.rb"
                                     "\\_<Rails::Application\\_>")))

(defun iruby-console-read-directory (type)
  (or
   (let ((predicate (car (rassq type iruby-console-patterns-alist))))
     (locate-dominating-file (read-directory-name "" nil nil t)
                             (lambda (dir)
                               (let ((default-directory dir))
                                 (if (stringp predicate)
                                     (file-expand-wildcards predicate)
                                   (funcall predicate))))))
   (error "No matching directory for %s console found"
          (capitalize (symbol-name type)))))

(defun iruby-console-run (command name)
  (run-iruby-or-pop-to-buffer command name
                              (iruby-buffer-in-directory default-directory)))

;;;###autoload
(defun iruby-console-zeus (dir)
  "Run Rails console in DIR using Zeus."
  (interactive (list (iruby-console-read-directory 'zeus)))
  (let ((default-directory (file-name-as-directory dir))
        (exec-prefix (if (executable-find "zeus") "" "bundle exec ")))
    (iruby-console-run (concat exec-prefix "zeus console") "zeus")))

;;;###autoload
(defun iruby-console-rails (dir)
  "Run Rails console in DIR."
  (interactive (list (iruby-console-read-directory 'rails)))
  (let* ((default-directory (file-name-as-directory dir))
         (env (iruby-console-rails-env))
         (with-bundler (file-exists-p "Gemfile")))
    (iruby-console-run
     (concat (when with-bundler "bundle exec ")
             "rails console -e "
             env
             ;; Note: this only has effect in Rails < 5.0 or >= 5.1.4
             ;; https://github.com/rails/rails/pull/29010
             (when (irb-needs-nomultiline-p)
               " -- --nomultiline"))
     "rails")))

(defun iruby-console-rails-env ()
  (if (stringp iruby-console-environment)
      iruby-console-environment
    (let ((envs (iruby-console-rails-envs)))
      (completing-read "Rails environment: "
                       envs
                       nil t
                       nil nil (car (member "development" envs))))))

(defun iruby-console-rails-envs ()
  (let ((files (file-expand-wildcards "config/environments/*.rb")))
    (if (null files)
        (error "No files in %s" (expand-file-name "config/environments/"))
      (mapcar #'file-name-base files))))

(defun iruby-console-hanami-p ()
  (and (file-exists-p "config.ru")
       (iruby-file-contents-match "config.ru" "\\_<run Hanami.app\\_>")))

(defun iruby-console-hanami (dir)
  "Run Hanami console in DIR."
  (interactive (list (iruby-console-read-directory 'hanami)))
  (let* ((default-directory (file-name-as-directory dir))
         (env (iruby-console-hanami-env))
         (with-bundler (file-exists-p "Gemfile"))
         (process-environment (cons (format "HANAMI_ENV=%s" env)
                                    process-environment)))
    (iruby-console-run
     (concat (when with-bundler "bundle exec ")
             "hanami console")
     "hanami")))

(defun iruby-console-hanami-env ()
  (if (stringp iruby-console-environment)
      iruby-console-environment
    (let ((envs '("development" "test" "production")))
      (completing-read "Hanami environment: "
                       envs
                       nil t
                       nil nil (car (member "development" envs))))))

;;;###autoload
(defun iruby-console-gem (dir)
  "Run IRB console for the gem in DIR.
The main module should be loaded automatically.  If DIR contains a
Gemfile, it should use the `gemspec' instruction."
  (interactive (list (iruby-console-read-directory 'gem)))
  (let* ((default-directory (file-name-as-directory dir))
         (gemspec (car (file-expand-wildcards "*.gemspec")))
         (base-command
          (if (file-exists-p "Gemfile")
              (if (iruby-file-contents-match gemspec "\\$LOAD_PATH")
                  "bundle exec irb"
                "bundle exec irb -I lib")
            "irb -I lib"))
         (name (iruby-file-contents-match
                gemspec "\\.name[ \t]*=[ \t]*['\"]\\([^'\"]+\\)['\"]" 1))
         args files)
    (unless (file-exists-p "lib")
      (error "The directory must contain a 'lib' subdirectory"))
    (let ((feature (and name (replace-regexp-in-string "-" "/" name))))
      (if (and feature (file-exists-p (concat "lib/" feature ".rb")))
          ;; There exists the main file corresponding to the gem name,
          ;; let's require it.
          (setq args (concat " -r " feature))
        ;; Let's require all non-directory files under lib, instead.
        (dolist (item (directory-files "lib"))
          (when (and (not (file-directory-p (format "lib/%s" item)))
                     (string-match-p "\\.rb\\'" item))
            (push item files)))
        (setq args
              (mapconcat
               (lambda (file)
                 (concat " -r " (file-name-sans-extension file)))
               files
               ""))))
    (when (irb-needs-nomultiline-p)
      (setq base-command (concat base-command " --nomultiline")))
    (iruby-console-run
     (concat base-command args
             " --prompt default --noreadline -r irb/completion")
     "gem")))

(defun iruby-console-racksh-p ()
  (and (file-exists-p "Gemfile.lock")
       (iruby-file-contents-match "Gemfile.lock" "^ +racksh ")))

(defun iruby-console-racksh (dir)
  "Run racksh in DIR."
  (interactive (list (iruby-console-read-directory 'racksh)))
  (let ((default-directory (file-name-as-directory dir)))
    (iruby-console-run "bundle exec racksh" "racksh")))

(defun iruby-in-ruby-compilation-modes (mode)
  "Check if MODE is a Ruby compilation mode."
  (member mode '(rspec-compilation-mode
                 ruby-compilation-mode
                 projectile-rails-server-mode
                 minitest-compilation-mode)))

;;;###autoload
(defun iruby-auto-enter ()
  "Switch to `iruby-mode' if the breakpoint pattern matches the current line."
  (when (and (iruby-in-ruby-compilation-modes major-mode)
             (save-excursion
               (beginning-of-line)
               (re-search-forward iruby-breakpoint-pattern nil t)))
    ;; Exiting excursion before this call to get the prompt fontified.
    (iruby-switch-from-compilation)
    (add-hook 'comint-input-filter-functions 'iruby-auto-exit nil t)))

;;;###autoload
(defun iruby-auto-exit (input)
  "Return to the previous compilation mode if INPUT is a debugger exit command."
  (when (iruby-in-ruby-compilation-modes iruby-orig-compilation-mode)
    (if (member input '("quit\n" "exit\n" ""))
        ;; After the current command completes, otherwise we get a
        ;; marker error.
        (run-with-idle-timer 0 nil #'iruby-maybe-switch-to-compilation))))

(defun iruby-enable-auto-breakpoint ()
  (interactive)
  (add-hook 'compilation-filter-hook 'iruby-auto-enter))

(defun iruby-disable-auto-breakpoint ()
  (interactive)
  (remove-hook 'compilation-filter-hook 'iruby-auto-enter))

(defun iruby-console-script-p ()
  (and (file-exists-p "Gemfile.lock")
       (or
        (file-exists-p "bin/console")
        (file-exists-p "console")
        (file-exists-p "console.rb"))))

;;;###autoload
(defun iruby-console-script (dir)
  "Run custom bin/console, console or console.rb in DIR."
  (interactive (list (iruby-console-read-directory 'script)))
  (let ((default-directory (file-name-as-directory dir)))
    (cond
     ((file-exists-p "bin/console")
      (iruby-console-run "bundle exec bin/console" "bin/console"))
     ((file-exists-p "console.rb")
      (iruby-console-run "bundle exec ruby console.rb" "console.rb"))
     ((file-exists-p "console")
      (iruby-console-run "bundle exec console" "console.rb")))))

;;;###autoload
(defun iruby-console-default (dir)
  "Run Pry, or bundle console, in DIR."
  (interactive (list (iruby-console-read-directory 'default)))
  (let ((default-directory (file-name-as-directory dir)))
    (unless (file-exists-p "Gemfile")
      (error "The directory must contain a Gemfile"))
    (cond
     ((iruby-file-contents-match "Gemfile" "[\"']pry[\"']")
      (iruby-console-run "bundle exec pry" "pry"))
     (t
      (iruby-console-run "bundle console" "bundle console")))))

;;;###autoload
(defun iruby-file-contents-match (file regexp &optional match-group)
  (with-temp-buffer
    (insert-file-contents file)
    (when (re-search-forward regexp nil t)
      (if match-group
          (match-string match-group)
        t))))

(defun iruby-smie--forward-token ()
  (let ((inhibit-field-text-motion t))
    (ruby-smie--forward-token)))

(defun iruby-smie--backward-token ()
  (let ((inhibit-field-text-motion t))
    (ruby-smie--backward-token)))


;; desktop integration

;; FIXME move this section into iruby-desktop.el

(make-variable-buffer-local
 (defvar iruby-mapped-buffer-name nil
   "If non-nil, a buffer-name.

During `desktop-save', this variable will be ignored by
`iruby-get-mapped-buffer-name', in lieu of the buffer's current
buffer name at time of `desktop-save'

During `desktop-read', this variable's value will be restored by
`iruby-restore-mapped-buffer-name', ovewriting any previous setting for
this variable in the restored buffer. Subsequently, any stored desktop
data requiring a reference to the original buffer-name may operate on
the value of this variable, as restored local to the buffer.

These functions will be added to `desktop-var-serdes-funs' by
`iruby-ensure-desktop-support' such that this local variable
will be available to `iruby-map-desktop-process-buffers' as run from
`desktop-after-read-hook'.

This feature is supported with desktop.el session files of a version
208 or newer.

See also: `iruby-mapped-source-buffers'"))

(defun iruby-get-mapped-buffer-name (previous)
  "If the current buffer has a non-nil value of `iruby-buffer' and is
not an `iruby-mode' buffer, return the buffer's current buffer name.

This function is used for iRuby desktop support.

See also: `iruby-mapped-buffer-name'"
  (unless (or (eq major-mode 'iruby-mode)
              (null iruby-buffer))
    (buffer-name)))

(defun iruby-restore-mapped-buffer-name (stored)
  "If the value of `stored' is non-nil, set the value of
`iruby-mapped-buffer-name' in the current buffer, using the value of
`stored'

This function is used for iRuby desktop support.

See also: `iruby-mapped-buffer-name'"
  (when stored
    (setq iruby-mapped-buffer-name stored)))



(make-variable-buffer-local
 (defvar iruby-mapped-source-buffers nil
   "Temporary storage for use with `iruby-restore-desktop-buffer' under
`desktop-read' and `desktop-save'

If non-nil, a list of buffer names assumed to have been mapped to the
current buffer via `iruby-buffer' in each named buffer, at time of
`desktop-save'.

This variable would normally be set as local to an `iruby-mode' process
buffer during `iruby-restore-desktop-buffer', such that the value may
then be processed after all buffers have been restored, using
`iruby-map-desktop-process-buffers'. The latter function would normally
be called as a callback from the hook variable `desktop-after-read-hook'.

This external mapping procedure is used for ensuring persistent buffer
references between `desktop-save' and `desktop-restore', for iRuby
desktop hooks.

Once the `desktop-save' and `destkop-restore' hooks have been configured
for this support, this may serve to ensure that for each buffer that was
mapped to a specific iRuby process buffer during `desktop-save', the
mapping should be restored after a later `desktop-restore'.

See also: `iruby-mapped-buffer-name', `iruby-ensure-desktop-support'"))

(make-variable-buffer-local
 (defvar iruby-mapped-misc-data nil
   "Associative list for data to `iruby-map-desktop-process-buffers'"))


(defun iruby-restore-desktop-buffer (file name data)
  "Callback function for iRuby support in `desktop-read'

This function's name will normally be stored for the `iruby-mode' entry
under the global `desktop-buffer-mode-handlers' list.

See also: `iruby-ensure-desktop-support'; `iruby-desktop-misc-data'"
  (let* ((cmd (or (cdr (assq 'iruby-buffer-command desktop-buffer-locals))
                  (cdr (assq :cmd data))
                  (progn
                    (warn "no iruby-buffer-command saved in desktop data for %s. \
Using current defaults for %s" name iruby-default-implementation )
                    (iruby-build-impl-cmd iruby-default-implementation))))
        (impl (or (cdr (assq 'iruby-buffer-impl-name desktop-buffer-locals))
                  (cdr (assq :impl data))
                  ;; FIXME this would not retrieve the implementation
                  ;; name if the ruby impl. command is prefixed or
                  ;; suffixed with a version specifier
                  (progn
                    (warn "No implementation stored for %s" name)
                    (file-name-nondiredtory cmd))))
        (dir (or (cdr (assq 'default-directory desktop-buffer-locals))
                 (cdr (assq :dir data))
                 default-directory))
        (default-p (cdr (assq :default-p data)))
        (mapped (or (cdr (assq :mapped data))
                    (cdr (assq 'iruby-mapped-source-buffers desktop-buffer-locals)))))

    (unless (boundp 'erm-full-parse-p)
      ;; FIXME this is a hack for enh-ruby-mode, such that may err
      ;; during desktop restore, on an unbound variable ...
      (make-variable-buffer-local 'erm-full-parse-p)
      (setq-default erm-full-parse-p nil))

    ;; NB seems to be run only once per original process buffer
    (with-temp-buffer
      ;; FIXME does not set the dir of the process - such that Dir.pwd
      ;; would use - during desktop-read, only sets the default-directory
      ;; in the emacs buffer ... will have to Dir.chdir in the process.
      ;;
      ;; TBD desktop-read may have localized default-directory when this
      ;; is called, to some side effect as such? or may it be a side
      ;; effect of with-temp-buffer?
      (setq default-directory dir)
      (let* ((procbuff (run-iruby-new cmd impl))
             (proc (get-buffer-process procbuff))
             (exp-dir (expand-file-name dir)))
        (when mapped
          (with-current-buffer procbuff
            ;; store buffer mapping data for the callback to
            ;; `iruby-map-desktop-process-buffers'
            ;; from `desktop-after-read-hook'
            (setq iruby-mapped-source-buffers mapped)))
        (when default-p
          (push '(:default-p . t)  iruby-mapped-misc-data))
        (iruby-send-string proc
                           (format "puts(%%q(# iRuby chdir to %s))" dir))
        (iruby-send-string proc
                           (format "Dir.chdir(%%q(%s))" exp-dir))))))

(defun iruby-desktop-misc-data (deskdir)
  "Callback function for `desktop-save' under iRuby process buffers

This function's name will usually be set as the value of the buffer-local
variable `desktop-save-buffer' in any iRuby process buffer.

This function returns data to be stored during `desktop-save' for later
initialization of an iRuby process buffer as during `desktop-read'. It's
assumed that this function will be called with an iRuby process
buffer as the current buffer, during `desktop-save'.

This function returns an associative list representing a mapping for
the following values in the current iruby-mode buffer:
- :impl => `iruby-buffer-impl-name', needed for later restoring
   any implementation-specific bindings under `iruby-initialize-impl-bindings'
- :cmd => `iruby-buffer-command' i.e for the original process
- :dir => `default-directory' for the buffer, in Emacs
- :mapped => list of buffer names, for buffers where `iruby-buffer' has
   been set as eq to the current buffer
- :default-p => present if true, indicating that the buffer was in use
   as the `iruby-default-ruby-buffer'

See also:
 `iruby-map-desktop-process-buffers',
 `iruby-ensure-desktop-support'"
  (let* ((self (current-buffer))
         (mapped (list nil))
         (default-p (eq self iruby-default-ruby-buffer))
         (last-mapped mapped)
         new-last)

    (dolist (srcbuff (buffer-list))
      (unless (eq srcbuff self)
        (with-current-buffer srcbuff
          (when (eq iruby-buffer self)
            ;; adds new values to the last cons cell in 'mapped'
            ;; without iterating on the list, under every iteration here
            (setf new-last (cons (buffer-name srcbuff) nil)
                  (cdr last-mapped) new-last
                  last-mapped new-last)))))

    (append (list (cons :impl iruby-buffer-impl-name)
                  (cons :cmd iruby-buffer-command)
                  (cons :dir default-directory)
                  (cons :mapped (cdr mapped)))
            ;; optional data
            (when default-p
              (list (cons :default-p t))))))

(defun iruby-map-desktop-process-buffers ()
  "Ensure that each  buffer stored under `desktop-save' will be re-bound
to any `iruby-buffer' that was in use with the source buffer, at time of
desktop-save.

This function is interoperable with `iruby-desktop-misc-data' and
`iruby-restore-desktop-buffer' under iRuby configuration for
`desktop-save' and `desktop-read'.

For' activation after `desktop-read', This function's name should
normally be present in `desktop-after-read-hook'. The function
`iruby-ensure-desktop-support' will configure that hook and other hooks
needed for iRuby desktop session support."
  (let ((mapped (list nil))
        (buffers (buffer-list)))
    (cl-labels ((find-buffer-for (ref-name)
                  (cl-dolist (b buffers)
                    (with-current-buffer b
                      (when (and iruby-mapped-buffer-name
                                 (equal ref-name
                                        iruby-mapped-buffer-name))
                        (cl-return b))))))
      (dolist (current iruby-process-buffers (cdr mapped))
        (cl-destructuring-bind (proc . procbuff) current
          (with-current-buffer procbuff
            (when (cdr (assq :default-p iruby-mapped-misc-data))
              (setq iruby-default-ruby-buffer procbuff))
            ;; iterate on the local value of `iruby-mapped-source-buffers'
            (dolist (mapped-name iruby-mapped-source-buffers)
              (let ((srcbuff (find-buffer-for mapped-name)))
                (cond
                  (srcbuff
                   (setf (cdr (last mapped)) (cons srcbuff nil))
                   (with-current-buffer srcbuff
                     (unless iruby-buffer
                       (setq iruby-buffer procbuff))))
                  (t
                   (warn "Buffer unavailable for iRuby process mapping: %s" mapped-name))
                  )))))))))

(defun iruby-ensure-desktop-support ()
  "Ensure desktop.el will be configured for restoring `iruby-mode' buffers

This function should normally be called from a user init file, after
desktop.el is loaded and before calling either `desktop-save' or
`desktop-read'. At the user's discretion, this may be accomplished with
autoloads, eval after load, and/or direct `require' calls.

This function adds callback functions to `desktop-buffer-mode-handlers',
`desktop-after-read-hook', and `desktop-var-serdes-funs' for
enduring that each `iruby-mode' process buffer may be stored under
`desktop-save' and restored under `desktop-read'. Furthermore, these
callbacks should serve to ensure that any buffer that was bound to a
specific `iruby-buffer' will be configured to use a corresponding
iruby process  buffer initialized during `desktop-read'.

Additional integration is provided in `run-iruby-new', which will store
a corresponding value for `desktop-save-buffer' in each new iruby-mode
buffer, such that the new `iruby-mode' buffer should be stored under
`desktop-save'

See also: `desktop-save', `desktop-read'"
  (interactive)
  (cond
    ((featurep 'desktop)
     (unless (assq 'iruby-mode desktop-buffer-mode-handlers)
       (setq desktop-buffer-mode-handlers
             (cons (cons 'iruby-mode 'iruby-restore-desktop-buffer)
                   desktop-buffer-mode-handlers)))
     ;; NB cannot store a buffer object in a desktop file, i.e cannot
     ;; store the value of `iruby-buffer', per se. There are
     ;; workarounds however, furthermore that may serve to support a
     ;; consistency for buffer name references within a desktop file.
     (cl-pushnew 'iruby-map-desktop-process-buffers desktop-after-read-hook
                 :test #'eq)
     ;; second part of a workaround:
     (unless (assq 'iruby-mapped-buffer-name desktop-var-serdes-funs)
       (setq desktop-var-serdes-funs
             (cons (list 'iruby-mapped-buffer-name
                         'iruby-get-mapped-buffer-name
                         'iruby-restore-mapped-buffer-name)
                   desktop-var-serdes-funs))))
    (t
     (let ((elt (assq 'desktop after-load-alist))
           (form  (lambda ()
                    ;; call again, after the desktop lib is loaded
                    (iruby-ensure-desktop-support))))
       (cond
         (elt (push (list 'desktop form) (cdr elt)))
         (t (push (list 'desktop form) after-load-alist))
       )))))

;;;; subsq, in user init files e.g (simplest approach)
;; (require 'desktop)
;; (require 'iruby)
;; (iruby-ensure-desktop-support)

;;;###autoload (dolist (mode (iruby-source-modes)) (add-hook (intern (format "%s-hook" mode)) 'iruby-minor-mode))

(provide 'iruby)
;;; iruby.el ends here
