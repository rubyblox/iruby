;; iruby.el --- Run a Ruby process in a buffer (a fork on inf-ruby)

;; Copyright (C) 1999-2008 Yukihiro Matsumoto, Nobuyoshi Nakada

;; Author: Yukihiro Matsumoto
;;         Nobuyoshi Nakada
;;         Cornelius Mika <cornelius.mika@gmail.com>
;;         Dmitry Gutov <dgutov@yandex.ru>
;;         Kyle Hargraves <pd@krh.me>
;;         Sean Champ <spchamp@users.noreply.github.com>
;; URL: http://github.com/nonsequitur/inf-ruby
;; Created: 8 April 1998
;; Keywords: languages ruby
;; Version: 2.6.2

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

(defgroup iruby-ui nil
  "iRuby user interface support"
  :group 'iruby)

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
  '(("erm" enh-ruby-mode enh-ruby-mode-syntax-table)
    ("ruby-mode" ruby-mode ruby-mode-syntax-table))
  "Definitions for `iruby-ruby-syntax-table'

Each element in this associative list must have the following syntax:
  (<name> <feature> <table-var>)
where <name> is a string name and <feature> and <table-var> are
symbols. The <feature> symbol denotes a feature that can be accessed
via `require' in Emacs, to ensure that the denoted <table-var> will be
defined. The <table-var> must denote a variable name providing an Emacs
syntax table. See also: `make-syntax-table', `modify-syntax-entry'

One entry from this list may be selected with the customization option,
`iruby-ruby-syntax'. The corresponding syntax table will then be enabled
in new iRuby process buffers, such as with `iruby' and similar functions"
  :group 'iruby-language
  :type '(repeat (list :tag "Mode definition"
                  (string :tag "Local mode name")
                  (symbol :tag "Feature symbol")
                  (symbol :tag "Mode syntax table (variable)"))))

(defcustom iruby-ruby-syntax "erm"
  "Ruby language mode input completion support in iRuby buffers

This value must match a key value in the associative list,
`iruby-ruby-modes'"
  :group 'iruby-language
  :type `(choice ,@(mapcar (lambda (item) `(const ,(car item)))
                           iruby-ruby-modes)))




;; TBD
;; - define an eieio class for iRuby ipmlementation definitions
;;   - define automatic custom integr. for user-editable fields of such class
;; - integrate the same with the top-level iruby-implementations
;;   customization option, in its custom UI and in its usage below
;; - define one or both of the 'irb' and 'ruby' cases for IRB as
;;   subclasses of some other
;; - because ... UI ? and extensibility, e.g forms to evaluate
;;   after the ruby process is initialized, e.g IRB.conf[:SAVE_HISTORY]=false
;; - alternately, the following syntax

(defgroup iruby-impl nil
  "Ruby implementation support for iRuby"
  :group 'iruby)


(defcustom iruby-ruby-irb-prefix '("-r" "irb" "-r" "irb/completion"
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
  ;; If the implementation name in NAME does not have a prefix either
  ;; "irb" or "ruby", this will return the cons of NAME and REST-ARGS
  ;;
  ;; This function will accept e.g "irb27" or "ruby-dev" as an
  ;; implementation name, then returning a list of command line argument
  ;; values in a manner similar to the "irb" or "ruby" case,
  ;; respectively.
  ;;
  (let ((cmd-name name))
    (cond
      ((string-match "^irb" cmd-name)
       (cons cmd-name (cons (iruby-irb-rl-arg (cons cmd-name rest-args))
                            rest-args)))
      ((string-match "^ruby" cmd-name)
       ;; NB ensuring that any rest-args will appear after the args for
       ;; launching irb via ruby
       (let ((%prefix (cons cmd-name iruby-ruby-irb-prefix)))
         (append %prefix (list (iruby-irb-rl-arg %prefix))
                 rest-args)))
      (t (warn "Unknown irb implementation %s" name)
         (cons cmd-name rest-args)))))

(defun iruby-irb-rl-arg (cmdlist)
  (let* ((output (shell-command-to-string
                  (mapconcat 'identity (append cmdlist (list "--version"))
                             " ")))
         (fields (split-string output "[ (]"))
         (impl (car fields)))
    (unless (string= impl "irb")
      (error "Unknown irb implementation: %s" output))
    (cond
      ;; TBD --noreadline presumaby implies something different than
      ;; --nomultiline. Furthermore, the former might not appear in the
      ;; '--help' text for the supporting irb versions.
      ;;
      ;; This may assume that "--noreadline" would imply "--nomultiline"
      ;;
      ;; Maintained for purpose of portability
      ((version<= "1.2.0" (nth 1 fields)) "--nomultiline")
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

(defvar iruby-last-process-mark-point nil)
;; NB this variable may be removed in some subsequent version
(make-variable-buffer-local 'iruby-last-process-mark-point)


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

(defcustom iruby-output-wait 0.005
  "Number of seconds for asynchronous delay in `iruby-show-last-output'

This value will provide a delay before displaying the result of
the last evaluation under  `iruby-send-region', when both
`iruby-show-last-output' and `iruby-threads' are non-nil.

Under such a configuration, `iruby-send-region' will create a separate
thread such that will call `iruby-show-last-output' after this number of
seconds of delay. This may serve to ensure that the iruby process has
produced any complete output, before `iruby-show-last-output' is
evaluated.  Moreover, it may serve to ensusure that the environment in
which `iruby-show-last-output' is called will be the current environment
as subsequent of the evaluation of `iruby-send-region' in the main Emacs
thread.

This value should be a positive number."
  :group 'iruby)

(defvar iruby-threads (featurep 'threads)
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

(defvar iruby-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "C-c C-l") 'iruby-load-file)
    (define-key map (kbd "C-x C-e") 'iruby-send-last-sexp)
    (define-key map (kbd "TAB") 'completion-at-point)
    (define-key map (kbd "C-x C-q") 'iruby-maybe-switch-to-compilation)
    (define-key map (kbd "C-c C-z") 'iruby-switch-to-last-iruby-buffer)
    map)
  "Mode map for `iruby-mode'.")

;;;###autoload
(defvar iruby-source-modes '(ruby-mode enh-ruby-mode iruby-minor-mode)
  "Used to determine if a buffer contains Ruby source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a ruby source file by `iruby-load-file'.
Used by these commands to determine defaults.")

(defvar iruby-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last `iruby-load-file' command.
Used for determining the default in the
next one.")

(defvar iruby-last-prompt nil)
(make-variable-buffer-local 'iruby-last-prompt)

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

(defvar iruby-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x") 'iruby-send-definition)
    (define-key map (kbd "C-x C-e") 'iruby-send-last-sexp)
    (define-key map (kbd "C-c C-b") 'iruby-send-block)
    (define-key map (kbd "C-c M-b") 'iruby-send-block-and-go)
    (define-key map (kbd "C-c C-x") 'iruby-send-definition)
    (define-key map (kbd "C-c M-x") 'iruby-send-definition-and-go)
    (define-key map (kbd "C-c C-r") 'iruby-send-region)
    (define-key map (kbd "C-c M-r") 'iruby-send-region-and-go)
    (define-key map (kbd "C-c C-z") 'iruby-switch-to-inf)
    (define-key map (kbd "C-c C-l") 'iruby-load-file)
    (define-key map (kbd "C-c C-s") 'iruby)
    (easy-menu-define
      iruby-minor-mode-menu
      map
      "Inferior Ruby Minor Mode Menu"
      '("Iruby"
        ;; TODO: Add appropriate :active (or ENABLE) conditions.
        ["Send definition" iruby-send-definition t]
        ["Send last expression" iruby-send-last-sexp t]
        ["Send block" iruby-send-block t]
        ["Send region" iruby-send-region t]
        "--"
        ["Load file..." iruby-load-file t]
        "--"
        ["Start REPL" iruby t]
        ["Switch to REPL" iruby-switch-to-inf t]
        ))
    map))



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
  :lighter "" :keymap iruby-minor-mode-map)

(make-variable-buffer-local
 (defvar iruby-buffer nil
   "When set, the iruby process buffer to use for this buffer.

If unset, `iruby-last-ruby-buffer' may be used.

See also: `iruby-get-prevailing-buffer', `iruby-proc'"))

(defvar iruby-buffers nil "List of Ruby process buffers.")

(make-variable-buffer-local
 (defvar iruby-buffer-command nil "The command used to run Ruby shell"))

(defvar iruby-buffer-impl-name nil "The name of the Ruby shell")
(make-variable-buffer-local 'iruby-buffer-impl-name)

(define-derived-mode iruby-mode comint-mode "Iruby"
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
  (setq comint-prompt-regexp iruby-prompt-pattern)
  (ruby-mode-variables)
  (when (bound-and-true-p ruby-use-smie)
    (set (make-local-variable 'smie-forward-token-function)
         #'iruby-smie--forward-token)
    (set (make-local-variable 'smie-backward-token-function)
         #'iruby-smie--backward-token))
  (add-hook 'comint-output-filter-functions 'iruby-output-filter nil t)
  (setq comint-get-old-input 'iruby-get-old-input)
  (set (make-local-variable 'compilation-error-regexp-alist)
       iruby-error-regexp-alist)
  (set (make-local-variable 'comint-prompt-read-only) iruby-prompt-read-only)
  (when (eq system-type 'windows-nt)
    (setq comint-process-echoes t))
  (add-hook 'completion-at-point-functions 'iruby-completion-at-point nil t)
  (compilation-shell-minor-mode t))

(defun iruby-output-filter (output)
  "Check if the current prompt is a top-level prompt."
  (unless (zerop (length output))
    ;; TBD usage of iruby-last-prompt at present
    (setq iruby-last-prompt (car (last (split-string output "\n"))))))

;; adapted from replace-in-string in XEmacs (subr.el)
(defun iruby-remove-in-string (str regexp)
  "Remove all matches in STR for REGEXP and returns the new string."
  (let ((rtn-str "") (start 0) match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
            start (match-end 0)
            rtn-str (concat rtn-str (substring str prev-start match))))
    (concat rtn-str (substring str start))))


(defun output-at-point-p (point)
  "Scan the buffer at point, to determine if point is within an output
field. If called interactively, a message will be displayed in the
minibuffer, such as to indicate the result.

This is a debugging function and may be removed in some later release"
  (interactive "d")
  (let ((rslt (get-text-property point 'field)))
    (if (interactive-p)
        (message "%s field at point" rslt)
      (eq rslt 'output))))


(defun iruby-jump-to-last-nonblank-output (point)
  "move point backwards over output fields from any blank continued
input prompt

This function provides a utility for `iruby-jump-to-last-prompt'"
  (interactive "d")
  (let ((at point)
        start end
        match)
    (goto-char at)
    (when (setq match (text-property-search-backward 'field 'output t))
      (goto-char at) ;; reset after the property search moved point
      (setq start (prop-match-beginning match))
      (setq end (prop-match-end match))
      (when (string-whitespace-p (buffer-substring-no-properties start end))
        (iruby-jump-to-last-nonblank-output start)))))

(defun iruby-jump-to-last-prompt (point)
  "If POINT is within an input area, move point to the end of the
previous prompt preceding POINT, similarly to the beginning of the
input area.

If POINT is not within an input area, point will be moved into the
previous input area before point will be moved to the end of the
previous prompt.

This function assumes that the irb prompt is configured to not display
additional printable text during continued input.

See also: `iruby-jump-to-input-end', `iruby-input-at-point'"
  (interactive "d")
  (let ((at point)
        (start point)
        (end point)
        match)

    (unless (null (get-text-property at 'field))
      ;; scan backwards until the previous input field,
      ;; such that may be assumed to have a 'field'
      ;; text property of nil
      (cl-do ((%at at (1- %at)))
             ((or (zerop %at)
                  (null (get-text-property %at 'field)))
              (unless (zerop at)
                (setq at %at))))
      ;; NB this is subsequent of the cl-do expr
      (goto-char at))

    ;; scan backwards over any continued input prompts,
    ;; such that may have a text property value of
    ;; 'output' for the 'field' property, though not
    ;; reprsented as printable characters in the buffer.
    ;; i.e beginning at an input field, those previous
    ;; 'output' fields - if in fact represented as such,
    ;; outside of a text property search - would be
    ;; comprised entirely of whitespace. The primary
    ;; prompt string would be assumed to be non-blank.
    (iruby-jump-to-last-nonblank-output at)

    (when (setq match
                (text-property-search-backward 'field 'output t))
      ;; move point to the end of the previous prompt string,
      ;; assumed to be located as the first 'output' field before
      ;; any subsequent input field
      (goto-char (prop-match-end match)))))


(defun iruby-jump-to-input-end (point)
  "If POINT is within an input area, move point to the end of the input
area.

This function assumes that the irb prompt is configured to not display
additional printable text during continued input.

See also: `iruby-jump-to-last-prompt', `iruby-input-at-point'"
  ;; FIXME does not move point into an input area - that functionality
  ;; was not needed for how this is used in  `iruby-input-at-point'
  ;; and subsequently, `iruby-get-old-input'
  (interactive "d")
  (let*((at point)
        (start at)
        (end at)
        expr match)

    (goto-char at)

    (when (setq match (text-property-search-forward 'field 'output t))
      (goto-char at) ;; reset after the property search moved point
      (setq start (prop-match-beginning match))
      (setq end (prop-match-end match))
      (when (string-whitespace-p (buffer-substring-no-properties start end))
        (iruby-jump-to-input-end end)))

    (when (setq match
                (text-property-search-forward 'field 'output t))
      (goto-char (1- (prop-match-beginning match))))))


(defun iruby-input-at-point (point)
  "Retrieve any input at point

See also: `iruby-jump-to-last-prompt', `iruby-jump-to-input-end',
`iruby-get-old-input'"
  ;; (interactive "d")
  (save-excursion
    (save-restriction
      (let* ((at (point))
             (field (get-text-property at 'field))
             start end match expr)

        (cond
          ((eq field 'output)
           (when (setq match (text-property-search-forward 'field 'output t t))
             (goto-char (prop-match-beginning match))))
          (t
           (when (setq match (text-property-search-backward 'field 'output t))
             (goto-char (prop-match-end match)))))

        (iruby-jump-to-last-prompt (point))

        (setq start (point))

        (iruby-jump-to-input-end start)

        (setq end (point))

        (setq expr (buffer-substring-no-properties start end))
        ;; (when (interactive-p)
        ;;   (message "Input: %S" expr))
        expr
        ))))

(defun iruby-get-old-input ()
  "Return a string from some previous section of input history
in an iruby process buffer

This function is used for iruby process buffers as the value
of `comint-get-old-input'. Thus, this function may be used by
`comint-previous-input', and as such, may be used by
`comint-send-input', to determine some section of earlier
input to re-use under a certain condition of `point`.

This function may be called as when `point' is located
somewhere before the most recent `process-mark' in the
process buffer, such as when the user has activated
`comint-send-input' with `point' positioned somewhere
earlier in the input history.

Generally, `comint-send-input'  would be activated with
the carriage return key, in an iruby process buffer.

In summary, this function is a part of the automation for
`comint-send-input' in iruby process buffers.

This function assumes that the irb prompt is configured
to display a single non-blank region of text on each initial
input line, with no additional non-blank text displayed for
any line of continued input. This would be the configuration
of the irb prompt as available with the shell command

  irb --inf-ruby-mode

See also: `iruby-jump-to-process-mark',
`iruby-forward-output', `iruby-jump-to-last-prompt',
`iruby-jump-to-input-end', `iruby-get-input-at-point'"

  (save-excursion
    (save-restriction
      (let* ((at (point))
             (at-field  (get-text-property at 'field))
             start end next-end match expr)

        (cond
          ((eq at-field 'output)
           ;;; (warn "At output")
           ;; move point to the end of the output field,
           ;; e.g to the end of the prompt string
           (setq match
                 (text-property-search-forward 'field 'output t))
           (when match
             (goto-char (prop-match-end match))))
          ((eq at-field 'boundary)
           ;;; (warn "At boundary")
           ;; point is at the end of an input line
           ;;
           ;; move point to the end of the previous output
           (setq match
                 (text-property-search-backward 'field 'output t))
           (when match
             (goto-char (prop-match-end match))))
          (t ;; point is within an input region
           ))

        ;; for debugging:
        ;; (setq expr (iruby-input-at-point (point)))
        ;; expr

        ;; general usage:
        (iruby-input-at-point (point))
        ))))

(defun iruby-forward-output ()
  "Move point to the beginning of the next output section in the current
buffer. The output section may represent a prompt, a backtrace, a
printed representation of some previous evaluation, or any other output
displayed to the buffer by the ruby process.

See also: `iruby-jump-to-process-mark', `iruby-jump-to-last-output'"
  (interactive)
  (let ((match ;; NB the search may move point:
         (text-property-search-forward 'field 'output t t)))
    (when match
      (goto-char (prop-match-beginning match)))))


(defun iruby-buffer ()
  "Return iruby buffer for the current buffer or project."
  (let ((current-dir (locate-dominating-file default-directory
                                             #'iruby-console-match)))
    (and current-dir
         (iruby-buffer-in-directory current-dir))))

(defun iruby-buffer-in-directory (dir)
  (setq dir (expand-file-name dir))
  (catch 'buffer
    (dolist (buffer iruby-buffers)
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
  ;; NB This is a utility function, used in:
  ;; - `run-iruby'
  ;; - `iruby-proc'
  ;;    thence, in `iruby-send-region' ...
  (cl-flet ((check-buffer (buffer)
              (when buffer
                (if no-filter-live
                    buffer
                  (when (process-live-p (iruby-buffer-process buffer))
                    buffer)))))
    (or (when (eq major-mode 'iruby-mode)
          (current-buffer))
        (check-buffer iruby-buffer)
        (check-buffer (iruby-buffer))
        (check-buffer iruby-last-ruby-buffer))))


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

Type \\[describe-mode] in the process buffer for the list of commands."
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
  ;; FIXME this function was receiving a 'hangup' value that was not a
  ;; symbol, but instead a string terminated with a newline
  ;; with GNU Emacs 27.2 (build 1, x86_64-pc-linux-gnu, GTK+ Version 3.24.27, cairo version 1.17.4)
  ;; on Arch Linux
  (let ((status (process-status process)))
    (unless (or (memq status '(exit finished))
                (stringp state) (string= state "hangup"))
      ;; NB the second arg delivered to the process sentinel
      ;; will normally be an informative string
      (warn "iRuby process %s state changed (%S): %S"
            process status state))))


(defun iruby-jump-to-comint-last-output ()
  "Switch to the ruby process buffer and move point to the position of
`cominit-last-output-start' for the ruby process.

This function may generally  place point at the same location as
`iruby-jump-to-last-output'"
  (interactive)
  (let* ((proc (or (iruby-proc t)
                   (error "No iruby-proc found")))
         (buff (and proc (iruby-process-buffer proc))))
    (switch-to-buffer buff)
    (goto-char comint-last-output-start)))



(defun iruby-jump-to-process-mark ()
  "Switch to the ruby process buffer and move point to the position of
the `process-mark' for the ruby process.

See also: `iruby-jump-to-last-output'"
  (interactive)
  (let* ((proc (or (iruby-proc t)
                   (error "No iruby-proc found")))
         (buff (and proc (iruby-process-buffer proc)))
         (mark (process-mark proc)))
    (switch-to-buffer buff)
    (goto-char mark)))


(defun iruby-jump-to-last-output ()
  "Switch to the ruby process buffer and move point to the start of
the last output text.

If no output has been produced subsequent of the previous input to
the ruby process - such as when entering an empty line of text, or
within a continued block under current input - `point' will then be
positioned at the start of the line representing the last prompt
displayed.

See also: `iruby-jump-to-process-mark', `iruby-jump-to-last-prompt',
`iruby-jump-to-input-end', `iruby-show-last-output'"
  (interactive)
  ;; NB this may generally reach the same point as the value of
  ;; `comint-last-output-start', albeit independent of some
  ;; automation in how the latter variable is set.
  ;;
  ;; NB this differs in relation to `iruby-jump-to-last-prompt'
  ;; and `iruby-jump-to-input-end' in that those functions will
  ;; utilize text properties typically set by comint, for the comint
  ;; configuration in an iruby process buffer. Those functions were
  ;; produced originally as utilities for `iruby-get-old-input'. The
  ;; functions may operate generally in a manner of text scanning on
  ;; the contents of the iruby process buffer, towards a purpose of
  ;; reusing earlier input.
  ;;
  ;; This function and the similar, here, were implemented for
  ;; a purpose of capturing the most recent output fromt the ruby
  ;; process. This set of functions will use local variables
  ;; assumed to have been set by various local hook functions, during
  ;; output presentation by comint within iruby process buffers
  (let* ((proc (or (iruby-proc t)
                   (error "No iruby-proc found")))
         (buff (and proc (iruby-process-buffer proc))))
    (switch-to-buffer buff)
    (goto-char iruby-last-process-mark-point)))

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


(defun iruby-process-impl (proc)
  (with-current-buffer (iruby-process-buffer proc)
    (or iruby-buffer-impl-name
        (error "No implementation name found for process %S" proc))))


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

;; (iruby-read-process)

(defun iruby-switch-to-process (process)
  "If the iRuby process buffer denoted by `process' is displayed in
a buffer on the current frame, then switch to that buffer, else switch
to the iRuby process buffer, using the selected window.

See also:
`iruby-switch-to-process-other-window'
`iruby-switch-to-process-other-frame'"
  (interactive (list (iruby-read-process "Switch to iRuby Process: ")))
  (let ((buff (iruby-process-buffer process))
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
      (t (switch-to-buffer buff)))))

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
  (interactive (list (iruby-read-process "Use process: ")
                     (if current-prefix-arg
                         (read-buffer "Switch process for buffer: ")
                       (current-buffer))))
  (let* ((buffproc (iruby-buffer-process buffer))
         (procbuff (when buffproc
                     (iruby-process-buffer buffproc))))
    (cond
      ((eq buffer procbuff)
       (error "Cannot change the iRuby process for process buffer %s" buffer))
      ((and buffproc (eq process buffproc))) ;; no-op
      (t (with-current-buffer buffer
           (unless (process-live-p process)
             (warn "Using extant process %s in buffer %s" process buffer))
           (setq iruby-buffer (iruby-process-buffer process)
                 ))))))

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

See also:
functions `iruby-buffer-process' and `iruby-process-buffer'
function `iruby-read-processs'
command `iruby-restart-process'
function `iruby-drop-process' and the variable `kill-buffer-hook'")


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
           (proc (iruby-buffer-process whence)))
      (when (and proc (iruby-process-running-p proc))
        ;;; FIXME this needs cleanup, in how iruby-close-process is implemented
        ;;; though it might be preferred, subsequently:
        ;; (iruby-close-process proc)
        ;;; FIXME temporarily more effective, in that it does not produce
        ;;; warnings as iruby-close-process may: `comint-send-eof',
        ;;; presently used as a first call in `iruby-close-process'
        (comint-send-eof))
      (iruby-remove-process-buffer whence)
      (when (eq whence iruby-last-ruby-buffer)
        (setq iruby-last-ruby-buffer (caar iruby-process-buffers))
      ))))

(add-hook 'kill-buffer-hook 'iruby-drop-process)

(defun iruby-close-process (process)
  (interactive
   (list (if (or current-prefix-arg (null iruby-buffer))
             (iruby-read-process "Close iRuby process: ")
           (iruby-buffer-process (iruby-get-prevailing-buffer t)))))
  ;; NB cleanup:
  ;;
  ;; This function will not modify `iruby-process-buffers', such that a
  ;; process that has exited will still be initially accessible to the
  ;; user, as via `read-iruby-process'
  ;;
  ;; `iruby-restart-process' will modify `iruby-process-buffers',
  ;; removing the closed process and setting the newly initialized
  ;; process for the corresponding entry in `iruby-process-buffers'
  ;;
  ;; When an iruby-mode buffer is closed and `kill-buffer-hook' has been
  ;; normally configured for this callback,  `iruby-drop-process' will
  ;; close the process and remove the process from `iruby-process-buffers'
  ;;
  ;; By not removing the process from `iruby-process-buffers' here, this
  ;; should serve to ensure that a closed process can be normally
  ;; restarted by the user, such as via `iruby-restart-process'
  (let ((buff (iruby-process-buffer process))
        (timeout 1.25)) ;; FIXME defcustom for the timeout... or N/A here
    (cl-macrolet ((when-process (&body body)
                    `(if (iruby-process-running-p process)
                         (catch 'timeout
                           (with-timeout (timeout)
                             ;; FIXME this timeout sequence is actually insufficient
                             ,@body))
                       (cl-return-from close))))
    (with-current-buffer buff
      (unwind-protect
           (cl-block close
             (when-process
              (process-send-eof process)
              (unless (iruby-process-running-p process)
                (cl-return-from close)))
             (when-process
              ;; (warn "INT while %s" (iruby-process-status process))
              ;; if process is still running, interrupt
              (interrupt-process process))
             (when-process
              ;; (warn "K while %s" (iruby-process-status process))
              ;; if process is still running, send a process kill signal
              (kill-process process))
             (when-process
              ;; else fail
              ;; FIXME reached too often - needs a normal wait for the
              ;; reply to the initial exit, and something like a timed
              ;; waitpid call after each eof, interrupt, etc send
              (error "Unable to close process %s" process))))))))

(defun iruby-restart-process (process)
  "Restart an iRuby process.

This function may be called interactively"
  (interactive
   (list (if (or current-prefix-arg (null iruby-buffer))
             (iruby-read-process "Restart iRuby process: ")
           (iruby-buffer-process (iruby-get-prevailing-buffer t)))))
  (let ((buff (iruby-process-buffer process))
        (elt (assq process iruby-process-buffers)))
    (with-current-buffer buff
      (let ((cmd (process-command process))
            newbuff proc)
        (iruby-close-process process)
        ;; update the entry in iruby-process-buffers
        ;; and return the emacs process object
        (setq newbuff
              (comint-exec buff (process-name process)
                           (car cmd) nil (cdr cmd))
              proc (get-buffer-process newbuff))
        (setf (car elt) proc)
        ))))


(defun iruby-configure-syntax-table (&optional buff mode)
  "Configure the syntax table in the buffer `buff' per `mode'

If non-nil, <mode> must denote a key in `iruby-ruby-modes'. The default
value is provided by `iruby-ruby-syntax'

This function may be called interactively.

If called interactively or if either arg is nil:
- The prevailing iRuby process buffer will be used as `buff' unless
  this function was called interactively and with a prefix numeric
  argument, in which case the user will be prompted to select an iRuby
  process buffer.
- The value of `iruby-ruby-syntax' will be used as `mode', if `mode' is
  nil or if this function is called interactively"
  (interactive (list iruby-ruby-syntax
                     (if current-prefix-arg
                         (iruby-read-process
                          "Configure syntax table for iRuby buffer: ")
                       (iruby-get-prevailing-buffer t))))
    (let* ((mode (or mode iruby-ruby-syntax))
           (mode-inf (or (assoc mode iruby-ruby-modes)
                         (error "not found in iruby-ruby-modes: %S" mode)))
           (buffer (or buff (iruby-get-prevailing-buffer t)
                       (error "Found no iRuby buffers"))))
      (cond
        (mode-inf
         (destructuring-bind (name feature table-var) mode-inf
           (with-current-buffer buffer
             (require feature)
             ;; TBD integrating mode-specific font lock keywords w/
             ;; comint. For now, this supports at least a syntax table
             ;; from some Ruby language mode
             (set-syntax-table (symbol-value table-var)))))
        (t (warn "iRuby: No syntax table available for syntax mode %s"
                 mode)))))


(defun run-iruby-new (command &optional name)
  "Create a new inferior Ruby process in a new buffer.

COMMAND is the command to call. NAME will be used for the name of
the buffer, defaults to \"ruby\"."
  (let* ((commandlist
          (cl-etypecase command
            (string (split-string-and-unquote command))
            (cons command)))
         (name (or name
                   (file-name-nondirectory (car commandlist))))
         (buffer (current-buffer)) ;; ??
         process
         ;; FIXME store the buffer-name (via the process),
         ;; indexed under the impl `name'
         (buffer-name (generate-new-buffer-name (format "*%s*" name)))
         (process-environment process-environment)
         (comint-delimiter-argument-list
          ;; ?? NB add all mathematical symbols, ...
          '(?\| ?& ?< ?> ?\( ?\) ?\;)))

    (let ((pager (cl-find-if (lambda (binding)
                               (equal (car (split-string binding "=")) "PAGER"))
                             process-environment))
          (cat-pager (format "PAGER=%s" (or (executable-find "cat")
                                            ;; FIXME relative cmd as a default
                                            "cat"))))
      ;; http://debbugs.gnu.org/15775
      (when pager
        (setq process-environment (remove pager process-environment)))
      (setq process-environment (cons cat-pager process-environment)))


    ;; FIXME comint (??) is removing tabs on input heredocs and other
    ;; expressions, such as to concatenate the expression as read by ruby
    ;;
    ;; - (??) may use 'comint-accumulate' + `comint-send-input`
    ;;   in heredoc sections and other multiline input
    ;;
    ;; - TBD how to handle the case of the tabs being removed with no
    ;;    whitespace substitution for single-line text (not only
    ;;    heredocs or defs)
    ;;
    ;; e.g  with "\t" being a literal tab character on input,
    ;; "def a;\tend" is read as "def a;end"
    ;;

    (setq buffer
          (apply 'make-comint-in-buffer
                 name buffer-name
                 (car commandlist) nil (cdr commandlist)))

    (setq process (get-buffer-process buffer))
    (set-process-sentinel process 'iruby-process-sentinel)
    (iruby-add-process-buffer process buffer)

    (set-buffer buffer)
    (iruby-mode)
    (iruby-configure-syntax-table buffer iruby-ruby-syntax)
    (setq iruby-buffer-impl-name name
          iruby-buffer-command command
          iruby-impl-binding-expr (ignore-errors
                                    (iruby-get-impl-binding-expr name))
          iruby-impl-completion-expr (ignore-errors
                                       (iruby-get-impl-completion-expr name)))

    (unless (boundp 'desktop-save-buffer)
      (make-variable-buffer-local
       (defvar desktop-save-buffer nil
         "Buffer-local configuration for `desktop-save'

This variable is normally initialized by the Emacs desktop library,
unless that feature is not available")))
    (setq desktop-save-buffer 'iruby-desktop-misc-data)

    (add-hook 'comint-preoutput-filter-functions  'iruby-update-last-mark nil t)
    (iruby-remember-ruby-buffer buffer) ;; TBD
    (push buffer iruby-buffers) ;; TBD

    (unless (and iruby-buffer (iruby-process-running-p iruby-buffer))
      (when iruby-buffer
        (let ((proc (get-buffer-process iruby-buffer)))
          (when (and proc (not (eq (process-status proc) 'run)))
            ;; NB trying a cleanup call, as when the buffer
            ;; process did not exit cleanly - seen during some testing
            ;; with Ruby Gtk support, in which they iruby process state
            ;; was changed externally, from within gtk, to a state
            ;; "trace/breakpoint trap (core dumped)"
            ;;
            ;; FIXME the call-flow needs to be implemented more cleanly here
            (kill-process proc))))
      (setq iruby-buffer buffer))
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

(defconst iruby-send-terminator "--iruby-%x-%d-%d-%d--"
  "Template for irb here document terminator.
Must not contain ruby meta characters.")


(defconst iruby-eval-separator "")


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
  (let* ((term (apply 'format iruby-send-terminator
                      (random) (current-time))))
    (save-excursion
      (save-restriction
        (let ((m (process-mark proc))
              (hdr
               (format "eval <<'%s', %s , '%s', %d;\n"
                       term iruby-impl-binding-expr
                       (or file "(Unknown)")
                       (or line 0)))
              (tlr
               (concat "\n" term "\n")))
          (set-buffer (marker-buffer m))
          (goto-char m)
          (insert iruby-eval-separator "\n")
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


(defun iruby-update-last-mark (string)
  "Update `iruby-last-process-mark-point' and return STRING,
before STRING may be written to the process buffer.

This function should be called with a ruby process buffer as
the current buffer.

This is a hook function for `comint-preoutput-filter-functions'"
  ;; NB The string may include any prompt text,
  ;; generally as the last line in the string
    (cond
      ((null iruby-last-process-mark-point)
       ;; reached for the first line of output,
       ;; such that would typically contain only
       ;; the prompt string
       ;;
       ;; FIXME this assumes that not any errors
       ;; have occurred errors during irb init
       (setq iruby-last-process-mark-point
             (length string)))
      (t
       (let ((markerpt (marker-position
                        (process-mark
                         (or (get-buffer-process (current-buffer))
                             (error "No process in buffer %s"
                                    (current-buffer)))))))
         (setq iruby-last-process-mark-point markerpt))))
    string)



(defun iruby-send-region (start end &optional file-name line process)
  "Send a region of text from the current buffer to the ruby process.

When called interactively, this function operates on any region
in the current buffer.

If a FILE-NAME and LINE are provided, these will be sent to the
ruby process for association with the evaluated code. Otherwise,
the first non-nil value of `buffer-file-name' or the return value
of `buffer-name' will be used as the file name, with the line
number of point in the current buffer

If `iruby-show-lkast-output' is enabled and `iruby-threads' is
non-nil, this function will call `iruby-show-last-output' in
a separate thread, after a delay in seconds configured in
`iruby-output-wait'. The output value, `true' would indicate
successful load of the file, within the ruby process"
  (interactive
   (let ((st (point))
         (end (condition-case nil
                  (mark)
                (mark-inactive (point-max))))
         (proc (if current-prefix-arg
                   (iruby-read-process "Send region to Ruby" t)
                 (iruby-proc))))
     (list st end nil nil proc)))
 (let ((proc (or process (iruby-proc))))
    (save-excursion
      (save-restriction
        (iruby-send-string proc (buffer-substring-no-properties start end)
                           (or file-name buffer-file-name (buffer-name))
                           (or line (1+ (line-number-at-pos (min start end) t))))
        (when (and iruby-show-last-output iruby-threads)
          ;; NB this will use multithreading, in a form of asynchronous
          ;; eval, to be able to capture the output from the process
          ;; just launched, outside of the control flow in the function
          ;; that launched that process
          ;;
          ;; This is needed not only for coordination with the ruby
          ;; process, but furthermore for allowing update of any state
          ;; variables for the comint buffer in the Emacs main thread,
          ;; such that those variables should be updated before the next
          ;; output is processed.
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
                       (format "iruby output monitor %s"
                               (mapconcat #'number-to-string (current-time) "-")))))
            ))))))

;; NB
;; (thread-last-error t)


(defun iruby-send-definition ()
  "Send the current definition to the inferior Ruby process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (ruby-beginning-of-defun)
      (iruby-send-region (point) end))))


(defun iruby-send-last-sexp ()
  "Send the previous sexp to the inferior Ruby process."
  (interactive "P")
  (iruby-send-region (save-excursion (ruby-backward-sexp) (point))
                     (point)))


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


(defvar iruby-last-ruby-buffer nil
  "The last buffer we switched to `iruby' from.")
(make-variable-buffer-local 'iruby-last-ruby-buffer)


(defun iruby-remember-ruby-buffer (buffer)
  (setq iruby-last-ruby-buffer buffer))


(defun iruby-switch-to-inf (eob-p)
  "Switch to the ruby process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (let ((buffer (current-buffer))
        (iruby-buffer* (or (iruby-buffer) iruby-buffer)))
    (if iruby-buffer*
        (progn
          (pop-to-buffer iruby-buffer*)
          (iruby-remember-ruby-buffer buffer))
      (error "No current process buffer, see variable iruby-buffers")))
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))


(defun iruby-switch-to-last-ruby-buffer ()
  "Switch back to the last Ruby buffer."
  (interactive)
  (if (and iruby-last-ruby-buffer
           (buffer-live-p iruby-last-ruby-buffer))
      (pop-to-buffer iruby-last-ruby-buffer)
    (message "Last Ruby buffer is unavailable")))


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
  "Load a Ruby file into the inferior Ruby process."
  (interactive
   (list (car
          (comint-get-source "Load Ruby file: " iruby-prev-l/c-dir/file
                             ;; T because LOAD needs an exact name
                             iruby-source-modes t))
         (if current-prefix-arg
             (iruby-read-process "Load file in ruby")
           (iruby-proc))))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (let ((file (expand-file-name file-name))
        (proc (or process (iruby-proc))))
    (setq iruby-prev-l/c-dir/file (cons (file-name-directory    file)
                                        (file-name-nondirectory file)))
    (with-temp-buffer
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
  (let* ((proc (iruby-proc))
         (line (buffer-substring (save-excursion (move-beginning-of-line 1)
                                                 (point))
                                 (point)))
         (expr (iruby-completion-expr-at-point))
         (prefix-offset (- (length expr) (length prefix)))
         (comint-filt (process-filter proc))
         (kept "") completions
         ;; Guard against running completions in parallel:
         )
    ;;;; (warn "Completions. prefix %S expr %S" prefix expr)
    (unless (equal "(rdb:1) " iruby-last-prompt)
      (set-process-filter proc
                          (lambda (proc string)
                            (setq kept (concat kept string))
                            ;; ensure that the string is not displayed:
                            nil))
      (unwind-protect
          (let ((completion-snippet
                 (format
                  (concat
                   iruby-impl-completion-expr
                   "; nil;\n"

                   ;; NB supporting IRB only , for now
                   ;;
                   ;; FIXME This API needs a better way to store
                   ;; implementation-specific properties as constant
                   ;; expressions, under custom - cmd, args,
                   ;; eval contxt, this bit, ...

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
              (setq completions (cdr completions))))
        (set-process-filter proc comint-filt)))
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
      (let ((bounds (bounds-of-thing-at-point 'sexp)))
        (when (consp (cdr bounds))
          ;; NB fix up some inconsistent syntax
          ;; from thingatpt
          (setf (cdr bonds) (cadr bounds)))
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
  (eval-after-load 'ruby-compilation ;; NB rinari
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

See also: `iruby-mapped-buffer-name'"
  (unless (or (eq major-mode 'iruby-mode)
              (null iruby-buffer))
    (buffer-name)))

(defun iruby-restore-mapped-buffer-name (stored)
  "If the value of `stored' is non-nil, set the value of
`iruby-mapped-buffer-name' in the current buffer, using the value of
`stored'

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


(defun iruby-restore-desktop-buffer (file name data)
  "Callback function for iRuby support in `desktop-read'

This function's name will normally be stored for the `iruby-mode' entry
under the global `desktop-buffer-mode-handlers' list.

See also: `iruby-ensure-desktop-support'; `iruby-desktop-misc-data'"
  (let ((cmd (or (cdr (assq 'iruby-buffer-command desktop-buffer-locals))
                 (cdr (assq :cmd data))
                 (progn
                   (warn "no iruby-buffer-command saved in desktop data for %s. \
Using current defaults for %s" name iruby-default-implementation )
                   (iruby-build-impl-cmd iruby-default-implementation))))
        (dir (or (cdr (assq 'default-directory desktop-buffer-locals))
                 (cdr (assq :dir data))
                 default-directory))
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
      (let* ((procbuff (run-iruby-new cmd (iruby-buffer-short-name name)))
             (proc (get-buffer-process procbuff))
             (exp-dir (expand-file-name dir)))
        (when mapped
          (with-current-buffer procbuff
            ;; set up for the hook onto `iruby-map-desktop-process-buffers'
            (setq iruby-mapped-source-buffers mapped)))
        (iruby-send-string proc
                           (format "puts(%%q(# iruby chdir to %s))" dir))
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
- :cmd => `iruby-buffer-command' i.e for the original process
- :dir => `default-directory' for the buffer, in Emacs
- :mapped => list of buffer names, for buffers where `iruby-buffer' has
   been set as eq to the current buffer

See also:
 `iruby-map-desktop-process-buffers',
 `iruby-ensure-desktop-support'"
  (let ((self (current-buffer))
        (mapped (list nil)))
    (dolist (srcbuff (buffer-list))
      (unless (eq srcbuff self)
        (with-current-buffer srcbuff
          (when (eq iruby-buffer self)
            (setf (cdr (last mapped))
                  ;; FIXME it may not be sufficient to store only the
                  ;; buffer name, as desktop-read will not exactly
                  ;; restore the buffer name in use at time of
                  ;; desktop-save
                  ;;
                  ;; e.g  both of "file.rb<lib>" and "file.rb<test>"
                  ;; will be stored with a buffer name "file.rb" in the
                  ;; desktop session data. It may not necessarily be
                  ;; assumed that the buffer names will be restored
                  ;; equivalently under every later Emacs session,
                  ;;
                  ;; As in this example, when a new "file.rb" has been
                  ;; opened, all of the buffer names on a "file.rb" file
                  ;; might be modified, such that if desktop-read is
                  ;; called after the new "file.rb" is opened, the
                  ;; buffer names may not match anything that was
                  ;; stored.
                  ;;
                  ;; TBD whether and how desktop-save may provide any
                  ;; any access to the internal buffer list used to
                  ;; write the desktop file
                  (cons (buffer-name srcbuff) nil))))))
    (list (cons :cmd iruby-buffer-command)
          (cons :dir default-directory)
          (cons :mapped (cdr mapped)))))

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

;;;###autoload (dolist (mode iruby-source-modes) (add-hook (intern (format "%s-hook" mode)) 'iruby-minor-mode))

(provide 'iruby)
;;; iruby.el ends here
