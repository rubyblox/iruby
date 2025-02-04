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

;;; Commentary:
;;
;; This Emacs Lisp library was originally developed as inf-ruby.el
;;
;; To prevent incompatibility with Emacs Lisp software that may expect
;; an interface for inf-ruby.el, after a fork, the library was renamed
;; to iruby.el and top-level symbols were renamed correspondingly.
;;
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
;;`iruby-interactive-impls'.
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
;;   interactive irb process buffer
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
;;    (autoload 'iruby "iruby" "Run an interactive Ruby process" t)
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
(require 'thingatpt)

(eval-when-compile
  (require 'cl-macs)
  (require 'cl)
  (defvar rspec-compilation-mode-map)
  (defvar ruby-compilation-mode-map)
  (defvar projectile-rails-server-mode-map))

(require 'iruby-util)


(defgroup iruby nil
  "Run Ruby process in a buffer"
  :tag "iRuby"
  :group 'languages)

(cl-defmacro with-iruby-widget-validate ((var &optional
                                              (message "Invalid value: %S"))
                                         &body test-forms)
  (with-symbols-iruby (widget)
    `(lambda (,widget)
       (let ((,var (widget-value ,widget)))
         (unless (progn ,@test-forms)
           (widget-put ,widget
                       :error (format ,message ,var))
           ,widget)))))

(defvar iruby-load-file-history nil
  "History data for interactive comint forms with `iruby-load-file'

The bounds of this history table under `iruby-load-file' may be
configured with `iruby-load-file-history-limit'")


(defcustom iruby-load-file-history-limit history-length
  "File name history limit for `iruby-load-file-history'

If zero, no file name history will be stored under `iruby-load-file'.

If t, no bounds will be used on `iruby-directory-history'.

The default initial value is derived from `history-length'"
  :tag "iRuby load file history - history limit"
  :type `(choice (integer
                  :tag "Finite limit"
                  :validate
                  ,(with-iruby-widget-validate (len "Unable to parse history limit: %S")
                     (and (integerp len) (or (zerop len) (plusp len)))))
                 (const :tag "No limit" t))
  ;; TBD :group 'iruby-files
  :group 'iruby)

(defvar iruby-directory-history nil
  "History list for `iruby:read-directory'

See also, `iruby-directory-history-limit'")

(defcustom iruby-directory-history-limit history-length
  "history limit for `iruby-directory-history'.

If zero, no new items will be added to `iruby-directory-history'.

If t, no bounds will be used on `iruby-directory-history'.

The default initial value is derived from `history-length'"
  :tag "iRuby directory history - history limit"
  :type `(choice (integer
                  :tag "Finite limit"
                  :validate
                  ,(with-iruby-widget-validate (len "Unable to parse history limit: %S")
                     (and (integerp len) (or (zerop len) (plusp len)))))
                 (const :tag "No limit" t))
  ;; TBD :group 'iruby-files
  :group 'iruby)



(defgroup iruby-ui nil
  "iRuby user interface support"
  :tag "iRuby User Interface"
  :group 'iruby)


(defcustom iruby-app-name "iRuby"
  "Label for iruby-minor-mode in various user interface elements.

This value will be used as a sufix for `iruby-minor-mode' in the
modeline, there prefixed with the value of `iruby-minor-mode-prefix.'

This value is also used for menu entries in `iruby-minor-mode', such
that will use the value of this variable when iruby.el is evaluated.
When this custom value is updated, the updated value may not be
reflected in those menu entries until Emacs is restarted"
  :tag "iRuby app name"
  :group 'iruby-ui
  :type 'string)

(defcustom iruby-minor-mode-prefix " "
  "Prefix delimiter for use with `iruby-app-name' in `iruby-minor-mode'

This value will be used under an `iruby-minor-modeline-format' that
would call `iruby-minor-modeline-default-label' in the format's
evaluation block.

This value should generally contain at least a whitespace character or
printable delimiter"
  :tag "iRuby minor-mode prefix"
  :group 'iruby-ui
  :type 'string)


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
  :tag "iRuby minor modeline format"
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
  "If non-nil, show results in the minibuffer after iruby send"
  :tag "iRuby show last output"
  :type 'boolean
  :group 'iruby-ui)

(defcustom iruby-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :tag "iRuby prompt read-only"
  :type 'boolean
  :group 'iruby-ui)

;;; the smie support here needs extensive debug (emacs 29)
;; (defcustom iruby-use-smie nil
;;   "If non-nil, use smie in new iRuby buffers
;;
;; This value will be used for ruby-mode syntax support in iRuby,
;; "
;;   :tag "iRuby use SMIE"
;;   :type 'boolean
;;   :group 'iruby-ui)

(defgroup iruby-language nil
  "Ruby language support for iRuby"
  :tag "iRuby language"
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

`iruby-buffer-syntax-table'
`iruby-ruby-abbrev-table'
`iruby-ruby-mode-function'
`iruby-source-modes'
`iruby-find-syntax'

Among other features, this allows for reusing the syntax table and
abbrev table from any major mode defined here, within any individual
iRuby buffer.

See also: `edit-abbrevs'"
  :tag "iRuby Ruby modes"
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

(defcustom iruby-default-ruby-syntax "ruby-mode"
  "Default Ruby language mode for iRuby buffers.

The Ruby language mode selected here will be used for configuration
of the syntax table and other Ruby language mode features for iRuby
buffers.

This value must match a key value (CAR) in the associative list,
`iruby-ruby-modes'"
  :tag "iRuby default Ruby syntax"
  :group 'iruby-language
  :type `(choice ,@(mapcar (lambda (item) `(const ,(car item)))
                           iruby-ruby-modes)))

(make-variable-buffer-local
 (defvar iruby-buffer-syntax nil
   "If non-nil, the Ruby syntax for the current Ruby buffer.

See also: `iruby-default-ruby-syntax'"))

(defun iruby-find-syntax (&optional name)
  "Return the syntax entry from `iruby-ruby-modes' for the current buffer.

If the current buffer is using a `major-mode' represented in `iruby-ruby-modes'
this returns the syntax entry for that major-mode.

Else, this returns the syntax entry `iruby-default-ruby-syntax'."
  ;; FIXME ensure that this is used early enough to provide reasonable
  ;; defaulting for `iruby-mode'
  (or
   (when name
     ;; determine the syntax for the provided syntax name
     (or (cl-find name iruby-ruby-modes :key #'car :test #'equal)
         (error "No iruby-ruby-modes syntax found for name %S" name)))
   (cl-dolist (elt iruby-ruby-modes)
     (cl-destructuring-bind (name feature stx abbrev &optional mode)
         elt
       (when (eq major-mode (or mode feature))
         (cl-return elt))))
   (iruby-find-syntax (or iruby-buffer-syntax iruby-default-ruby-syntax))))


(defun iruby-buffer-syntax-table (&optional syntax)
  (cl-destructuring-bind (name feature stx abbrev &optional mode)
      (iruby-find-syntax syntax)
    (require feature)
    (symbol-value stx)))

;;; ad-hoc test
;; (type-of (iruby-buffer-syntax-table "erm"))
;; => char-table

(defun iruby-ruby-abbrev-table (&optional syntax)
  ;; see also: `edit-abbrevs'
  (cl-destructuring-bind (name feature stx abbrev &optional mode)
      (iruby-find-syntax syntax)
    (require feature)
    (symbol-value abbrev)))


;;; ad-hoc test
;; (type-of (iruby-ruby-abbrev-table "ruby-mode"))
;; => vector

(defun iruby-ruby-mode-function (&optional syntax)
  ;; see also: `edit-abbrevs'
  (cl-destructuring-bind (name feature stx abbrev &optional mode)
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
               (cl-destructuring-bind (name feature stx abbrev &optional mode)
                   elt
                 (or mode feature)))
           iruby-ruby-modes)))

;; (iruby-source-modes)

(defgroup iruby-proc nil
  "iRuby process configuration"
  :tag "iRuby processes"
  :group 'iruby)

(defcustom iruby-pager "cat"
  ;; FIXME find a way to pipe to xterm/other
  "Pager for Ruby process environments

This value should be a string providing a shell command. This shell
command will be used as the value for the PAGER environment variable in
Ruby process buffers.

The first whitespace-delimited element in this string should be a
literal shell command name or absolute path for a shell command name"
  :tag "iRuby pager"
  :type `(string
          :validate
          ,(with-iruby-widget-validate (cmd "Command not found, in %s")
             (ignore-errors (executable-find (car (iruby:split-shell-string cmd))))))
  :group 'iruby-proc)

(defcustom iruby-console-environment 'ask
  "Environment to use for the `iruby-console-*' commands.
If the value is not a string, ask the user to choose from the
available ones.  Otherwise, just use the value.

Currently only affects Rails and Hanami consoles."
  :group 'iruby-proc
  :type '(choice
          ;; "the available ones" ??
          (const ask :tag "Ask the user")
          (string :tag "Environment name")))

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
  :tag "iRuby output wait"
  :group 'iruby-proc)

(defcustom iruby-restart-timeout 1.25
  "Number of seconds for timeouts when closing or restarting a Ruby buffer.

This value represents a measure of seconds for timeout in `iruby-close-process'.
In that function, this value will be applied for each successive call as
to close the Ruby process

This timeout will be used similarly in `iruby-restart-process'

This value should be a positive number or zero."
  :tag "iRuby restart timeout"
  :group 'iruby-proc)

(defvar iruby-threads-p (featurep 'threads)
  "If non-nil, threads are available in this Emacs.")

(defconst iruby-prompt-format
  (concat
   "\\("
   (mapconcat
    #'identity
    '("^%s> *"		; Simple
      "(rdb:1) *"	; Debugger
      "(byebug) *"	; byebug
      "(irb([^)]+)"	; IRB default
      "\\([[0-9]+] \\)?[Pp]ry ?([^)]+)"   ; Pry
      "\\(jruby-\\|JRUBY-\\)?[1-9]\\.[0-9]\\(\\.[0-9]+\\)*\\(-?p?[0-9]+\\)?" ; RVM
      "rbx-head")	; RVM continued
    "\\|")
   ;; Statement and nesting counters, common to the last four.
   " ?[0-9:]* ?%s *\\)")
  "Format string for the prompt regexp pattern.")

(defvar iruby-first-prompt-pattern (format iruby-prompt-format ">" ">")
  ;; not test in iruby - developer uses irb with --inf-ruby-mode
  "First prompt regex pattern of Ruby interpreter.")

(defvar iruby-prompt-pattern (format iruby-prompt-format "[?>]" "[\\]>*\"'/`]")
  ;; used for interactive support with comint, visible on
  ;; mouse-over for previous input and the prompt fontification added by
  ;; commint
  ;;
  ;; not presently used for iruby-prompt/iruby-continuation text properties
  ;; in iruby-preoutput-filter
  "Prompt regex pattern of Ruby interpreter.")

(defvar iruby-mode-hook nil
  "Hook for customizing `iruby-mode'.")

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
    ("C-c C-z" iruby-switch-to-ruby)
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
  ("C-c C-z" iruby-switch-to-ruby)
    ;;; &rest
  ("C-M-x"   iruby-send-definition)
  ("C-x C-e" iruby-send-last-sexp)
  ("C-c C-b" iruby-send-block)
  ("C-c M-b" iruby-send-block-and-go)
  ("C-c C-x" iruby-send-definition)
  ("C-c M-x" iruby-send-definition-and-go)
  ("C-c C-r" iruby-send-region)
  ("C-c M-r" iruby-send-region-and-go)
  ("C-c C-z" iruby-switch-to-ruby)
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
    ["Switch to REPL" iruby-switch-to-ruby t]))


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


(defvar iruby-warnings-once-history nil
  "Session-local storage for `iruby-warn-once'")

(defun iruby-warn-once (message &rest format-args)
  "Call `warn' with `message' and `format-args' unless a similar warning
message has already been produced under `iruby-warn-once'

This function is used in `iruby-completion-at-point', to ensure that the
user is notified at most once when the local iRuby implementation does
not have any completion support enabled in iRuby"
  (let ((msg (apply #'format-message message format-args)))
    (unless (member msg iruby-warnings-once-history)
      ;; NB ensuring that the actual format-args are passed to the
      ;; warning - using the locally produced msg only for purposes
      ;; of caching
      (push msg iruby-warnings-once-history)
      (apply #'warn message format-args))))


(defvar iruby-warnings-once-history nil
  "Session-local storage for `iruby-warn-once'")

(defun iruby-warn-once (message &rest format-args)
  "Call `warn' with `message' and `format-args' unless a similar warning
message has already been produced under `iruby-warn-once'

This function is used in `iruby-completion-at-point', to ensure that the
user is notified at most once when the local iRuby implementation does
not have any completion support enabled in iRuby"
  (let ((msg (apply #'format-message message format-args)))
    (unless (member msg iruby-warnings-once-history)
      ;; NB ensuring that the actual format-args are passed to the
      ;; warning - using the locally produced msg only for purposes
      ;; of caching
      (push msg iruby-warnings-once-history)
      (apply #'warn message format-args))))


;;;###autoload
(define-minor-mode iruby-minor-mode
    "Minor mode for interacting with iRuby processes in Ruby source buffers

Minibuffer display for this minor mode can be configured with the
custom variable, `iruby-minor-modeline-format'

The following commands are available:

\\{iruby-minor-mode-map}"
  ;; NB the :lighter form will be used with `minor-mode-alist'.
  ;; see also, `mode-line-format'
  :lighter iruby-minor-modeline-format
  :keymap iruby-minor-mode-map
  )


(make-variable-buffer-local
 (defvar iruby-buffer nil
   "When non-nil, the iruby process buffer to use for this buffer.

See also:  `iruby-use-ruby', `iruby-proc', and the variable
`iruby-default-ruby-buffer'"))

(make-variable-buffer-local
 (defvar iruby-buffer-command nil
   "The command that was used to run the interactive Ruby"))

(make-variable-buffer-local
(defvar iruby-buffer-interactive-impl nil
  "Interactive Ruby implementation, local to an iRuby process buffer"))

(defun iruby-initialize-buffer-for (&optional impl syntax)
  ;; shared forms for configuring syntax and implementation bindings for
  ;; `iruby-mode'
  ;;
  ;; used by `run-iruby-new' and `iruby-restart-process'
  (let* ((buff (current-buffer))
         (proc (or (get-buffer-process buff)
                   (error "Buffer has no process: %s" buffer)))
         (use-impl (or impl iruby-buffer-interactive-impl
                       (progn
                         (iruby-debug-info "Using default interactive ruby in %S" buff)
                         (iruby:default-interactive-ruby))))
         (use-syntax (or syntax iruby-buffer-syntax iruby-default-ruby-syntax)))
    (setq iruby-buffer-command (process-command proc))
    (when use-syntax
      (setq iruby-buffer-syntax use-syntax)
      (set-syntax-table (iruby-buffer-syntax-table use-syntax)))
    (setq
     iruby-buffer-interactive-impl use-impl
     iruby-buffer-binding-expr (iruby:interactive-binding-expr use-impl)
     iruby-buffer-completion-expr (iruby:interactive-completion-expr use-impl))
    use-impl))


(define-derived-mode iruby-mode comint-mode 'iruby-app-name
  "Major mode for interacting with an interactive Ruby REPL process.

A simple IRB process can be fired up with \\[iruby].

To launch a REPL with project-specific console instead, type
\\[iruby-console-auto].  It recognizes several
project types, including Rails, gems and anything with `racksh'
in their Gemfile.

Customization: When entered, this mode runs `comint-mode-hook' and
`iruby-mode-hook' (in that order).

You can send text to the activer Ruby process from other buffers
containing Ruby source.

- `iruby-send-definition' sends the current definition in the active
   buffer. The bounds of the definition will be determined as per the
   syntax handling of the active major-mode in the buffer, e.g
   `ruby-mode' or `enh-ruby-mode'.

- `iruby-send-region' sends the current region of text

- `iruby-load-file' sends an expression to load the current file

- `iruby-send-definition-and-go' and `iruby-send-region-and-go'
    switch to the ruby process buffer after sending their text.

- `iruby-switch-to-ruby' switches to the active Ruby process buffer

Commands:
`RET' after the end of the process' output sends the text from the
    end of process to point.
`RET' before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
`DEL' converts tabs to spaces as it moves back.
`TAB' completes the input at point. IRB and Pry completion are supported.
`C-M-q' does `TAB' on each line starting within following expression.
Paragraphs are separated only by blank lines.  # start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

The following commands are available:

\\{iruby-mode-map}"
  :group 'iruby
  :syntax-table (iruby-buffer-syntax-table)
  :abbrev-table (iruby-ruby-abbrev-table)
  :interactive nil
  (setq comint-prompt-regexp iruby-prompt-pattern)

  (when (equal (or iruby-buffer-syntax iruby-default-ruby-syntax)
               "ruby-mode")
  (ruby-mode-variables)

  ;; (when iruby-use-smie
  ;;   ;; FIXME needs extensive debug (Emacs 29)
  ;;   (require 'ruby-mode)
  ;;   ;; FIXME how is this still being reached?
  ;;   (ruby-mode-varibles) ;; calls `smie-setup' for `ruby-smie-grammar'
  ;;   (set (make-local-variable 'smie-forward-token-function)
  ;;        #'iruby-smie--forward-token)
  ;;   (set (make-local-variable 'smie-backward-token-function)
  ;;        #'iruby-smie--backward-token))
  )

  (set (make-local-variable 'comint-delimiter-argument-list)
        '(?\| ?& ?< ?> ?\( ?\) ?\; ?\" ?\.))

  ;; preoutput filter is where text props for the prompt are added
  (add-hook 'comint-preoutput-filter-functions 'iruby-preoutput-filter nil t)
  ;; output filter is used for recovering any pending input after
  ;; process output
  (add-hook 'comint-output-filter-functions 'iruby-output-filter nil t)

  (add-to-list 'kill-buffer-hook 'iruby-drop-process)

  (unless (boundp 'desktop-save-buffer)
    (make-variable-buffer-local
     ;; desktop.el preload
     ;;
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
  ;; (set (make-local-variable 'comint-prompt-read-only) iruby-prompt-read-only)
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

(defun iruby-forward-output (point &optional use-current)
  (interactive "d")
  (let ((at point)
        start end
        match)
    (goto-char at)
    (when (setq match (text-property-search-forward 'field 'output t
                                                    (not use-current)))
      (goto-char at) ;; reset after the property search moved point
      (setq start (prop-match-beginning match)
            end (prop-match-end match))
      (cond
        ((string-whitespace-p (buffer-substring-no-properties start end))
         (iruby-forward-output end))
        (t
         (goto-char start))))))

(defun iruby-start-of-input (point)
  "If POINT is within an input area, move point to the end of the
previous prompt preceding POINT - similarly, the beginning of the
input area.

If POINT is within a prompt, point will be moved to the end of the
prompt.

If POINT is not within an output area, point will be moved to the
beginning of the previous input area."
  ;; NB the iruby-prompt text property will have been added in
  ;; `iruby-preoutput-filter',  assuming that the Ruby subprocess'
  ;; prompt matches the regexp stored in `iruby-prompt-pattern'
  (interactive "d")
  (let ((at point)
        (start point)
        match)
    (goto-char at)
    (cond
      ((get-text-property at 'iruby-prompt)
       ;; generally at an iRuby prompt
       ;; - go forward to end of prompt
       ;;
       ;; NB not-current => t  might be an implicit default with the
       ;; text-property-search functions, thus it may need an explicit
       ;; nil arg to allow for including any current property in the
       ;; search
       (when (setq match (text-property-search-forward 'iruby-prompt t 'eq nil))
         (goto-char (prop-match-end match))))
      (t
       ;; point is at an input field, or in output from the ruby process
       ;; - move to the end of the previous prompt
       ;; - if no previous prompt is found, return point at the end of
       ;;   search
       (cond
         ((setq match (text-property-search-backward 'iruby-prompt t 'eq t))
          (goto-char (prop-match-end match)))
         (t (point)))))))


(defun iruby-end-of-input (point)
  (interactive "d")
  (iruby-start-of-input point)
  (iruby-forward-output (point))
  (let ((match (text-property-search-backward 'field 'boundary 'eq)))
    ;; move point to the last boundry field before the output area.
    ;;
    ;; that should represent the lexical end of the previous input area
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
  "Retrieve any input at or previous to point

When called interactively, the full text of any input at or previous to
point will be displayed in the minibuffer.

See also: `iruby-start-of-input', `iruby-end-of-input',
`iruby-get-old-input'"
  ;; NB This has been tested with an --inf-ruby-mode prompt for irb
  ;;
  ;; - needs testing with a non-empty 2ary prompt, e.g "> " for continued input
  (interactive "d")
  (save-excursion
    (save-restriction
      (let (start match end expr)
        (cond
          ;; move point to the first input area after previous prompt
          ((get-text-property point 'iruby-prompt)
           (iruby-start-of-input point))
          (t
           (iruby-previous-prompt point)))
        (setq start (point))
        (cond
          ((and (setq match (text-property-search-forward 'iruby-prompt nil))
                ;; move to beginning of next non-prompt area, i.e input field
                (goto-char (prop-match-beginning match))
                ;; compute bounds of output - this "Just works" ...
                ;; .. assuming the text properties for iruby-prompt
                ;;    have been added in the preoutput filer
                ;; .. and that the buffer parser has not been confused
                ;;    by text properties from user insert/yank/etc
                (setq match (text-property-search-backward 'field 'output)))
           (setq end (prop-match-end match)))
          (t (setq end start)))
        (setq expr (string-trim (buffer-substring-no-properties start end)))
        (when (interactive-p)
          ;; debug support
          (message "At point: %S" expr))
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
  (interactive)
  (save-excursion
    (save-restriction
      (let* ((at (point))
             (at-field  (get-text-property at 'field))
             (at-prompt (get-text-property at 'iruby-prompt))
             start end next-end match expr)
        (cond
          (at-prompt
           ;; move to end of current prompt
           (when (setq match
                       (text-property-search-forward 'iruby-prompt t 'eq))
             (goto-char (prop-match-end match))))
          (t ;; move to end of previous prompt
           (when (setq match
                       (text-property-search-backward 'iruby-prompt t nil))
             (goto-char (prop-match-end match)))))
        ;; return input at point (FIXME)
        (let ((input (iruby-input-at-point (point))))
          ;: debug support
          (when (interactive-p)
            (message "Old input: %S" input))
          input)
        ))))

(defun iruby-stage-old-input ()
  "Utility function for `iruby-send-or-stage-input'"
  (interactive)
  (save-restriction
    (let ((str (iruby-get-old-input)))
      (goto-char (iruby-strip-pending-input))
      (insert str))))

(defun iruby-send-or-stage-input ()
  "Send or stage input in an `iruby-mode' buffer.

If point is within a previous input area, that input will be copied to
the most recent input prompt, to allow editing before send. Any pending
input at the prompt will be stored and presented after the next input
prompt, in the buffer.

If point is within a previous output area, this should serve to capture
the input that was evaluated when producing the output from the Ruby
process. The initial input should be transposed to the current input
prompt - with recovery for any pending input - similar to the previous
case.

Otherwise, any input at the prompt will be sent to the iRuby process."
  ;; TBD compat with ruby 2.7
  (interactive)
  (let* ((initial (point))
         (buffer-end (goto-char (point-max)))
         (prompt-end (progn (iruby-start-of-input buffer-end)
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
       ;; This might generally be similar to
       ;;  `comint-stored-incomplete-input' (var) and
       ;;  `comint-restore-input' (func)
       ;; Those forms might be applied for in-place history navigation,
       ;; e.g for scrolling through earlier input with point at an input
       ;; field in an iRuby comint buffer.
       ;;
       ;; The forms implemented here are designed to be applied when
       ;; moving point across earlier input items in an iRuby comint
       ;; buffer. Considering the per-line approach to history recording
       ;; in comint, this provides at least one way to edit and reuse
       ;; any earlier multi-line input.
       ;;
       ;; This would be usable without reimplementing the input and
       ;; history-recording forms in comint, such as to perhaps resemble
       ;; those developed in recent versions of IRB.
       (iruby-stage-old-input)))))


;;
;; -----
;;

(defun iruby-directory-buffer (dir)
  (setq dir (expand-file-name dir))
  (catch 'buffer
    (dolist (buffer (mapcar #'cdr iruby-process-buffers))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (string= (expand-file-name default-directory) dir)
            (throw 'buffer buffer)))))))



(cl-defun iruby-get-active-buffer (&key (for-buffer (current-buffer))
                                     (live-p t) (console-ok t))
  "Return the active iRuby process buffer

This function performs the following checks, in sequence,
returning the first available buffer:
- the current buffer, if an `iruby-mode' buffer
- any non-nil binding for `iruby-buffer'
- the first buffer matched to a directory for `iruby-console-match',
  beginning at the present `default-directory'
- the `iruby-default-ruby-buffer'
- the first matching buffer in `iruby-process-buffers'

If `no-filter-live' is non-nil, then the iRuby process for the first
matched buffer must not be a closed process. Otherwise, a buffer may
match irrespective of whether the buffer's iRuby process is running

This function may return nil, if no iruby process is found in any buffer"
  (cl-flet ((check-buffer (buffer)
              (when buffer
                (if (not live-p)
                    buffer
                  (when (process-live-p (iruby-buffer-process buffer))
                    buffer)))))
    (with-current-buffer for-buffer
      (or (when (eq major-mode 'iruby-mode)
            (check-buffer for-buffer))

          (check-buffer iruby-buffer)

          (when console-ok
            (let ((console (iruby-find-console-buffer default-directory)))
              (check-buffer console)))

          (check-buffer iruby-default-ruby-buffer)

          (catch 'fallback
            ;; return the first live iruby process buffer
            (dolist (elt iruby-process-buffers)
              (let ((b (cdr elt)))
                (when (check-buffer b)
                  (throw 'fallback b)))))))))

;;; ad hoc tests
;; (iruby-get-active-buffer :live-p t)
;; (iruby-get-active-buffer :live-p nil)


(defun iruby-get-last-output (&optional proc)
  "Return the last output from the ruby process PROC as a string.

If PROC is nil, the value returned by `iruby-proc' will be used
as PROC

Known limitation: This function requires that any prompt string
in the ruby process, including any empty string, will have been
displayed within a single line of text, beginning at start of
line."
  (let* ((proc (iruby-proc))
         (buff (iruby-process-buffer proc))
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


(defun iruby-buffer-impl (buffer)
  (with-current-buffer buffer
    iruby-buffer-interactive-impl))

(defun iruby-process-impl (&optional whence)
  ;; utility function, used in `iruby-minor-modeline-default-label'
  (let ((buffer
          (cl-etypecase whence
            (process (iruby-process-buffer whence))
            (buffer whence)
            (string (or (cdr (cl-find whence iruby-process-buffers
                               :key #'(lambda (elt) (buffer-name (cdr elt)))
                               :test #'string=))
                        (error "Found no buffer for name %S in iruby-process-buffers"
                               whence)))
            (null (iruby-process-buffer (iruby-proc))))))
    (iruby-buffer-impl buffer)))

;;; ad-hoc test, assuming at least one iRuby process
;; (iruby-process-impl (caar iruby-process-buffers))
;;; similar - should return the same value as the previous
;; (iruby-process-impl (cdar iruby-process-buffers))
;;; this test assumes an iRuby process using ruby directly
;; (iruby-process-impl "*ruby*")

(defun iruby-minor-modeline-default-label (&optional buffer)
  "Return a modeline string for `iruby-minor-mode'

This function is used in the default value for the customization option,
`iruby-minor-modeline-format'

See also: Customization for `iruby-minor-mode-prefix', `iruby-app-name'"
  (when buffer (set-buffer buffer))
  (let ((impl (cond
                ((eq major-mode 'iruby-mode)
                 (iruby-process-impl (current-buffer)))
                (iruby-buffer (iruby-process-impl iruby-buffer))
                (iruby-default-ruby-buffer
                 (iruby-process-impl iruby-default-ruby-buffer)))))
    (cond
      (impl (concat iruby-minor-mode-prefix iruby-app-name ":"
                    (iruby:impl-name impl)))
      (t (concat iruby-minor-mode-prefix iruby-app-name)))))


(defun iruby-buffer-short-name (whence)
  ;; utility function for `iruby-read-process'
  (cl-block self
    (let ((name (cl-etypecase whence
                  (string whence)
                  (buffer (cond
                            ((buffer-live-p whence)
                             (iruby-buffer-short-name (buffer-name whence)))
                            (t (cl-return-from self "<buffer unavailable>"))))
                  (process (or (iruby-buffer-short-name (iruby-process-buffer whence))
                               (cl-return-from self "<buffer unavailable>"))))))
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

;; (iruby-buffer-short-name (let ((b (get-buffer-create "*test*"))) (kill-buffer b) b))

;;
;; utility forms (iruby-proc)
;;

(cl-defmacro with-iruby-process-environment ((&optional impl &rest bindings)
                                             &rest body)
  "evaluate BODY in an environment configured for a process environment

In applications, this macro will establish a local binding for the
Emacs variable `process-environment', then evaluating the BODY forms
within that Emacs Lisp environment.

IMPL will be evaluted as the first argument for a call to the EIEIO
generic function `iruby:get-initial-environment'.

Each element in BINDINGS should be a literal string or a form that will
evalute to either a string or nil value. Each non-nil value in BINDINGS
will be prepended to the value returned by the EIEIO generic function
`iruby:get-initial-environment' as called on the provided IMPL. In
effect, any provided BINDINGs will override any value returned by
`iruby:get-initial-environment'

For any process initialized as with comint or shell exec, the process
should then inherited the `process-environment' configured with this
macro."
  `(let ((process-environment
          (append
           (cl-remove-if 'null (list ,@bindings))
           (iruby:get-initial-environment ,impl))))
     ,@body))

;; (let ((iruby-pager "false")) (with-iruby-process-environment () (shell-command-to-string "echo -n $PAGER")))
;; => "false" (when evaluated with BASH or similar)
;; (with-iruby-process-environment (nil "PAGER=true") (shell-command-to-string "echo -n $PAGER"))
;; => "true" (when evaluted in an SH-like shell)

;;
;; interactive forms (for iruby-proc)
;;

(defvar iruby-process-history nil
  "History list for `iruby-read-process'")

(defun iruby-read-process (&optional prompt live-p)
  (cond
    ((null iruby-process-buffers)
     (error "No active iRuby processes"))
    ((cdr iruby-process-buffers)
     (let* ((prompt (or prompt "iRuby Process: "))
            (table (mapcan #'(lambda (elt)
                               (let ((p (car elt)))
                                 (when (or (null live-p)
                                           (process-live-p p))
                                   (let ((name (iruby-buffer-short-name p)))
                                     (when name
                                       (list (cons name p)))))))
                           iruby-process-buffers))
            (default (caar table))
            (selected
             (completing-read (format "%s (default: %s): " prompt default)
                              table nil t nil 'iruby-process-history default)))
       (cdr (assoc selected table))))
    (t
     ;; only one process registered
     (caar iruby-process-buffers))))

(defun iruby-read-process-interactive (&optional prompt live-p)
  (list (if current-prefix-arg
            (iruby-read-process prompt live-p)
          (iruby-buffer-process
           (iruby-get-active-buffer :live-p live-p)))))

(defun iruby-buffer-windows (&optional buffer frame)
  (let* ((for-buffer (cl-typecase buffer
                       (process (iruby-process-buffer buffer))
                       (t (iruby-get-active-buffer
                           :for-buffer (or buffer (current-buffer))
                           :live-p nil
                           :console-ok t))))
         (wdows (list nil))
         (wdows-next wdows))
    (walk-windows (lambda (wn)
                    (let ((wn-b (window-buffer wn)))
                      (when (eq wn-b buff)
                        (let ((new (list wn)))
                          (setf (cdr wdows-next) new)
                          (setq windows-next new)))))
                  nil frame)
    (cdr wdows)))

;;; ad hoc test
;; (iruby-process-windows (iruby-proc))

(defun iruby-switch-to-ruby (&optional process)
  "Switch to the active iRuby process buffer

If the iRuby process buffer denoted by `process' is displayed in
a buffer on the current frame, then switch to that buffer. Else, switch
to the iRuby process buffer using the window of the current buffer.

If called interactively, the default Ruby process will be used unless
called with an interactive prefix argument, in which case the user will
be queried to select a process to switch to.

See also:
`iruby-switch-to-ruby-other-window'
`iruby-switch-to-ruby-other-frame'
`iruby-use-ruby'"
  (interactive
   (iruby-read-process-interactive  "Switch to iRuby Process:"))
  (cl-block top
    (let* ((buff (cl-etypecase process
                   (process (iruby-process-buffer process))
                   (buffer process)
                   (null
                    (let ((it (iruby-get-active-buffer :live-p nil)))
                      (cond
                        (it it)
                        (t
                         (warn "iruby-switch-to-ruby: nil is not a process")
                         (cl-return-from top)))))))
           (windowlist (iruby-buffer-windows buff (selected-frame))))
    (cond
      (windowlist (select-window (car windowlist)))
      (t (switch-to-buffer buff))
      ))))

(defun iruby-switch-to-ruby-other-window (process)
  "If the iRuby process buffer denoted by `process' is displayed in
a buffer on the current frame, then switch to that buffer, else create
and switch to a new window for the iRuby process buffer

See also: `iruby-switch-to-ruby'"
  ;; FIXME redundant to original iruby-switch-to-... cmds
  (interactive (list (iruby-read-process "Switch to iRuby Process: ")))
  (switch-to-buffer-other-window (iruby-process-buffer process)))

(defun iruby-switch-to-ruby-other-frame (process)
  "If the iRuby process buffer denoted by `process' is displayed in
a buffer on the current frame, then switch to that buffer, else create
and raise a new frame for the iRuby process buffer

See also: `iruby-switch-to-ruby'"
  (interactive (list (iruby-read-process "Switch to iRuby Process: ")))
  (switch-to-buffer-other-frame (iruby-process-buffer process)))

(defun iruby-use-ruby (process buffer)
  "Select an iRuby process to use for the specified source buffer.

This function may be called interactively. If called with an interactive
prefix argument, the user will be queried as to whether to to use the
selected iRuby process buffer globally, or for which buffer to bind. If
called without an interactive prefix argument, this will select an iRuby
process only for the current buffer.

The `process' should be an Emacs process object, such that may be
determined with `iruby-read-process'.

If `buffer' is the symbol `t', this will set the global iRuby process
under `iruby-default-ruby-buffer'.

Otherwise, `buffer' should denote an active source buffer, typically a
buffer in some Ruby source file mode. In this case, the `process' will
be selected for storing the iRuby buffer of that process in the value of
`iruby-buffer' in the provided `buffer'."
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
  "Return the process status for a specified iRuby process buffer

`whence' may be provided as a string denoting a buffer name, or a buffer
object, or an Emacs process object.

If an iRuby process is associated with  `whence', this function returns
the `process-status' for that process, else nil.

See also: `iruby-process-running-p'"
  (let ((proc (cl-etypecase whence
                (string (iruby-buffer-process (get-buffer whence)))
                (buffer (iruby-buffer-process whence))
                (process whence))))
    (when proc
      (process-status proc))))

(defun iruby-process-running-p (whence)
  "Return true if a specified iRuby process is running.

This funion return trues if a process is associated with `whence' and
that process is in a 'run' state.

`whence' may be provided as a string denoting a buffer name, or a buffer
object, or an Emacs process object.

See also: `iruby-process-status'"
  (eq (iruby-process-status whence) 'run))

(defvar iruby-process-buffers nil
  "Internal storage for buffer/process mapping in iRuby

See also: functions `iruby-buffer-process' and `iruby-process-buffer'

For interactive forms, see also: `iruby-read-process'")


(defun iruby-process-buffer (process)
  "Return the buffer created for an iRuby process;

The provided PROCESS should be a process created such as with
`run-iruby-new'.

This function may differ with relation to `process-buffer', insofar as
for behaviors onto a closed process.

See also: `iruby-buffer-process'"
  (cdr (assq process iruby-process-buffers)))

(defun iruby-buffer-process (buffer)
  "Return the process created for an iRuby process buffer.

The provided BUFFER should have a `major-mode' of `iruby-mode', such
that the buffer may have been created with `run-iruby-new'.

This function should return any iRuby process originally recorded for
the buffer, whether an active or closed process;

This function is available for interactive forms in iRuby and is not
intended as a drop-in replacement for Emacs `get-buffer-process'.

See also: `iruby-process-buffer', `iruby-restart-process',
`iruby-switch-to-ruby'"
  (car (rassq buffer iruby-process-buffers)))

(defun iruby-add-process-buffer (process buffer)
  (setq iruby-process-buffers (cons (cons process buffer)
                                    iruby-process-buffers)))

(defun iruby-remove-process-buffer (whence)
  (let* ((proc (cl-etypecase whence
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
  ;; For a purpose of user interface support, closed processes will
  ;; generally remain in `iruby-process-buffers' until removed with this
  ;; function, such as via `kill-buffer-hook'.
  ;;
  ;; Once a buffer is removed from `iruby-process-buffers', the buffer
  ;; will no longer be available via `iruby-read-process' or any
  ;; interactive forms calling the same.
  ;;
  ;; As such, until when a buffer is removed from `iruby-process-buffers'
  ;; and the buffer is still a live buffer, the buffer can be selected
  ;; with normal Emacs forms or most iRuby interactive forms. Once the
  ;; buffer is selected, the original process in that buffer can be
  ;; restarted as with `iruby-restart-process'
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
           (iruby-buffer-process (iruby-get-active-buffer :live-p t)))))
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
                         (with-symbols-iruby (thread)
                           `(let ((,thread (make-thread
                                            (lambda () ,change-state-form)
                                            "iruby-restart(close)")))
                              (thread-join ,thread)
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
            (impl iruby-buffer-interactive-impl)
            (syntax iruby-buffer-syntax)
            (multibyte-p enable-multibyte-characters)
            (locals (cl-remove 'enable-multibyte-characters
                               (buffer-local-variables buff)
                               :key 'car :test 'eq))
            usebuff proc)
        (iruby-close-process process)
        ;; This calls `comint-exec' directly, without reinitilizing the
        ;; buffer as within `run-iruby-new'
        (with-iruby-process-environment (impl)
          (setq usebuff
                (comint-exec buff (process-name process)
                             (car cmd) nil (cdr cmd))
                proc (get-buffer-process usebuff)))
        (set-buffer usebuff) ;; to be sure ...
        (let ((iruby-default-ruby-syntax (or iruby-buffer-syntax
                                             iruby-default-ruby-syntax)))
          (iruby-mode)
          ;; reset local variables after iruby-mode
          (dolist (bind locals)
            (set (car bind) (cdr bind)))
          (when multibyte-p
            ;; `enable-multibyte-characters' cannot be set with 'set'
            (set-buffer-multibyte multibyte-p))
          (iruby-initialize-buffer-for impl syntax))
        (setf (car cached-data) proc)))))


(defun iruby-proc (&optional noerr)
  "Return the interactive Ruby process for the current buffer or project.

See also: `iruby-get-active-buffer'"
  (let ((buffer (iruby-get-active-buffer :live-p nil)))
    (or (when buffer (iruby-buffer-process buffer))
        (unless noerr
          (error "No current iRuby process.")))))

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
  (save-excursion
    (save-restriction
      (let ((m (process-mark proc))
            (term (format iruby-send-terminator (random)
                          (time-convert nil 'integer))))
        (set-buffer (marker-buffer m))
        (let* ((hdr
                ;; NB reuse 'term' as a header/footer marker for the
                ;; reply, and a key onto a callback form in `iruby-preoutput-filter'
                (format "eval <<'%s', %s , '%s', 0x%x\n"
                        ;; FIXME needs test for nil impl binding expr
                        term
                        (or iruby-buffer-binding-expr "nil")
                        (or file "(Unknown)")
                        (or (and (integerp line) (>= line 0) line)
                            0)))
               (tlr
                (concat "\n" term "\n"))
               (send-str (concat hdr str tlr)))
          (goto-char m)
          (insert "\n")
          (set-marker m (point))
          (iruby-debug-info "Sending string to %s: %S"
                            (ignore-errors (iruby:impl-name iruby-buffer-interactive-impl))
                            send-str)
          (comint-send-string proc send-str)
          )))))

;; ad hoc tests
;;
;; (iruby-send-string (iruby-proc) "def a; end")
;; (iruby-get-last-output)
;;
;;  ^ FIXME odd behavior when $DEBUG = true in the iruby process,
;;     seming like a partial failure in the lexer though it shows a
;;     correct return value after the exception
;;
;;     - needs a longer backtrace in irb
;;
;;
;; expecting a warning from the sub-ruby on redefinition of a
;; constant
;;
;; (dotimes (n 2 nil) (iruby-send-string (iruby-proc) "module ABC; D=:EF; end"))
;; (iruby-get-last-output)
;;
;; ^ FIXME this second test may show a quirk in iruby-get-last-output.
;;   Here, iruby-get-last-output will capture the output after both of
;;   the expressions to the Ruby process, together with the prompt
;;   string conctatenated before the second expression.

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
as a feature of comint output processing. The value
of `output' will be the string that was received by comint.
The value returned by this function will be applied for
further output processing by comint, as the text to be
inserted to the iruby-mode buffer

See also: `comint-preoutput-filter-functions' [variable]"
  ;; policy, similar to comint:
  ;; - last line of output will be parsed as a prompt
  ;;
  ;; FIXME this does not parse for and fontify any "return" section
  ;; in the output, not for the output text itself or any "return"
  ;; prompt string under any single interactive framework (IRB, pry, ...)
  (with-temp-buffer
    (insert output)
    (end-of-buffer)
    (let ((eob (point))
          last-line
          last-text)
      (beginning-of-line)
      (setq last-line (point)
            last-text (buffer-substring last-line eob))
      (cond
        ((string-match "^[[:space:]]*$" last-text)
         ;; this matches the blank continued input prompt for multi-line input
         ;; in irb with "--inf-ruby-mode" prompts, generally.
         ;;
         ;; if user pastes input with text properties, it may interfere
         ;; with the iruby parser functions that would rely on properties
         ;; added here.
         (put-text-property last-line eob 'iruby-continuation t (current-buffer)))
        (t
         (put-text-property last-line eob 'iruby-prompt t (current-buffer))))
      (buffer-substring (point-min) eob))))

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
  "Send the current definition to the interactive Ruby process."
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
will be handled in `iruby-output-filter'

See also: `iruby-strip-pending-input"))


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
  (let ((use-buffer (iruby-get-active-buffer :for-buffer (or buffer
                                                             (current-buffer))
                                             :live-p t)))
    (with-current-buffer use-buffer
      (when buffer
        (unless (eq major-mode 'iruby-mode)
          (error "Not an iruby-mode buffer: %s" buffer)))
      (save-mark-and-excursion
        (save-restriction
          (let* ((buffer-end (point-max))
                 (prompt-end
                  (progn (iruby-start-of-input buffer-end)
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
            (point)))))))


(defun iruby-send-last-thing (thing point)
  "Send the previous thing at `point' to the interactive Ruby process.
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
                (buff (iruby-get-active-buffer :live-p t)))
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
  "Send the current block at POINT to the interactive Ruby process."
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

If non-nil, this value may be used as a default when the buffer-local
variable `iruby-buffer' is nil in the current buffer")


(defun iruby-remember-ruby-buffer (buffer)
  (setq iruby-default-ruby-buffer buffer))


(defun iruby-switch-to-default-buffer ()
  "Switch to the default Ruby buffer.

See also: `iruby-use-ruby', `iruby-switch-to-ruby'"
  (interactive)
  (if (and iruby-default-ruby-buffer
           (buffer-live-p iruby-default-ruby-buffer))
      (pop-to-buffer iruby-default-ruby-buffer)
    (message "Default Ruby buffer is unavailable")))


(defun iruby-send-region-and-go (start end)
  "Send the current region to the interactive Ruby process.
Then switch to the process buffer."
  (interactive "r")
  (iruby-send-region start end)
  (iruby-switch-to-inf t))


(defun iruby-send-definition-and-go ()
  "Send the current definition to the interactive Ruby.
Then switch to the process buffer."
  (interactive)
  (iruby-send-definition)
  (iruby-switch-to-inf t))


(defun iruby-send-block-and-go ()
  "Send the current block to the interactive Ruby.
Then switch to the process buffer."
  (interactive)
  (iruby-send-block)
  (iruby-switch-to-inf t))


(defun iruby-load-file (file-name &optional process)
  "Load a Ruby file into the interactive Ruby process.

The interactive form will store history data in the variable
`iruby-load-file-history' when `iruby-load-file-history-limit' is
non-zero.

The file will be loaded by the active Ruby process for the current
buffer, or that for the global iRuby environment. This proceses may be
selected previous to `iruby-load-file', with the command
`iruby-use-ruby'

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
  "Send the current buffer to the interactive Ruby process."
  (interactive)
  (save-restriction
    (widen)
    (iruby-send-region (point-min) (point-max))))

(defun iruby-send-buffer-and-go ()
  "Send the current buffer to the interactive Ruby process.
Then switch to the process buffer."
  (interactive)
  (iruby-send-buffer)
  (iruby-switch-to-inf t))

(defun iruby-send-line ()
  "Send the current line to the interactive Ruby process."
  (interactive)
  (save-restriction
    (widen)
    (iruby-send-region (point-at-bol) (point-at-eol))))

(defun iruby-send-line-and-go ()
  "Send the current line to the interactive Ruby process.
Then switch to the process buffer."
  (interactive)
  (iruby-send-line)
  (iruby-switch-to-inf t))

;; FIXME debug (emacs 29)
;; (defun iruby-smie--forward-token ()
;;   (let ((inhibit-field-text-motion t))
;;     (ruby-smie--forward-token)))
;;
;; (defun iruby-smie--backward-token ()
;;   (let ((inhibit-field-text-motion t))
;;     (ruby-smie--backward-token)))

;;
;; after-load-alist handling for iruby-desktop.el
;;

(defun iruby-load-desktop-support ()
  (interactive)
  (require 'iruby-desktop))

(defmacro iruby-preload-desktop-support ()
  (interactive)
  (with-symbols-iruby (found)
    `(let ((,found (assq 'desktop after-load-alist)))
       (cond
         (,found (cl-pushnew 'iruby-load-desktop-support
                             (cdr ,found)  :test #'eq))
         (t (add-to-list 'after-load-alist
                         '(desktop iruby-load-desktop-support)))))))

(iruby-preload-desktop-support)

(provide 'iruby)

;; avoiding cicular dep errors by requiring all symmetrically dependent
;; libraries afer the provide call
(require 'iruby-impl)
(require 'iruby-proc)
(require 'iruby-complete)
(require 'iruby-compile)
(require 'iruby-console)

;;; iruby.el ends here
