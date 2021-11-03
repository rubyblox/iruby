;;; inf-ruby.el --- Run a Ruby process in a buffer

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
;; Version: 2.6.1

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

(defcustom iruby-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :type 'boolean
  :group 'iruby)


(defcustom iruby-ruby-irb-prefix '("-r" "irb" "-r" "irb/completion"
                                      "-e" "IRB.start" "--")
  "List of prefix arguments for running irb via ruby

The last element in this list should generally be the string \"--\""
  :type '(repeat string)
  :group 'iruby)


(defcustom iruby-implementations
  '(("irb"      iruby--impl-cmd-list
     "--prompt" "default" "-r" "irb/completion")
    ;; NB concerning behaviors of `iruby--impl-cmd-list':
    ;;
    ;; When launching irb via a "ruby" cmd, e.g "ruby" or "ruby27" etc,
    ;; the ruby cmd will receive arguments in `iruby-ruby-irb-prefix'.
    ;; The args listed for irb, as here, will then be added after the
    ;; trailing "--" in `iruby-ruby-irb-prefix'
    ;;
    ("ruby"     iruby--impl-cmd-list "--prompt" "default")
    ("jruby"    . "jruby -S irb --prompt default --noreadline -r irb/completion")
    ("rubinius" . "rbx -r irb/completion")
    ("yarv"     . "irb1.9 -r irb/completion")
    ("macruby"  . "macirb -r irb/completion")
    ("pry"      . "pry"))
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
  :type '(repeat (cons string (choice string symbol (repeat (choice string symbol)))))
  :group 'iruby)

(defcustom iruby-default-implementation "ruby"
  "Which Ruby implementation to use if none is specified."
  :type `(choice ,@(mapcar (lambda (item) (list 'const (car item)))
                           iruby-implementations))
  :group 'iruby)


(defun iruby--parse-impl-cmd (&optional impl)
  (let* ((%impl (or impl iruby-default-implementation))
         (cmd (cdr (assoc %impl iruby-implementations))))
    (cl-labels ((join (argv)
                  (etypecase argv
                    (string argv)
                    (list (mapconcat #'identity argv " "))))
                (parse (impl elt)
                  (typecase elt
                    (cons
                     (join (mapcar #'(lambda (%elt)
                                       (join (parse impl %elt)))
                                   elt)))
                    (list ;; NB value is 'nil' here
                     (error "Uknown ruby implementation: %s" impl))
                    (symbol (funcall elt impl))
                    (string elt)
                    (t (error "Unknown command syntax for implementation %S: %S"
                              impl elt)))))
      (parse %impl cmd))))

;; (iruby--parse-impl-cmd)
;; (iruby--parse-impl-cmd "jruby")

(defun iruby--impl-cmd-list (name &optional rest-args)
  ;; NB This will resuse the implementation name as the command name for
  ;; the implemmentation.
  ;;
  ;; The implementation name would generally be provided as a CAR in
  ;; some element of `iruby-implementations', with this function's
  ;; name then provided as either the CDR or the first element in the
  ;; CDR of that element of `iruby-implementations'.
  ;;
  ;; This function should accept e.g "irb27" or "ruby-dev" as an
  ;; implementation name, then returning command line argument values in
  ;; a manner similar to the "irb" or "ruby" case, respectively. 
  (let ((cmd-name name))
    (cond
      ((string-match "^irb" cmd-name)
       (cons cmd-name (cons (iruby--irb-input-arg (cons cmd-name rest-args))
                            rest-args)))
      ((string-match "^ruby" cmd-name)
       (let ((%prefix (cons cmd-name iruby-ruby-irb-prefix)))
         (append %prefix (list (iruby--irb-input-arg %prefix))
                 rest-args)))
      ;; does not match "ruby" or "irb" - returning the impl name as the
      ;; cmd and the list of irb args, as-is
      (t (cons cmd-name rest-args)))))

(defun iruby--irb-input-arg (cmdlist)
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


(defcustom iruby-console-environment 'ask
  "Envronment to use for the `iruby-console-*' commands.
If the value is not a string, ask the user to choose from the
available ones.  Otherwise, just use the value.

Currently only affects Rails and Hanami consoles."
  :type '(choice
          ;; "the available ones" ??
          (const ask :tag "Ask the user")
          (string :tag "Environment name")))

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
(defvar iruby-source-modes '(ruby-mode enh-ruby-mode)
  "Used to determine if a buffer contains Ruby source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a ruby source file by `iruby-load-file'.
Used by these commands to determine defaults.")

(defvar iruby-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last `iruby-load-file' command.
Used for determining the default in the
next one.")

(defvar iruby-at-top-level-prompt-p t)
(make-variable-buffer-local 'iruby-at-top-level-prompt-p)

(defvar iruby-last-prompt nil)
(make-variable-buffer-local 'iruby-last-prompt)

(defconst iruby-error-regexp-alist
  '(("^SyntaxError: \\(?:compile error\n\\)?\\([^\(].*\\):\\([1-9][0-9]*\\):" 1 2)
    ("^\tfrom \\([^\(].*\\):\\([1-9][0-9]*\\)\\(:in `.*'\\)?$" 1 2)))

;;;###autoload
(defun iruby-setup-keybindings ()
  "Hook up `iruby-minor-mode' to each of `ruby-source-modes'."
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

;;;###autoload
(define-minor-mode iruby-minor-mode
  "Minor mode for interacting with the inferior process buffer.

The following commands are available:

\\{iruby-minor-mode-map}"
  :lighter "" :keymap iruby-minor-mode-map)

(defvar iruby-buffer nil "The oldest live Ruby process buffer.")

(defvar iruby-buffers nil "List of Ruby process buffers.")

(defvar iruby-buffer-command nil "The command used to run Ruby shell")
(make-variable-buffer-local 'iruby-buffer-command)

(defvar iruby-buffer-impl-name nil "The name of the Ruby shell")
(make-variable-buffer-local 'iruby-buffer-impl-name)

(define-derived-mode iruby-mode comint-mode "Iruby"
  "Major mode for interacting with an inferior Ruby REPL process.

A simple IRB process can be fired up with \\[iruby].

To launch a REPL with project-specific console instead, type
\\[iruby-console-auto].  It recognizes several
project types, including Rails, gems and anything with `racksh'
in their Gemfile.

 oCustomization: When entered, this mode runs `comint-mode-hook' and
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
    (setq iruby-last-prompt (car (last (split-string output "\n")))
          iruby-at-top-level-prompt-p
          (string-match iruby-first-prompt-pattern
                        iruby-last-prompt))))

;; adapted from replace-in-string in XEmacs (subr.el)
(defun iruby-remove-in-string (str regexp)
  "Remove all matches in STR for REGEXP and returns the new string."
  (let ((rtn-str "") (start 0) match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
            start (match-end 0)
            rtn-str (concat rtn-str (substring str prev-start match))))
    (concat rtn-str (substring str start))))

(defun iruby-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (re-search-backward iruby-first-prompt-pattern)
      (iruby-remove-in-string (buffer-substring (point) end)
                                 iruby-prompt-pattern))))

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
                           nil t))
         (if (and (stringp txt) (zerop (length txt)))
             iruby-default-implementation
           txt))))

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
  (setq impl (or impl "ruby"))
  (let ((command (iruby--parse-impl-cmd impl)))
    (run-iruby command impl)))

;; (run-iruby-new (funcall (cdar iruby-implementations)) "ruby")

;;;###autoload
(defun run-iruby (&optional command name)
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
                           (iruby--parse-impl-cmd iruby-default-implementation)
                         cmd))))
  (let* ((%command (or command (iruby--parse-impl-cmd)))
         (%name (or name (file-name-nondirectory
                          (car (split-string-and-unquote %command))))))
    (run-iruby-or-pop-to-buffer %command %name
                                (or (iruby-buffer)
                                    iruby-buffer))))

(defun run-iruby-new (command &optional name)
  "Create a new inferior Ruby process in a new buffer.

COMMAND is the command to call. NAME will be used for the name of
the buffer, defaults to \"ruby\"."
  ;;
  ;; used in `run-iruby-or-pop-to-buffer'
  ;; which is used by both of `run-iruby' and `iruby-console-run'
  ;;
  (setq name (or name "ruby"))

  (let ((commandlist
         ;; FIXME Need to preserve the list form here
         (split-string-and-unquote command))
        (buffer (current-buffer))
        (process-environment process-environment)
        (comint-delimiter-argument-list
         ;; ?? NB add all mathematical symbols, ...
         '(?\| ?& ?< ?> ?\( ?\) ?\;)))
    ;; http://debbugs.gnu.org/15775
    (setenv "PAGER" (executable-find "cat"))
    ;; ^ FIXME not in the main Emacs process
    ;;
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
    (set-buffer (apply 'make-comint-in-buffer
                       name
                       (generate-new-buffer-name (format "*%s*" name))
                       (car commandlist)
                       nil (cdr commandlist)))
    (iruby-mode)
    (iruby-remember-ruby-buffer buffer)
    (push (current-buffer) iruby-buffers)
    (setq iruby-buffer-impl-name name
          iruby-buffer-command command))

  (unless (and iruby-buffer (comint-check-proc iruby-buffer))
    (setq iruby-buffer (current-buffer)))

  (pop-to-buffer (current-buffer)))

(defun run-iruby-or-pop-to-buffer (command &optional name buffer)
  ;; NB used in
  ;; - `run-iruby'
  ;; - `iruby-console-run'
  (if (not (and buffer
                (comint-check-proc buffer)))
      ;; FIXME support a list syntax for args to 'command' ?
      (run-iruby-new command name)
    (pop-to-buffer buffer)
    (unless (and (string= iruby-buffer-impl-name name)
                 (string= iruby-buffer-command command))
      (error (concat "Found iruby buffer, but it was created using "
                     "a different NAME-COMMAND combination: %s, `%s'")
             iruby-buffer-impl-name
             iruby-buffer-command))))

(defun iruby-proc ()
  "Return the inferior Ruby process for the current buffer or project.

See variable `iruby-buffers'."
  (or (get-buffer-process (if (eq major-mode 'iruby-mode)
                              (current-buffer)
                            (or (iruby-buffer)
                                iruby-buffer)))
      (error "No current process. See variable iruby-buffers")))

;; These commands are added to the iruby-minor-mode keymap:

(defconst iruby-send-terminator "--iruby-%x-%d-%d-%d--"
  "Template for irb here document terminator.
Must not contain ruby meta characters.")

(defconst iruby-eval-binding
  (concat "(defined?(IRB) && IRB.conf[:MAIN_CONTEXT] && IRB.conf[:MAIN_CONTEXT].workspace.binding) || "
          "(defined?(Pry) && Pry.toplevel_binding)"))

(defconst iruby-eval-separator "")

(defun iruby-send-string (str)
  "Send the provided string for evaluation as a ruby expression, in the
irb process buffer.

See also: commands `iruby-send-region', `iruby-send-definition',
`iruby-send-block', other iruby-send commands, and `iruby-show-result'"
  (let ((term (apply 'format iruby-send-terminator
                     (random) (current-time)))
        (proc (iruby-proc)))
    (save-excursion
      (save-restriction
        ;; compilation-parse-errors parses from second line.
        (let ((m (process-mark proc)))
          (set-buffer (marker-buffer m))
          (goto-char m)
          (insert iruby-eval-separator "\n")
          (set-marker m (point)))
        (comint-send-string proc (format "eval <<'%s', %s;\n"
                                         term iruby-eval-binding))
        (comint-send-string proc str)
        (comint-send-string proc (concat "\n" term "\n"))))))

;; test
;; (iruby-send-string "def a; end")
;; (iruby-get-result)


(defun iruby-get-result ()
  "Return the result of the last evaluation in the ruby process buffer,
as a string

If evaluation has failed, this function will present an error in Emacs
with the message from the failed evaluation.

See also: `iruby-show-result' [command], `iruby-print-result' [command]
and `iruby-eval-binding' [constant]"
  ;; FIXME may accept an additional arg to indicate which process buffer
  ;; to use, such that would be passed from each calling function, for
  ;; the instance of an Emacs with multiple concurrent IRB sessions.
  (let ((proc (iruby-proc)))
    (with-current-buffer (or (iruby-buffer)
                             iruby-buffer)
      (while (not (and comint-last-prompt
                       (goto-char (car comint-last-prompt))
                       (looking-at iruby-first-prompt-pattern)))
        (accept-process-output proc))
      (re-search-backward iruby-prompt-pattern)
      (or (re-search-forward " => " (car comint-last-prompt) t)
          ;; Evaluation seems to have failed.
          ;; Try to extract the error string.
          (let* ((inhibit-field-text-motion t)
                 (s (buffer-substring-no-properties (point) (line-end-position))))
            (while (string-match iruby-prompt-pattern s)
              (setq s (replace-match "" t t s)))
            ;; FIXME send the error text to a pop-up buffer
            (error "%s" s)))
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun iruby-show-result ()
  "Display the result of the last evaluation, using the minibuffer.

If the evaluation has failed, a corresponding error will be
produced in Emacs.

The result may also be displayed in the *Messages* buffer

See also: `iruby-print-result', `iruby-get-result'"
  ;; FIXME for multi-line results, use a pop-up - similar to magit
  (interactive)
  (message "%s" (iruby-get-result)))


(defun iruby-print-result ()
  "Print the result of the last evaluation to the current buffer

If the evaluation has failed, a corresponding error will be
produced in Emacs.

See also: `iruby-show-result', `iruby-get-result'"
  ;; FIXME for multi-line results, use a pop-up - similar to magit
  (interactive)
  (print (iruby-get-result)
         (current-buffer)))


(defun iruby-send-region (start end &optional showp)
  "Send a region of text from the current buffer to the inferior Ruby process.

When called interactively, this operates on any current region in the
buffer. Prefix argumet will provide the value of `print'"
  (interactive "r\nP")
  (iruby-send-string (buffer-substring start end))
  (when showp
    (iruby-show-result)))

(defun iruby-send-definition (&optional showp)
  "Send the current definition to the inferior Ruby process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (ruby-beginning-of-defun)
      (iruby-send-region (point) end showp))))

(defun iruby-send-last-sexp (&optional showp)
  "Send the previous sexp to the inferior Ruby process."
  (interactive "P")
  (iruby-send-region (save-excursion (ruby-backward-sexp) (point))
                    (point)
                    showp))

(defun iruby-send-block (&optional showp)
  "Send the current block to the inferior Ruby process."
  (interactive "P")
  (save-excursion
    (ruby-end-of-block)
    (end-of-line)
    (let ((end (point)))
      (ruby-beginning-of-block)
      (iruby-send-region (point) end showp))))

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
    (message "Don't know the original Ruby buffer")))

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

(defun iruby-load-file (file-name)
  "Load a Ruby file into the inferior Ruby process."
  (interactive (comint-get-source "Load Ruby file: " iruby-prev-l/c-dir/file
                                  iruby-source-modes t)) ;; T because LOAD needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq iruby-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                     (file-name-nondirectory file-name)))
  (comint-send-string (iruby-proc) (concat "(load \""
                                              file-name
                                              "\"\)\n")))

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
         iruby-at-top-level-prompt-p)
    (unless (equal "(rdb:1) " iruby-last-prompt)
      (set-process-filter proc (lambda (proc string) (setq kept (concat kept string))))
      (unwind-protect
          (let ((completion-snippet
                 (format
                  (concat
                   "proc { |expr, line|"
                   "  require 'ostruct';"
                   "  old_wp = defined?(Bond) && Bond.started? && Bond.agent.weapon;"
                   "  begin"
                   "    Bond.agent.instance_variable_set('@weapon',"
                   "      OpenStruct.new(:line_buffer => line)) if old_wp;"
                   "    if defined?(_pry_.complete) then"
                   "      puts _pry_.complete(expr)"
                   "    elsif defined?(pry_instance.complete) then"
                   "      puts pry_instance.complete(expr)"
                   "    else"
                   "      completer = if defined?(_pry_) then"
                   "        Pry.config.completer.build_completion_proc(binding, _pry_)"
                   "      elsif old_wp then"
                   "        Bond.agent"
                   "      elsif defined?(IRB::InputCompletor::CompletionProc) then"
                   "        IRB::InputCompletor::CompletionProc"
                   "      end and puts completer.call(expr).compact"
                   "    end"
                   "  ensure"
                   "    Bond.agent.instance_variable_set('@weapon', old_wp) if old_wp "
                   "  end "
                   "}.call('%s', '%s')\n")
                  (iruby-escape-single-quoted expr)
                  (iruby-escape-single-quoted line))))
            (process-send-string proc completion-snippet)
            (while (and (not (string-match iruby-prompt-pattern kept))
                        (accept-process-output proc 2)))
            (setq completions (butlast (split-string kept "\r?\n") 2))
            ;; Subprocess echoes output on Windows and OS X.
            (when (and completions (string= (concat (car completions) "\n") completion-snippet))
              (setq completions (cdr completions))))
        (set-process-filter proc comint-filt)))
    (mapcar
     (lambda (str)
       (substring str prefix-offset))
     completions)))

(defconst iruby-ruby-expr-break-chars " \t\n\"\'`><,;|&{(")

(defun iruby-completion-bounds-of-prefix ()
  "Return bounds of expression at point to complete."
  (let ((iruby-ruby-expr-break-chars
         (concat iruby-ruby-expr-break-chars ".")))
    (iruby-completion-bounds-of-expr-at-point)))

(defun iruby-completion-bounds-of-expr-at-point ()
  "Return bounds of expression at point to complete."
  (when (not (memq (char-syntax (following-char)) '(?w ?_)))
    (save-excursion
      (let ((end (point)))
        (skip-chars-backward (concat "^" iruby-ruby-expr-break-chars))
        (cons (point) end)))))

(defun iruby-completion-expr-at-point ()
  "Return expression at point to complete."
  (let ((bounds (iruby-completion-bounds-of-expr-at-point)))
    (and bounds
         (buffer-substring (car bounds) (cdr bounds)))))

(defun iruby-completion-at-point ()
  "Retrieve the list of completions and prompt the user.
Returns the selected completion or nil."
  (let ((bounds (iruby-completion-bounds-of-prefix)))
    (when bounds
      (list (car bounds) (cdr bounds)
            (when iruby-at-top-level-prompt-p
              (if (fboundp 'completion-table-with-cache)
                  (completion-table-with-cache #'iruby-completions)
                (completion-table-dynamic #'iruby-completions)))))))

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
        (funcall iruby-orig-compilation-mode)
        (setq mode-line-process orig-mode-line-process)
        (set (make-local-variable 'compilation-arguments) arguments)
        (set (make-local-variable 'compilation-error-regexp-alist) errors)
        (when proc
          (set-process-filter proc filter)))
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
             (when (iruby--irb-needs-nomultiline-p)
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
    (when (iruby--irb-needs-nomultiline-p)
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

;;;###autoload (dolist (mode iruby-source-modes) (add-hook (intern (format "%s-hook" mode)) 'iruby-minor-mode))

(provide 'iruby)
;;; iruby.el ends here
