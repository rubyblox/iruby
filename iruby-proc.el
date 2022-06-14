;;; iruby-proc.el ---- process handling forms for iRuby

(require 'iruby-impl)

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

(provide 'iruby-proc)
