;;; iruby-desktop.el --- desktop integration for iRuby buffers

(eval-when-compile
  (require 'cl-macs))

(require 'desktop)

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
                    (iruby-get-interactive-cmd iruby-default-implementation))))
         ;; FIXME an implementation object cannot be stored here
         ;; and there is no cl-make-load-form in emacs.
        (impl (or (cdr (assq 'iruby-buffer-impl desktop-buffer-locals))
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
      (let* ((procbuff (run-iruby-new cmd impl)) ;; point of call NB
             ;; (FIXME needs environment bindings)
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
- :impl => `iruby-buffer-impl', needed for later restoring
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

    (append (list (cons :impl iruby-buffer-impl)
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

(provide 'iruby-desktop)
