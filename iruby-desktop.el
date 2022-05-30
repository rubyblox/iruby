;;; iruby-desktop.el --- desktop integration for iRuby buffers

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

;;
;; % Overview
;;
;; This file provides configuration for store/restore of iRuby
;; process buffers under interactive `desktop-save' and `desktop-read'
;; functions as defined in desktop.el
;;
;; The main configuration will be applied when the user calls
;; `iruby-ensure-desktop-support', whether interactively or
;; via init-file.
;;
;; Recommended configuration: Add the following to `user-init-file'
;;
;;   (autoload 'iruby-ensure-desktop-support "iruby-desktop" nil t)
;;   (add-hook 'after-init-hook 'iruby-ensure-desktop-support)
;;
;; This will call `iruby-ensure-desktop-support' after the user
;; init file has been evaluated.
;;
;; If the `desktop' feature is not available during `ater-init-hook',
;; then the function `iruby-ensure-desktop-support' will ensure that
;; an element exists on `after-load-alist' such that the function
;; i.e  `iruby-ensure-desktop-support' will be called again after
;; the desktop' library has been loaded into Emacs.

(eval-when-compile
  (require 'cl-macs))


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
  (let* ((default-directory
          ;; should be inherited by any new process
          (or (cdr (assq 'default-directory desktop-buffer-locals))
              (cdr (assq :dir data))
              default-directory))
         (impl-data (cdr (assq :impl-data data)))
         (impl (cond
                 ((and (consp impl-data) (symbolp (car impl-data)))
                  (apply 'make-instance impl-data))
                 (impl-data impl-data)
                 (t
                  (progn
                    (warn "No implementation data stored for buffer %S. using default"
                          (current-buffer))
                    (iruby:default-interactive-ruby)))))
         (cmd (iruby:parse-cmd impl))
         (default-p (cdr (assq :default-p data)))
         (mapped (or (cdr (assq :mapped data))
                     (cdr (assq 'iruby-mapped-source-buffers desktop-buffer-locals)))))

    (unless (boundp 'erm-full-parse-p)
      ;; FIXME ensure that this variable is not unbound,
      ;; if enh-ruby-mode will be used in the buffer
      (make-variable-buffer-local 'erm-full-parse-p)
      (setq-default erm-full-parse-p nil))

    (let* ((procbuff (run-iruby-new impl (iruby:impl-name impl)))
           (proc (get-buffer-process procbuff)))
      (when mapped
        (with-current-buffer procbuff
          ;; store buffer mapping data for this process, to be
          ;; accessed in `iruby-map-desktop-process-buffers'
          ;; from `desktop-after-read-hook'
          (setq iruby-mapped-source-buffers mapped)))
      (when default-p
        (push '(:default-p . t)  iruby-mapped-misc-data))
      (switch-to-buffer procbuff))))

(defun iruby-desktop-misc-data (deskdir)
  "Callback function for `desktop-save' under iRuby process buffers

This function's name will usually be set as the value of the buffer-local
variable `desktop-save-buffer' in any iRuby process buffer.

This function returns data to be stored during `desktop-save' for later
initialization of an iRuby process buffer as during `desktop-read'. It's
assumed that this function will be called with an iRuby process
buffer as the current buffer, during `desktop-save'.

This function returns an associative list representing a mapping for
the following values in each iRuby process buffer, i.e each buffer
with a major-mode `iruby-mode'

- :impl-data => A literal value as returned by the generic function
  `iruby:make-desktop-load-form' when called on the interactive
  implementation object for the original iRuby process buffer.

  The syntax of this value may vary as dependent on the class of the
  implementation.

  If the implementation was provdied as a string or a list, indicating a
  console shell command, then the value of the :impl-data field will
  typically represent that original string or list value.

  If the implementation was provided as an `iruby:impl' then the value
  of the :impl-data field will typically be a list object, of the
  following syntax. The value should have a `car' denoting a constructor
  function for the class of the orignal object. The `cdr' of the value
  would be returned in a plist syntax, providing a list of initialization
  arguments. The :impl-data value in this instance would represent an
  Emacs Lisp form, such that may be applied as to initialize a generally
  equivalent `iruby:impl' object.

  As one known limitation in the `iruby:impl' instance, an object
  initialized from this :impl-data value may not be exactly equivalent
  to any object in `iruby-interactive-impls', though providing the
  behaviors of an interactive implementation of an equivalent iRuby
  class.

- :dir => `default-directory' for the buffer, in Emacs. This may
  or may not match the CWD for the Ruby process at the time of the
  call to `desktop-save'

- :mapped => list of buffer names, for buffers where `iruby-buffer' has
  been set as eq to the selected iRuby process buffer. This will be
  applied during `desktop-read' with an assumption that the buffer names
  for iRuby source buffers will remain consistent across the initial
  call to `desktop-save' and any later call to `desktop-read'. Otherwise,
  each non-matching buffer would not have the original `iruby-buffer'
  binding restored.

- :default-p => presented with a non-nil CDR in the return value, if
  the buffer was in  use as the `iruby-default-ruby-buffer' as visible
  from within the process buffer itself. Generally, that variable will
  have a global binding in Emacs - if non nil, then identifying the
  default Ruby process buffer to use for any Ruby source-mode buffer
  that has a nil `iruby-buffer'

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

    (append (list (cons :impl-data
                        (iruby:make-desktop-load-form iruby-buffer-interactive-impl))
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
  (let ((mapped (list nil)) ;; mapped: local storage & return
        (buffers (buffer-list)))
    (require 'iruby)
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

;;;###autoload
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
         ((cdr elt) (push form (cdr (last elt))))
         (t (push (list 'desktop form) after-load-alist))
       )))))


(provide 'iruby-desktop)
