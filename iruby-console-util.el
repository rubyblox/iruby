;;; iruby-console.util --- utility forms for iruby-console

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

(require 'eieio)
(require 'eieio-base)


(defvar iruby-console-patterns-alist
  ;; FIXME reimplement for returning an actual console object when
  ;; parsing with this table
  ;; - `iruby-console-get-impl'
  '((iruby-console-rails-p . rails)
    (iruby-console-hanami-p . hanami)
    (iruby-console-racksh-p . racksh)
    (iruby-console-script-p . script)
    ("\\.zeus\\.sock$" . zeus)
    ("^Gemfile$" . default)
    ("\\.gemspec$" . gem))
  "Mapping from predicates (regexp patterns or functions) to type
symbols. `iruby-console-auto' walks up from the current directory until
one of the predicates matches, then calls `iruby-console-TYPE',
passing it the found directory.")

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

(defun iruby-console-match (dir)
  "Find matching console command for DIR, if any."
  (catch 'type
    (dolist (pair iruby-console-patterns-alist)
      (let ((default-directory dir)
            (pred (car pair)))
        (when (if (stringp pred)
                  (iruby-expand-files pred)
                (funcall pred))
          (throw 'type (cdr pair)))))))


(defun iruby-console-read-directory (type)
  ;; a utility for interactive input
  (let ((predicate (car (rassq type iruby-console-patterns-alist)))
        (dir (read-file-name
              (format "Initial directory for console seach (%s): " type)
              nil nil t nil 'file-directory-p)))
    (or  (iruby-console-match dir)
         (error "No matching directory for %s console found"
                (capitalize (symbol-name type))))))


(cl-defun iruby-console-walk-dirs (&optional (start default-directory)
                                     (table iruby-console-patterns-alist))
  (let* ((re-tests-ptr (list nil))
         (re-tests re-tests-ptr)
         (func-tests-ptr (list nil))
         (func-tests func-tests-ptr))
    ;; perform some initial validation on each element in
    ;; `iruby-console-patterns-alist', furthermore creating
    ;; a table for (A) the function-type tests, which are called
    ;; in effect on a directory (by way of default-directory)
    ;; and (B) string-type tests, which is each a regexp to be
    ;; tested on an individual filename.
    (dolist (test table)
      (let ((datum (car test)))
        (typecase datum
          (string (iruby-rconc (list test) re-tests))
          (function (iruby-rconc (list test) func-tests))
          (t (error "Unknown file predicate: %S" datum)))))
    (setq re-tests (cdr re-tests-ptr))
    (setq func-tests (cdr func-tests-ptr))
    (cl-labels ((stop-dir-p (dir)
                  ;; NB does not check if dir is "/"
                  (string-match-p locate-dominating-stop-dir-regexp dir))
                (match-dir-p (dir)
                  (cl-block matched
                    (let ((default-directory dir))
                      (dolist (test func-tests)
                        (let ((predicate (car test))
                              (type (cdr test)))
                          ;; NB each predicate function has typically used
                          ;; `default-directory' as a parameter passed by
                          ;; way of the lexical environment of the function.
                          (when (funcall predicate)
                            (cl-return-from matched (cons type dir))))))
                    (dolist (f (directory-files dir))
                      ;; iterate for tests on filenames in dir,
                      ;; if reached
                      (unless (or (string= f ".") (string= f ".."))
                        (dolist (test re-tests)
                          (let ((predicate (car test))
                                (type (cdr test)))
                            (when (string-match-p predicate f)
                              (cl-return-from matched (cons type dir)))))))))
              (check-dir (dir)
                ;; when the parent directory is the same as dir, it's at
                ;; the root directory of the filesystem
                (or (match-dir-p dir)
                    (let ((next (unless (stop-dir-p dir)
                                  (file-name-directory (directory-file-name dir)))))
                      (unless (or (null next) (equal next dir))
                        (check-dir next))))))
      (check-dir (expand-file-name start)))))

(cl-defun iruby-console-get-impl (&optional (start default-directory)
                                    (table iruby-console-patterns-alist))
  (let ((first-match (iruby-console-walk-dirs start table)))
    (cl-destructuring-bind (type . dir) first-match
      
      )))

(cl-defun iruby-find-console-buffer (&optional (dir default-directory) name)
  (let* ((dir-attrs (file-attributes dir 'integer))
         (dir-device (when dir-attrs
                       (file-attribute-device-number dir-attrs)))
         (dir-ino (when dir-attrs
                    (file-attribute-inode-number dir-attrs))))
    (when dir-attrs
      (cl-block search
        (cl-dolist (elt iruby-process-buffers)
          (let ((buff (cdr elt)))
            (when (buffer-live-p buff)
              (with-current-buffer buff
                (and (if name (equal name (iruby-impl-name iruby-buffer-impl))
                       t)
                     (let* ((o-attrs (file-attributes default-directory
                                                      'integer))
                            (o-device (when o-attrs
                                        (file-attribute-device-number o-attrs)))
                            (o-ino (when o-attrs
                                     (file-attribute-inode-number o-attrs))))
                       (and o-attrs
                            (= o-device dir-device)
                            (= o-ino dir-ino)))
                     (cl-return-from search buff))))))))))


;; (iruby-console-walk-dirs "/opt/puppet/puppet_wk/src/puppet_devsrc/lib/puppet")

;; (iruby-console-walk-dirs "/tmp")



(provide 'iruby-console-util)

