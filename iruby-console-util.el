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


(cl-defun iruby-find-console-buffer (&optional (dir default-directory) name)
  (let* ((dir-attrs (or (file-attributes dir 'integer)
                        (error "Directory not found: %s" dir)))
         (dir-device (file-attribute-device-number dir-attrs))
         (dir-ino (file-attribute-inode-number dir-attrs)))
    (catch 'search
      (cl-dolist (elt iruby-process-buffers)
        (let ((buff (cdr elt)))
          (when (buffer-live-p buff)
            (with-current-buffer buff
              (and (if name (equal name (iruby:impl-name iruby-buffer-impl))
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
                   (throw 'search buff)))))))))

(provide 'iruby-console-util)

