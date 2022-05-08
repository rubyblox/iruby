;;; iruby-util.el --- general utility forms for iRuby

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


(eval-when-compile

(cl-defmacro with-symbols-iruby ((&rest symbols) &body body)
  ;; an alternative to the common `with-gensym' pattern,
  ;; this does not use gensym.
  `(let ,(mapcar #'(lambda (s)
                     `(,s (make-symbol ,(concat "%" (symbol-name s)))))
                 symbols)
     ,@body))


(defmacro iruby:rconc (newcdr seq)
  (with-symbols-iruby (new lst)
    `(let* ((,new ,newcdr)
            (,lst (last ,new)))
       (rplacd (last ,seq) ,new)
       (setq ,seq ,lst))))

(defsubst iruby:split-shell-string (str)
  (cond
    ((or (> emacs-major-version 28)
         (and (= emacs-major-version 28) (>= emacs-minor-version 1)))
     (split-string-shell-command str))
    (t (split-string-and-unquote str))))


) ;; eval-when-compile

(defun iruby:same-file-p (file-a file-b)
  "Return true if file-a and file-b represent the same physical
filesystem object.

This function will not dereference symbolic links"
  (let* ((f-a-attrs (file-attributes file-a 'integer))
         (f-a-dev  (file-attribute-device-number f-a-attrs))
         (f-b-attrs (file-attributes file-b 'integer))
         (f-b-dev  (file-attribute-device-number f-b-attrs)))
    (when (and f-a-dev f-b-dev
               (= f-a-dev f-b-dev))
      (let ((f-a-ino (file-attribute-inode-number f-a-attrs))
            (f-b-ino (file-attribute-inode-number f-b-attrs)))
        (= f-a-ino f-b-ino)))))

;;; ad-hoc test
;; (iruby:same-file-p (expand-file-name "iruby.el") "iruby.el")
;; (iruby:same-file-p (expand-file-name "irubyx.el") "iruby.el")

(defun iruby:read-directory (prompt)
  (let* ((dir (read-file-name prompt nil nil t nil 'file-directory-p))
         (use-dir
          (if (zerop (length dir))
              default-directory
            dir)))
    (unless (or (zerop iruby-directory-history-limit)
                (equal use-dir (car iruby-directory-history)))
       (add-to-history 'iruby-directory-history use-dir
                       iruby-directory-history-limit))
    use-dir))


(provide 'iruby-util)
