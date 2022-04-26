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


(cl-defmacro with-symbols-iruby ((&rest symbols) &body body)
  ;; an alternative to the common `with-gensym' pattern,
  ;; this does not use gensym.
  ;;
  ;; FIXME use more often, here - other make-symbol forms
  `(let ,(mapcar #'(lambda (s)
                     `(,s (make-symbol ,(concat "%" (symbol-name s)))))
                 symbols)
     ,@body))

(defmacro iruby-rconc (newcdr seq)
  (with-symbols-iruby (new lst)
    `(let* ((,new ,newcdr)
            (,lst (last ,new)))
       (rplacd (last ,seq) ,new)
       (setq ,seq ,lst))))


(provide 'iruby-util)
