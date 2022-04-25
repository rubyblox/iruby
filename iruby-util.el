;;; iruby-util.el --- general utility forms for iRuby

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
