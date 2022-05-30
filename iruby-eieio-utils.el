;; ...

;; this file is not a package file

(eval-when-compile
  (require 'cl-macs))

(require 'eieio)

(defun iruby:class-slot-initargs (class)
  (let* ((cls (cl-etypecase class
                (symbol (find-class class))
                (eieio--class class)))
         (cls-in (eieio--class-initarg-tuples cls)))
    (when cls-in
      (mapcan (lambda (sl)
                (let* ((sl-nm (cl--slot-descriptor-name sl))
                       (in-args (mapcan (lambda (elt)
                                          (when (eq (cdr elt) sl-nm)
                                            (list (car elt))))
                                        cls-in)))
                  (when in-args
                    (list (cons sl-nm in-args)))))
              (eieio-class-slots cls)))))

(defun iruby:slot-initargs (slot class)
  (let ((sl (cl-etypecase slot
              (symbol slot)
              (cl-slot-descriptor (cl--slot-descriptor-name slot))))
        (in (iruby:class-slot-initargs class)))
    (cdr (assq sl in))))

(provide 'iruby-eieio-utils)

