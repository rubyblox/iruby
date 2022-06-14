;; ...

;; this file is not a package file

(eval-when-compile
  (require 'cl-macs))

(require 'eieio)
(require 'eieio-custom)

(defun iruby:class-slot-initargs (class)
  "Return an assoc list of slot names and defined intiargs in CLASS

CLASS may be provided as a symbol representing a class name, or as an
object of type eieio--class representing a class definition object.

See also: `iruby:slot-initargs'"
  ;; FIXME NEEDSTEST for class-allocated slots in EIEIO
  ;; mainly to determine whether initargs for class-allocated slots
  ;; will be visible in the return value here
  ;; - this would have a side effect onto the eieio-default-superclass method
  ;;   for iruby:make-load-form
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
  "Return the list of initargs defined for SLOT in CLASS

SLOT may be provided as a slot name, i.e a symbol, or as a slot
definition object, i.e of type cl--slot-descriptor.

CLASS may be provided as a symbol representing a class name, or as an
object of type eieio--class representing a class definition object.

This function dispaches to `iruby:class-slot-initargs'"
  (let ((sl (cl-etypecase slot
              (symbol slot)
              (cl-slot-descriptor (cl--slot-descriptor-name slot))))
        (in (iruby:class-slot-initargs class)))
    (cdr (assq sl in))))


(cl-defgeneric iruby:make-load-form (object)
  "Produce a readable load form for an OBJECT.

Any primary method on this generic function should return a literal,
readable form in a syntax for `eval'. When the form is evaluated, it
should produce an object semantically `equal' to the original OBJECT."
  (:method ((object number))
    "Return the object"
    object)
  (:method ((object string))
    "Return the object"
    object)
  (:method ((object symbol))
    "Return a quoted representation of the object"
    (list 'quote object))
  (:method ((object sequence))
    "Return a constructor form for the sequence OBJECT

The constructor form should contain a readable representation for all
sequence elements of the OBJECT, with an order of elements equivalent to
that in the original OBJECT.

The constructor form will use one of the functions 'vector' or 'list',
dependent on the type of the OBJECT.

This method supports generic list and vector types"
    (let* ((seq-type (type-of object))
           (ctor (cl-typecase object
                   (vector 'vector)
                   ;; ensure that this will not use #'cons here
                   (list 'list)
                   ;; probably not reached:
                   (t seq-type))))
      (cons ctor
            (cl-map 'list (lambda (elt) (iruby:make-load-form elt))
                    object))))
  (:method ((object eieio-default-superclass))
    "Return a CONS representing a class constructor and list of initargs.

This method assumes that the class name of the class of OBJECT will be
available as a constructor function, such that should create an object
of that class, given a list of initialization arguments for the object

Known limitation: This  method assumes that all slot values for OBJECT
will have a readable representation (FIXME)"
    (let* ((argmap (iruby:class-slot-initargs (eieio-object-class object)))
           (load-args (list nil))
           (next-args load-args)
           (ctor (eieio-class-name (eieio-object-class object))))
      (dolist (arginf argmap)
        (let ((initargs (cdr arginf))
              (slname (car arginf)))
          (when (and initargs
                     (slot-boundp object slname))
            ;; NEEDSTEST for class-allocated slots in EIEIO
            (let ((add-args (list (iruby:make-load-form (car initargs))
                                  (iruby:make-load-form
                                   (slot-value object slname)))))
              ;; push the ADD-ARGS list to the tail of LOAD-ARGS,
              ;; via NEXT-ARGS
              (setf (cdr (last next-args)) add-args)
              (setq next-args add-args)))))
      (cons ctor (cdr load-args)))))

;;; ad hoc tests
;; (iruby:make-load-form '("A" "B" c d :t))
;; (eval (iruby:make-load-form '("A" "B" c d :t)))
;;
;; (iruby:make-load-form ["A" "B" c 1 -0.1 :t])
;; (eval (iruby:make-load-form ["A" "B" c 1 -0.1 :t]))
;;
;;; this test requires additional library code
;; (iruby:make-load-form (iruby:irb-binding :name "TBD"))
;; (eval (iruby:make-load-form (iruby:irb-binding :name "TBD")))


(provide 'iruby-eieio-utils)

