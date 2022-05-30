;; iruby-sandbox.el -- ad hoc tests and general prototyping for iRuby

;; this file is not a package file

(require 'iruby)


(defun iruby-buffer-get-interactor (which)
  (with-current-buffer (iruby-process-buffer (iruby-proc which))
    iruby-buffer-interactive-impl))


;; (iruby-buffer-get-interactor "gem(iokit)")

;; (iruby:impl-args  (iruby-buffer-get-interactor "gem(iokit)"))
;; NIL ?

;; (iruby:parse-cmd  (iruby-buffer-get-interactor "gem(iokit)"))
;; ^ FIXME no '("-I" "lib") args being included here.
;;
;; see iruby:gem-console (eieio)


;; iruby-buffer tests

(defun iruby-field-at-point ()
  (interactive)
  (message "field at point: %S" (get-text-property (point) 'field)))

(defun iruby-prompt-at-point ()
  (interactive)
  (message "at point: %S" (get-text-property (point) 'iruby-prompt)))

(defun iruby-ifind-syntax ()
  (interactive)
  (message "Found syntax: %s" (iruby-find-syntax)))

(eval-when ()
  (with-current-buffer (cdar iruby-process-buffers)
    (char-syntax ?\;)
    )

  (with-current-buffer (cdar iruby-process-buffers)
    (mapcar #'(lambda (int) (decode-char 'ascii int)) comint-delimiter-argument-list)
    )

  (with-current-buffer (cdar iruby-process-buffers)
    (mapcar #'char-to-string comint-delimiter-argument-list)
    )

  (iruby-send-string (caar iruby-process-buffers) "$!")

  ;; odd error (irb)
  (iruby-send-string (caar iruby-process-buffers) "$-")
  )

;;; iruby:impl tests


(eval-when ()
  ;; tests
 (require 'eieio-custom)
  (eieio-customize-object (make-instance 'iruby:impl))

  (customize-option 'iruby-ruby-language-impls)


  (customize-option 'iruby-interactive-impls)

)




(eval-when ()
  ;; test forms (FIXME move to -dev)

  (iruby:parse-cmd (iruby:get-language-impl "ruby"))
  ;; ^ albeit may not be very usefull for interactive eval ..

  (iruby:parse-cmd "irb")

  (iruby:parse-cmd "pry")

  ;; oh fixme
  (iruby:parse-cmd "jruby-irb")

  (run-iruby-new   (iruby:parse-cmd "irb") "irb-test" "irb")

  ;; DNW (FIXME) (probably needs to use its own exe script this DNW
  (run-iruby-new   (iruby:parse-cmd "pry") "pry-test" "pry")

  (case nil
    ((t) 'true)
    ((nil) 'false))

)


(eval-when ()
  ;;; its' not a list, here
  ;; (car (eieio--class-slots (find-class 'iruby-wrapper-binding)))

  (type-of (eieio--class-slots (find-class 'iruby-wrapper-binding)))
  ;; ^ it's a vector ...

  (type-of (cl--class-slots (find-class 'iruby-wrapper-binding)))
  ;; ^ also a vector


  (type-of (cl-svref (cl--class-slots (find-class 'iruby-wrapper-binding))
                     0))
  ;; ^ it's a cl-slot-descriptor


  ;; (cl--slot-descriptor-name
  ;;  (cl-svref (cl--class-slots (find-class 'iruby-wrapper-binding))
  ;;            )

  (cl--class-slots (find-class 'iruby-wrapper-binding))

)


;; iruby:impl - tests towards iruby:initialize-wrap


(cl-defgeneric iruby:initialize-wrap (class base &rest initargs)
  ;; no longer used in iruby-console.el
  (:method ((class eieio--class)
            (base iruby:interactive-ruby) &rest initargs)
    (let ((inst (apply 'make-instance (eieio--class-name class)
                       initargs)))
      (iruby:initialize-instance-from inst base)
      inst)))


(eval-when ()
  (let ((wrp (iruby-wrapper-binding :name "TEST"
                                    :wrapper-base-cmd '("echo" "TEST@"))))
    (iruby:initialize-instance-from wrp (iruby:default-interactive-ruby))
    (iruby:parse-cmd wrp))


  (iruby:initialize-wrap '("echo" "TEST@")
                      (iruby:default-interactive-ruby)
                      "test")

  (iruby:parse-cmd
   (iruby:initialize-wrap nil
                       (iruby:default-interactive-ruby)
                       "test2"))

  )


;;
;; console interface functions/macros
;;

(eval-when ()
  (iruby:console-match-p "/usr/local/lib/ruby/gems/3.0/gems/atk-3.5.1"
                         (cl-find "gem" iruby-console-tests
                           :test #'equal :key 'iruby:console-test-tag))
  ;; => "atk.gemspec"

  (iruby-console-create
   :start "/usr/local/lib/ruby/gems/3.0/gems/atk-3.5.1")

  (iruby:impl-name (iruby-console-create
                    :start "/usr/local/lib/ruby/gems/3.0/gems/atk-3.5.1"))

  (iruby:console-match-p "/tmp" (car iruby-console-tests))
  ;; => NIL

  (mapcar #'iruby:console-test-tag iruby-console-tests)

  (mapcar #'(lambda (tst) (cons (iruby:console-test-tag tst)
                                (iruby:console-test-test tst)))
          iruby-console-tests)

  (iruby:console-wrapper-args (iruby:console-gem))
  ;; should not return nil, and yet it will consistently,
  ;; for how EIEIO does notimplement the default-initargs property of CLOS

  (iruby:console-wrapper-class-args (iruby:console-gem))

  (iruby:console-wrapper-class-args (iruby:console-gemfile))

  (iruby:console-wrapper-class-args (iruby:console-rails))
  ;; ^ FIXME missing the ("rails" "console") part in one of its superclasses


  (print
   (cl-macroexpand
   (quote
    (define-iruby-console gem (iruby:gem-console)
      "*.gemspec" ;; one glob, no Gemfile deps parse
      )
    ))
   (current-buffer))

  )

;; -- console impl tests

;; (iruby:class-slots (find-class 'iruby-wrapper-binding))

(eval-when ()
  (let ((a (iruby:class-slots (find-class 'iruby-wrapper-binding)))
        (b (iruby:class-slots (find-class 'iruby:interactive-ruby))))
    ;; => ... set difference in present implementation: (wrapper-base-cmd)
    (print (list :intersection (cl-intersection a b :test #'eq)
                 :difference (cl-set-difference a b :test #'eq)
                 :a a :b b)
           (current-buffer)))
)

(eval-when ()

  (let* ((binding (iruby:default-interactive-ruby))
         (inst (iruby:initialize-wrap nil (find-class 'iruby:gem-console)
                                      binding)))
    (append (iruby:console-wrapper-prefix-cmd inst)
            (iruby:parse-cmd binding)
            (iruby:console-wrapper-append-args inst)
            ))
  ;; ^ suffix args are returned as should be (FIXME lost before comint?)

  (let* ((binding (iruby:default-interactive-ruby))
         (inst (iruby:initialize-wrap nil (find-class 'iruby:gemfile-console)
                                      binding)))
    (append (iruby:console-wrapper-prefix-cmd inst)
            (iruby:parse-cmd binding)
            (iruby:console-wrapper-append-args inst)
            ))
  ;; ^ prefix cmd is appended as it should be appended
  )





(defun iruby-test ()
  (interactive)
  (let ((impl
         (with-current-buffer (or (cdar iruby-process-buffers)
                                  (error "Found no Ruby"))
           iruby-buffer-interactive-impl)))

    ;; console-prefix-cmd is showing up nil for the impl?
    (message "cmd for %S: %S" (iruby:impl-name impl)
             (iruby:parse-cmd impl))
    ))

;; (iruby-test)

(eval-when ()
  (let ((inst (make-instance 'iruby:gemfile-console))
        (base (iruby:default-interactor-for nil)))
    (iruby:initialize-instance-from inst base)
    (iruby:parse-cmd inst)
    )
  )
;; -- tests for macroexpansion onto cl-generic defmethod forms

(cl-defgeneric iruby-test (datum)
  (:method (datum)
    (warn "In T"))
  (:method ((datum string))
    (warn "In STRING")
    (when (cl-next-method-p)
      (cl-call-next-method))))

;; (iruby-test "PING")


;;
;; more API tests onto EIEIO
;;

;; (iruby:slot-initargs 'requires 'iruby:impl)
;; => (:requires)

(eval-when-compile

(defsubst iruby:initarg-slot (initarg cls)
  (let ((map (eieio--class-initarg-tuples (iruby:ensure-class cls))))
    (cdr (assq initarg map))))

) ;; eval-when-compile

;; (iruby:initarg-slot :tag 'iruby:console-test)
;; => tag


(defun unset-proxy ()
  ;; try to clear any proxy environment variables in Emacs
  ;; such that may be used by subprocesses, e.g bundle(1)
  (interactive)
  (dolist (envt process-environment t)
    (let ((var (car (split-string envt "=")))
          (case-fold-search t))
      (when (string-match-p ".*proxy$" var)
        (setenv var nil)))))


;;
;; ad hoc debugging for assignment of interactive binding/completion
;; exprs, mainly in iruby console process buffers
;;

(iruby:impl-p
 (with-current-buffer (iruby-process-buffer (iruby-proc))
   iruby-buffer-interactive-impl))
;; ^ shouldf => t [x] (iruby:impl-p reimplemented)

(iruby:interactive-binding-expr-list
  (with-current-buffer (iruby-process-buffer (iruby-proc))
    ;; the corresponding slot is bound, so wh does the top form return nil?
    iruby-buffer-interactive-impl)
  )
;; ^ should not => nil [x] (initializing console impl from base impl)

(eieio-oref
  (with-current-buffer (iruby-process-buffer (iruby-proc))
    ;; the corresponding slot is bound, so wh does the top form return nil?
    iruby-buffer-interactive-impl)
  'binding
  )
;; ^ should not => nil [x] (initializing console impl from base impl)

(iruby:interactive-binding-expr-list
 (iruby:wrapper-base-impl
  (with-current-buffer (iruby-process-buffer (iruby-proc))
    ;; the corresponding slot is bound, so wh does the top form return nil?
    iruby-buffer-interactive-impl))
  )
;; ^ does not => nil [x]


;;;
;;; setting `byte-optimizations' to 'byte' to prevent source
;;; optimizations that might result in errors on unbound symbols,
;;; when calling either `cl-call-next-method' or `cl-next-method-p'
;;;
;; Local Variables:
;; TBD.byte-optimize: byte
;; End:
