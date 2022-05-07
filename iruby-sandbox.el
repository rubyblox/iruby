;; iruby-sandbox.el -- ad hoc tests and general prototyping for iRuby

(defun iruby-buffer-impl (which)
  (with-current-buffer (iruby-process-buffer (iruby-proc which))
    iruby-buffer-impl))


;; (iruby-buffer-impl "gem(iokit)")

;; (iruby-impl-args  (iruby-buffer-impl "gem(iokit)"))
;; NIL ?

;; (iruby:parse-cmd  (iruby-buffer-impl "gem(iokit)"))
;; ^ FIXME no '("-I" "lib") args being included here.
;;
;; see iruby:gem-console (eieio)

