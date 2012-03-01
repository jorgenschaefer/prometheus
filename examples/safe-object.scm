;;; This defines two Scheme 48 modules which each exports only a
;;; single value: An object which can't be modified from the outside.

;;; The first version is trivial. We just steal the parent.

(define-structure safe-object-full (export full-safe-object)
  (open scheme
        prometheus)
  (begin
    (define fully-safe-object (*the-root-object* 'clone))
    (full-safe-object 'add-value-slot! 'fnord 'set-fnord! 23)
    (full-safe-object 'delete-slot! 'parent)))

;;; The second assumes you just want to hide a few of the messages of
;;; the parent object.

;;; The trick is to overwrite all modifying messages. Since the parent
;;; object might be used to modify us, we also hide it behind a
;;; private message name.

(define-structure safe-object-partial (export partial-safe-object)
  (open scheme
        srfi-23
        prometheus)
  (begin
    (define partial-safe-object ((make-prometheus-root-object) 'clone))
    ;; The private parent message
    (let ((parent (list '*parent-message*)))
      (partial-safe-object 'add-value-slot! 'immutable 23)
      ;; Add our private parent
      (partial-safe-object 'add-parent-slot! parent (safe-object 'parent))
      ;; And delete the one added by the clone
      (partial-safe-object 'delete-slot! 'parent)
      ;; Overwrite all unneeded slots - since some messages need
      ;; others internally, we do a resend until we did overwrite all
      ;; slots:
      (let ((resend? #t))
        (for-each (lambda (msg)
                    (partial-safe-object
                     'add-method-slot! msg
                     (lambda (self resend . args)
                       (if resend?
                           (apply resend #f msg args)
                           (error "Object is immutable!")))))
                  '(add-slot-binding!
                    remove-slot-bindings!
                    clone
                    add-value-slot!
                    add-parent-slot!
                    add-method-slot!
                    delete-slot!
                    slots->list))
        (set! resend? #f)))))
