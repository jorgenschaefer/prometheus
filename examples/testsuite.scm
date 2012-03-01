;;; This is a simple testsuite.
;;; It requires my ASSERT macros

;;; ,open assert prometheus
;;; And run the following:

(define (prometheus-test)
  (define (println . args)
    (for-each display args)
    (newline))

  (println "Testing Prometheus")
  (println "==================")
  (println "CLONE")
  (println "-----")
  (let* ((o1 (*the-root-object* 'clone))
         (o2 (o1 'clone)))
    (assert "Parent slot set by clone"
      (eq? *the-root-object* (o1 'parent)))

    (newline)
    (println "ADD-VALUE-SLOT!")
    (println "---------------")
    (o1 'add-value-slot! 'fnord 17)
    (assert "Read-Only Getter"
      (= 17 (o1 'fnord)))
    (o1 'add-value-slot! 'fnord 'set-fnord! 23)
    (assert "Getter"
      (= 23 (o1 'fnord)))
    (o2 'set-fnord! 5)
    (assert "Setter"
      (= 5 (o2 'fnord)))
    (assert "Setter does not modify parent"
      (= 23 (o1 'fnord)))

    (newline)
    (println "ADD-METHOD-SLOT!")
    (println "----------------")
    (o1 'add-method-slot! 'add (lambda (self resend a b)
                                 (+ a b)))
    (assert "Read-only getter"
      (= 5 (o1 'add 2 3)))
    (o1 'add-method-slot! 'add 'set-add! (lambda (self resend a b)
                                           (+ a b)))
    (assert "Getter"
      (= 17 (o2 'add 10 7)))
    (o2 'set-add! (lambda (self resend a b)
                    (* a b)))
    (assert "Setter"
      (= 42 (o2 'add 6 7)))
    (assert "Setter does not modify parent"
      (= 17 (o1 'add 10 7)))
    )

  (newline)
  (println "ADD-PARENT-SLOT!")
  (println "----------------")
  (let* ((testparent (*the-root-object* 'clone))
         (o1 (*the-root-object* 'clone))
         (o2 (o1 'clone))
         (otherparent (*the-root-object* 'clone)))
    (testparent 'add-value-slot! 'testparent #t)
    (o1 'add-parent-slot! 'parent2 testparent)
    (assert "Children inherit stuff from their parents"
      (eq? #t (o2 'testparent)))
    (o1 'add-parent-slot! 'parent2 'set-parent2 testparent)
    (assert "Parent with setter slot"
      (eq? #t (o2 'testparent)))
    (otherparent 'add-value-slot! 'otherparent #f)
    (o2 'set-parent2 otherparent)
    (assert "Setter modifies parent slot"
      (eq? #f (o2 'otherparent)))
    (assert "Setter does not modify parents"
      (eq? #t (o1 'testparent)))
    (o2 'add-value-slot! 'parent2 #f)
    (assert-fails "Other types of slot overwrite parent slots"
      (o2 'otherparent))
    )

  (newline)
  (println "SELF")
  (println "----")
  (let* ((o1 (*the-root-object* 'clone))
         (o2 (o1 'clone)))
    (o1 'add-method-slot! 'get-self (lambda (self resend)
                                      self))
    (assert "Self is passed correctly in inheritance"
      (eq? o2 (o2 'get-self)))
    )

  (newline)
  (println "Resends")
  (println "-------")
  (let* ((a (*the-root-object* 'clone))
         (b (a 'clone))
         (c (b 'clone)))
    (define-method (a 'info self resend)
      'a)
    (define-method (b 'info self resend)
      'b)
    (define-method (c 'info self resend)
      'c)
    (define-method (c 'get-info self resend where)
      (resend where 'info))
    (c 'add-value-slot! 'parent2 a)

    (assert "Local resend"
      (eq? 'c (c 'get-info #t)))
    (assert "Undirected resend"
      (eq? 'b (c 'get-info #f)))
    (assert "Directed resend"
      (eq? 'b (c 'get-info 'parent)))
    (assert "Directed resend to a non-parent"
      (eq? 'a (c 'get-info 'parent2)))
    )

  (newline)
  (println "Error handling")
  (println "--------------")

  (let* ((o1 (*the-root-object* 'clone))
         (o2.1 (o1 'clone))
         (o2.2 (o1 'clone))
         (o3 (o2.1 'clone)))
    (o3 'add-parent-slot! 'parent2 o2.2)
    (o2.1 'add-value-slot! 'fnord 5)
    (o2.2 'add-value-slot! 'fnord 23)
    (assert-fails "Unknown message signals error"
      (o3 'gobble-gobble-gobble-gobble-gobble))
    (assert-fails "Ambiguous message signals error"
      (o3 'fnord))

    (o1 'add-method-slot! 'message-not-understood
        (lambda (self resend msg args)
          (cons 'message-not-understood
                (cons msg args))))
    (o1 'add-method-slot! 'ambiguous-message-send
        (lambda (self resend msg args)
          (cons 'ambiguous-message-send
                (cons msg args))))

    (assert "Message-not-understood is called correctly"
      (equal? '(message-not-understood foo 1 2 3) (o3 'foo 1 2 3)))
    (assert "Ambiguous-message-send is called correctly"
      (equal? '(ambiguous-message-send fnord 5 17 23) (o3 'fnord 5 17 23)))

    ;; FIXME! This might/should already die?
    ;; (o3 'add-parent-slot! 'parent3 #f)
    ;; We can write an PROMETHEUS-OBJECT? predicate
    ;; with the help of an error handler...
    (o3 'add-parent-slot! 'parent3 (lambda args #f))
    (assert-fails "Parent slots that are not objects cause an error"
      (o3 'really-does-not-exist))
    )

  (let* ((a (*the-root-object* 'clone))
         (b (a 'clone)))
    (a 'add-parent-slot! 'parent2 b)
    (assert-fails "Parent cycles don't cause infinite loops"
      (a 'skiddoo))
    )
  )


