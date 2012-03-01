;;; This is a simple account-keeping object.

;;; It's just like a normal object
(define account (*the-root-object* 'clone))

;;; But it has a balance
(account 'add-value-slot! 'balance 'set-balance! 0)

;;; Which can be modified
(account 'add-method-slot! 'payment!
         (lambda (self resend amount)
           (self 'set-balance!
                 (+ (self 'balance)
                    amount))))

;;; Some tests:
(define a1 (account 'clone))
(define a2 (account 'clone))

(a1 'payment! 100)
(a2 'payment! 200)

(a1 'balance)
;;; => 100
(a2 'balance)
;;; => 200

(a1 'payment! -20)
(a1 'balance)
;;; => 80

;;; The typing for the slot definitions above can be rather tedious.
;;; Prometheus provides syntactic sugar for those operations.

;;; A method can be added with the DEFINE-METHOD syntax. This code is
;;; equivalent to the code above which adds the PAYMENT! method:
(define-method (account 'payment! self resend amount)
  (self 'set-balance!
        (+ (self 'balance)
           amount)))

;;; And this defines the whole object with the BALANCE slot and the
;;; PAYMENT! method just as above:
(define-object account (*the-root-object*)
  (balance set-balance! 0)
  ((payment! self resend amount)
   (self 'set-balance!
         (+ (self 'balance)
            amount))))
