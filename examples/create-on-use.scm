;;; A simple object which creates slots as they are used. This
;;; demonstrates the use of the MESSAGE-NOT-UNDERSTOOD error message.

;;; Slots behave like value slots, and the accessors use a second
;;; argument as the "default value". If that is not given, (if #f #f)
;;; is used, which is usually not what is intended.
(define-object create-on-use-object (*the-root-object*)
  ((message-not-understood self resend slot args)
   (self 'add-method-slot! slot (lambda (self resend . default)
                                  (if (pair? args)
                                      (car args))))
   (self slot)))
