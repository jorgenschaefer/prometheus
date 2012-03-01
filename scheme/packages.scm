(define-structure hermes (export make-hermes-object)
  (open scheme
        srfi-1
        srfi-8
        srfi-9
        srfi-23)
  (files hermes))

(define-structure prometheus (export make-prometheus-root-object
                                     *the-root-object*
                                     (define-method :syntax)
                                     (define-object :syntax))
  (open scheme
        srfi-1
        srfi-16
        srfi-23
        hermes)
  (files prometheus))
