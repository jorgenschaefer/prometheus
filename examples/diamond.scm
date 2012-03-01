;;; This requires SRFI-23

;;; We create an amphibious vehicle which inherits from a car - which
;;; can only drive on ground - and from a ship - which can only drive
;;; on water. Roads have a type of terrain. The amphibious vehicle
;;; drives along the road, using either the drive method of the car or
;;; of the ship.

;;; First, let's build a road.
(define-object road-segment (*the-root-object*)
  (next set-next! #f)
  (type set-type! 'ground)
  ((clone self resend next type)
   (let ((o (resend #f 'clone)))
     (o 'set-next! next)
     (o 'set-type! type)
     o)))

;;; Create a road with the environment types in the ENVIRONMENTS list.
(define (make-road environments)
  (if (null? (cdr environments))
      (road-segment 'clone
                    #f
                    (car environments))
      (road-segment 'clone
                    (make-road (cdr environments))
                    (car environments))))

;;; Now, we need a vehicle - the base class.
(define-object vehicle (*the-root-object*)
  (location set-location! #f)
  ((drive self resend)
   #f)
  ((clone self resend . location)
   (let ((o (resend #f 'clone)))
     (if (not (null? location))
         (o 'set-location! (car location)))
     o)))


;;; All vehicles have to drive quite similarily - no one stops us from
;;; using a normal helper procedure here.
(define (handle-drive self handlers)
  (let ((next ((self 'location) 'next)))
    (cond
     ((not next)
      (display "Yay, we're at the goal!")
      (newline))
     ((assq (next 'type) handlers)
      => (lambda (handler)
           ((cdr handler) next)))
     (else
      (error "Your vehicle crashed on a road segment of type"
             (next 'type))))))

;;; And a car. Hm. Wait. A CAR is something pretty specific in Scheme,
;;; make an automobile instead.
(define-object automobile (vehicle)
  ((drive self resend)
   (resend #f 'drive)
   (handle-drive self `((ground . ,(lambda (next)
                                     (display "*wrooom*")
                                     (newline)
                                     (self 'set-location! next)))))))

;;; And now a ship, for waterways.
(define-object ship (vehicle)
  ((drive self resend)
   (resend #f 'drive)
   (handle-drive self `((water . ,(lambda (next)
                                    (display "*whoosh*")
                                    (newline)
                                    (self 'set-location! next)))))))

;;; And an amphibious vehicle for good measure!
(define-object amphibious (ship (ground-parent automobile))
  ((drive self resend)
   (handle-drive self `((water . ,(lambda (next)
                                   (resend 'parent 'drive)))
                       (ground . ,(lambda (next)
                                    (resend 'ground-parent 'drive)))))))


;;; The code above works already. We can clone ships, automobiles and
;;; amphibious vehicles as much as we want, and they drive happily on
;;; roads. But we could extend this, and add gas consumption. This
;;; will even modify already existing vehicles, because they inherit
;;; from the vehicle object we extend:
(vehicle 'add-value-slot! 'gas 'set-gas! 0)
(vehicle 'add-value-slot! 'needed-gas 'set-needed-gas! 0)
(define-method (vehicle 'drive self resend)
  (let ((current-gas (self 'gas))
        (needed-gas (self 'needed-gas)))
    (if (>= current-gas needed-gas)
        (self 'set-gas! (- current-gas needed-gas))
        (error "Out of gas!"))))

;;; If you want to test the speed of the implementation:
(define (make-infinite-road)
  (let* ((ground (road-segment 'clone #f 'ground))
         (water (road-segment 'clone ground 'water)))
    (ground 'set-next! water)
    ground))

(define (test n)
  (let ((o (amphibious 'clone (make-infinite-road))))
    (do ((i 0 (+ i 1)))
        ((= i n) #t)
      (o 'drive))))

