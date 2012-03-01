;; prometheus.scm --- A prototype-based object system

;; Copyright (C) 2005, 2006 Jorgen Schaefer

;; Author: Jorgen Schaefer <forcer@forcix.cx>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301  USA

;;; Commentary:

;; This implements the Prometheus object system ontop of the Hermes
;; system.


;;; Code:

;;;;;;;;;;;;;;;;;;;;;
;;; Prometheus Object

;;; This creates a new root object for a Prometheus hierarchy.

(define (make-prometheus-root-object)
  (let ((o (make-hermes-object)))
    (o 'add-message! 'clone root-clone)
    (o 'add-message! 'message-not-understood root-message-not-understood)
    (o 'add-message! 'ambiguous-message-send root-ambiguous-message-send)
    (o 'add-message! 'immediate-slot-list root-immediate-slot-list)
    (o 'add-message! 'set-immediate-slot-list! root-set-immediate-slot-list!)
    (o 'add-message! 'add-value-slot! root-add-value-slot!)
    (o 'add-message! 'add-method-slot! root-add-method-slot!)
    (o 'add-message! 'add-parent-slot! root-add-parent-slot!)
    (o 'add-message! 'delete-slot! root-delete-slot!)
    o))

;;; Return the slot list. Each entry in the slot list is a list of
;;; three elements: The name of the getter, the name of the setter,
;;; and a boolean whether this is a parent slot or not.
;;; For the initial object, this is hardcoded. Bad programmer. No
;;; cookie.
(define (root-immediate-slot-list self resend)
  '((clone #f #f)
    (message-not-understood #f #f)
    (ambiguous-message-send #f #f)
    (immediate-slot-list set-immediate-slot-list! #f)
    (add-value-slot! #f #f)
    (add-method-slot! #f #f)
    (add-parent-slot! #f #f)
    (delete-slot! #f #f)))

;;; Set the slot list to be returned in the future to a new list. This
;;; just adds a new message so the slot list of a parent is never
;;; overwritten.
(define (root-set-immediate-slot-list! self resend new)
  (self 'add-message! 'immediate-slot-list
        (lambda (self resend)
          new)))

;;; Return a new object with the parent pointer set to this one.
(define (root-clone self resend)
  (let ((child (make-hermes-object)))
    (child 'add-message! 'parent
           (lambda (self2 resend)
             self)
           #t)
    (child 'add-message! 'immediate-slot-list
           (lambda (self2 resend)
             '((parent #f parent))))
    child))

;;; When the root object receives a MESSAGE-NOT-UNDERSTOOD message,
;;; signal an error. We don't handle that.
(define (root-message-not-understood self resend message args)
  (error "Message not understood" self message args))

;;; When the root object receive an AMBIGUOUS-MESSAGE-SEND message,
;;; signal an error. We don't handle that either.
(define (root-ambiguous-message-send self resend message args)
  (error "Message ambiguous" self message args))

(define-syntax make-getter-setter
  (syntax-rules ()
    ((make-getter-setter 'MESSAGE VALUE TYPE PURE-GETTER)
     (make-getter-setter 'MESSAGE VALUE TYPE PURE-GETTER PURE-GETTER))
    ((make-getter-setter 'MESSAGE VALUE TYPE PURE-GETTER SETABLE-GETTER)
     (case-lambda

      ((self resend getter VALUE)
       (self 'delete-slot! getter)
       (self 'set-immediate-slot-list!
             (alist-cons getter
                         (list #f TYPE)
                         (self 'immediate-slot-list)))
       (self 'add-message! getter PURE-GETTER (eq? TYPE 'parent)))

      ((self resend getter setter VALUE)
       (self 'delete-slot! getter)
       (self 'delete-slot! setter)
       (self 'set-immediate-slot-list!
               (alist-cons getter
                           (list setter TYPE)
                           (self 'immediate-slot-list)))
         (self 'add-message! getter SETABLE-GETTER (eq? TYPE 'parent))
         (self 'add-message! setter
               (lambda (self2 resend new)
                 (if (eq? self2 self)
                     (set! VALUE new)
                     (self2 'MESSAGE getter setter new)))))))))

;;; Add a value slot. Nothing fancy when no setter is given, except
;;; that we make sure a possible earlier setter is removed. But when
;;; there is a setter given, we make them share a value for fast
;;; modification.
(define root-add-value-slot!
  (make-getter-setter 'add-value-slot! value 'value
                      (lambda (self resend)
                        value)))

;;; A method slot is just a normal message slot, except that we record
;;; its existence in the slot list.
(define root-add-method-slot!
  (make-getter-setter 'add-method-slot! value 'method
                      value
                      (lambda (self resend . args)
                        (apply value self resend args))))

;;; A parent slot isn't very special, either, except that we note its
;;; special status for both Hermes and our slot list.
(define root-add-parent-slot!
  (make-getter-setter 'add-parent-slot! value 'parent
                      (lambda (self resend)
                        value)))


;;; Delete a slot again. If it does have an associated setter, remove
;;; that setter as well.
(define (root-delete-slot! self resend getter)
  (self 'set-immediate-slot-list!
        (let loop ((alis (self 'immediate-slot-list)))
          (cond
           ((null? alis)
            '())
           ((eq? getter (caar alis))
            (self 'delete-message! (cadar alis))
            (loop (cdr alis)))
           (else
            (cons (car alis)
                  (loop (cdr alis)))))))
  (self 'delete-message! getter))

;;;;;;;;;;;;;;;;;;;
;;; Syntactic Sugar

;;; The syntactic sugar for defining methods and objects.

(define-syntax define-method
  (syntax-rules ()
    ((_ (obj 'message self resend args ...)
        body1 body ...)
     (obj 'add-method-slot! 'message
          (lambda (self resend args ...)
            body1 body ...)))))

(define-syntax define-object
  (syntax-rules ()
    ((_ name (creation-parent (parent-name parent-object) ...)
        slots ...)
     (define name (let ((o (creation-parent 'clone)))
                    (o 'add-parent-slot! 'parent-name parent-object)
                    ...
                    (define-object/add-slots! o slots ...)
                    o)))))

(define-syntax define-object/add-slots!
  (syntax-rules ()
    ((_ o)
     (values))
    ((_ o ((method-name . method-args) body ...)
        slots ...)
     (begin
       (o 'add-method-slot! 'method-name (lambda method-args
                                           body ...))
       (define-object/add-slots! o slots ...)))
    ((_ o (slot-getter slot-setter slot-value)
        slots ...)
     (begin
       (o 'add-value-slot! 'slot-getter 'slot-setter slot-value)
       (define-object/add-slots! o slots ...)))
    ((_ o (slot-getter slot-value)
        slots ...)
     (begin
       (o 'add-value-slot! 'slot-getter slot-value)
       (define-object/add-slots! o slots ...)))))

;;; Let there be light.
;; FIXME! Better name?
(define *the-root-object* (make-prometheus-root-object))

;;; prometheus.scm ends here
