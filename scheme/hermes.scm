;; hermes.scm --- A very simple prototype-based object system

;; Copyright (C) 2006 Jorgen Schaefer

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

;; Hermes is a very simple object system upon which Prometheus is
;; built.

;; Hermes provides a simple object which knows about messages, message
;; handlers, and inheritance. Each message is associated with a
;; boolean on whether this messages' value is a parent.

;; More specifically, a Hermes object is a closure to which a message
;; can be sent by applying it to the message name and a possible empty
;; list of message arguments. When the object does not know the
;; message, any parent slot's value is asked to handle this message.
;; If none knows how to, the original object receives a
;; MESSAGE-NOT-UNDERSTOOD message, with the arguments being the
;; message not being understood and a list of arguments. If more than
;; one parent knows about this message, the original object receives a
;; AMBIGUOUS-MESSAGE-SEND message, with the arguments again being the
;; ambiguous message and a list of arguments. Hermes is intelligent
;; enough to notice when the same object is reachable via different
;; parents, so diamond inheritance is no problem.

;; On the other hand, when the objects knows the message, the message
;; handler will be called with the message arguments in addition to
;; two values: SELF, a reference to the object that received the
;; original message, and RESEND, a procedure which can be used to ask
;; other objects to handle this message for you, while maintaining the
;; SELF object.

;; RESEND accepts two or more arguments. The first is the target. If
;; the target is #f, any parent that can handle the message receives
;; it as if the current object wouldn't have been able to handle it by
;; itself. If it is a symbol, the value of the message named by that
;; symbol is used as the parent. If it's an object, that object is
;; used directly. For messages that contain parent objects, the
;; handler must not use RESEND.

;; Hermes objects know about three messages by default, but only one
;; of them is essential.

;; Message: add-message! name handler [parent?]
;;
;; This adds a message named NAME, upon receiving which, HANDLER is
;; called. If PARENT? is supplied and not false, this message contains
;; a parent object.

;; Message: delete-message! name
;;
;; Remove the handler for messages named NAME, and resend them to
;; parent objects in the future again.

;; Message: %get-handler name receiver args visited
;;
;; The only essential message. This enables inheritance. NAME is the
;; name of the message we are looking for. RECEIVER is the original
;; receiver of the message, to be used as the SELF argument to the
;; handler procedure. ARGS is a list of arguments. VISITED is a list
;; of objects we have seen so far. This is used to detect cycles in
;; the inheritance graph.
;;
;; This message returns two values. The first one is the handler and
;; the other one the object in which this handler was found. The
;; handler value can also be one of two symbols to signify an error
;; condition. If it's the symbol MESSAGE-NOT-UNDERSTOOD, then neither
;; this object nor any parents knew how to handle this message. If
;; it's AMBIGUOUS-MESSAGE-SEND, the same message could be handled by
;; multiple parents in the inheritance graph. The user needs to add a
;; message which resends the ambiguous message unambiguously to the
;; correct parent. In either case, the second return value is #f. The
;; handler procedure itself accepts no arguments, and just runs the
;; message.


;;; Code:

;;; Create a new Hermes object. Hermes objects know only three
;;; messages, %GET-HANDLER, ADD-MESSAGE! and DELETE-MESSAGE!. These
;;; suffice to create a more advanced object systems on top of this.
(define (make-hermes-object)
  (let ((msg (make-messages)))
    (letrec ((self (lambda (name . args)
                     (messages-handle msg self name args))))
      (messages-add! msg '%get-handler
                     (lambda (_ resend name receiver args visited)
                       (get-handler msg self name receiver args visited))
                     #f)
      (messages-add! msg 'add-message!
                     (lambda (_ resend name handler . parentl)
                       (messages-add! msg name handler
                                      (if (null? parentl)
                                          #f
                                          (car parentl))))
                    #f)
      (messages-add! msg 'delete-message!
                     (lambda (_ resend name)
                       (messages-delete! msg name))
                     #f)
      self)))


;;;;;;;;;;;;;;;;;;;;;
;;; Helper procedures

(define (assq-set! name value alist)
  (cond
   ((assq name alist)
    => (lambda (entry)
         (set-cdr! entry value)
         alist))
   (else
    (alist-cons name value alist))))

;;;;;;;;;;;;;;;;;;;
;;; Messages record

;;; The messages record stores an association of message names and
;;; message handlers. It also stores such an association for parents.
;;; This uses two lists for efficiency reasons: The list of parents is
;;; needed much more often.

(define-record-type messages
  (%make-messages alist parents)
  messages?
  (alist messages-alist set-messages-alist!)
  (parents messages-parents set-messages-parents!))

(define (make-messages)
  (%make-messages '() '()))

(define (messages-add! msg name handler parent?)
  (set-messages-alist! msg
                       (assq-set! name handler
                                  (messages-alist msg)))
  (if parent?
      (set-messages-parents! msg
                             (assq-set! name handler
                                        (messages-parents msg)))))

(define (messages-delete! msg name)
  (set-messages-alist! msg
                       (alist-delete! name
                                      (messages-alist msg)
                                      eq?))
  (set-messages-parents! msg
                         (alist-delete! name
                                        (messages-parents msg)
                                        eq?)))

;;; Do a direct lookup (as opposed to asking the parents) for a
;;; message handler in the record.
(define (messages-direct-lookup msg name)
  (cond
   ((assq name (messages-alist msg))
    => cdr)
   (else
    #f)))

;;; Ask the parents in the messages record for handlers. This returns
;;; two values as explained above. To enable the
;;; AMBIGUOUS-MESSAGE-SEND error, the parent list is searched
;;; completely even when a handler is found.
(define (messages-parent-lookup msg self name receiver args visited)
  (let loop ((alis (messages-parents msg))
             (handler #f)
             (found #f))
    (if (null? alis)
        (if handler
            (values handler found)
            (values 'message-not-understood #f))
        (receive (new new-found)
            (((cdar alis)
              receiver
              (lambda args
                (error "Parent slots must not use resend."
                       receiver name args)))
             '%get-handler name receiver args (cons self visited))
          (case new
            ((message-not-understood)
             (loop (cdr alis)
                   handler
                   found))
            ((ambiguous-message-send)
             (values 'ambiguous-message-send
                     #f))
            (else
             (if (and handler
                      (and (not (eq? found new-found))))
                 (values 'ambiguous-message-send
                         #f)
                 (loop (cdr alis)
                       new
                       new-found))))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handling of Messages

;;; Handle a single message, checking for errors.
(define (messages-handle msg self name args)
  (receive (handler found)
      (get-handler msg self name self args '())
    (run-with-error-checking handler self name args)))

;;; Return the appropriate handler procedure.
(define (get-handler msg self name receiver args visited)
  (if (memq self visited)
      (values 'message-not-understood #f)
      (cond
       ((messages-direct-lookup msg name)
        => (lambda (handler)
             (values (lambda ()
                       (apply handler
                              receiver
                              (make-resender msg self receiver visited)
                              args))
                     self)))
       (else
        (messages-parent-lookup msg self name receiver args visited)))))

;;; Create a resender for the message.
(define (make-resender msg self receiver visited)
  (lambda (target name . args)
    (receive (handler found)
        (cond
         ((eq? target #f) ; ask parents
          (messages-parent-lookup msg self name receiver args visited))
         ((or (eq? target self) ; ask this object
              (eq? target #t)) ; hystorical syntax
          (get-handler msg self name receiver args visited))
         ((symbol? target) ; ask named parent
          ((self target) '%get-handler name receiver args (cons self visited)))
         (else ; ask that object
          (target '%get-handler name receiver args (cons self visited))))
      (run-with-error-checking handler self name args))))

;;; Signal the appropriate errors, if handler is not a procedure.
;;; Else, call that handler.
(define (run-with-error-checking handler self name args)
  (case handler
    ((message-not-understood)
     (if (eq? name 'message-not-understood)
         (error "Message MESSAGE-NOT-UNDERSTOOD not understood"
                self args)
         (self 'message-not-understood name args)))
    ((ambiguous-message-send)
     (if (eq? name 'ambiguous-message-send)
         (error "Message AMBIGUOUS-MESSAGE-SEND is ambiguous"
                self args)
         (self 'ambiguous-message-send name args)))
    (else
     (handler))))

;;; hermes.scm ends here
