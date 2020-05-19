#lang racket

(provide send-message make-node)

(define (send-message obj msg . msg-args)
  (let ([method (obj msg)])
    (apply method msg-args)))

;;Create a node object, whose internal state is a name and
;;a list of children. The children aren't arguments to the constructor, because
;;we need to create all the nodes before passing them as someone's children.
(define (make-node name)
  ;;internal state
  (let ([marked #f]
        [children '()])
    ;;method implementations
    (define (marked?) marked)
    (define (mark!) (set! marked #t))
    (define (get-children) children)
    (define (set-children! kids) (set! children kids))
    (define (get-name) name)
    
    ;;method dispatch
    (lambda (msg)
      (cond [(eq? msg 'get-name) get-name]
            [(eq? msg 'get-children) get-children]
            [(eq? msg 'set-children!) set-children!]
            [(eq? msg 'marked?) marked?]
            [(eq? msg 'mark!) mark!]))))
