#lang racket
(require "message-passing-defns.rkt")

(provide make-lifo make-fifo make-dfs-node)

;; make a stack object
(define (make-lifo)
  ;; internal state
  (let ([stack '()])
    ;; methods that mutate or access the internal state
    (define (enqueue! element) (set! stack (append (list element) stack)))
    (define (dequeue!) (let ([top (car stack)])
                         (begin (set! stack (cdr stack))
                                top)))
    (define (empty?) (if (null? stack)
                         #t
                         #f))
    ;; method dispatch
    (lambda (msg)
      (cond [(eq? msg 'enqueue!) enqueue!]
            [(eq? msg 'dequeue!) dequeue!]
            [(eq? msg 'empty?) empty?]))
    ))

(define (make-fifo)
  ;; internal state
  (let ([queue '()])
    ;; methods that mutate or access the internal state
    (define (enqueue! element) (set! queue (append queue (list element))))
    (define (dequeue!) (let ([top (car queue)])
                         (begin (set! queue (cdr queue))
                                top)))
    (define (empty?) (if (null? queue)
                         #t
                         #f))
    ;; method dispatch
    (lambda (msg)
      (cond [(eq? msg 'enqueue!) enqueue!]
            [(eq? msg 'dequeue!) dequeue!]
            [(eq? msg 'empty?) empty?]))
    ))

; return get-children in reverse order
(define (make-dfs-node name)
  ;;internal state
  (let ([marked #f]
        [children '()])
    ;;method implementations
    (define (marked?) marked)
    (define (mark!) (set! marked #t))
    (define (get-children) (reverse children))
    (define (set-children! kids) (set! children kids))
    (define (get-name) name)
    
    ;;method dispatch
    (lambda (msg)
      (cond [(eq? msg 'get-name) get-name]
            [(eq? msg 'get-children) get-children]
            [(eq? msg 'set-children!) set-children!]
            [(eq? msg 'marked?) marked?]
            [(eq? msg 'mark!) mark!]))))

; reverse a list
(define (reverse lst count)
  (if (null? lst)
      '()
      (begin(printf"lst: ~v" lst) (append (reverse (cdr lst) (+ count 1)) (list (car lst))))))

