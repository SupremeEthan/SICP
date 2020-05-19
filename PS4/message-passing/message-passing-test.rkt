#lang racket
(require rackunit rackunit/text-ui
         "example-graphs.rkt"
         "message-passing-defns.rkt"
         "graph-search.rkt"
         "message-passing.rkt")

(define (enqueue-list q l)
  (map (lambda (i) (send-message q 'enqueue! i)) l))

(define (drain-queue q)
  (if (send-message q 'empty?)
      '()
      (cons (send-message q 'dequeue!) (drain-queue q))))

(define lifo-tests
  (test-suite
   "LIFO tests"
   (test-case
       "Does a lifo know when it's empty?"
     (let ([lifo (make-lifo)])
       (check-true (send-message lifo 'empty?))
       (send-message lifo 'enqueue! 1)
       (check-false (send-message lifo 'empty?))
       (send-message lifo 'dequeue!)
       (check-true (send-message lifo 'empty?))))
   (test-case "Do objects come off the lifo in reverse order?"
     (let ([lifo (make-lifo)])
       (enqueue-list lifo '(1 2 3))
       (check-equal? (drain-queue lifo) '(3 2 1))))
   (test-case "Interleaved enqueues and dequeues"
     (let ([lifo (make-lifo)])
       (enqueue-list lifo '(1 2 3))
       (check-equal? (send-message lifo 'dequeue!) 3)
       (check-false (send-message lifo 'empty?))
       (send-message lifo 'enqueue! 5)
       (check-equal? (drain-queue lifo) '(5 2 1))
       (check-true (send-message lifo 'empty?))))))

(define fifo-tests
  (test-suite
   "FIFO tests"
   (test-case
       "Does a FIFO know when it's empty?"
     (let ([fifo (make-fifo)])
       (check-true (send-message fifo 'empty?))
       (send-message fifo 'enqueue! 1)
       (check-false (send-message fifo 'empty?))
       (send-message fifo 'dequeue!)
       (check-true (send-message fifo 'empty?))))
   (test-case "Do objects come off the fifo in order?"
     (let ([fifo (make-fifo)])
       (enqueue-list fifo '(1 2 3))
       (check-equal? (drain-queue fifo) '(1 2 3))))
   
   (test-case "Interleaved enqueues and dequeues"
     (let ([fifo (make-fifo)])
       (enqueue-list fifo '(1 2 3))
       (check-equal? (send-message fifo 'dequeue!) 1)
       (check-false (send-message fifo 'empty?))
       (send-message fifo 'enqueue! 5)
       (check-equal? (drain-queue fifo) '(2 3 5))
       (check-true (send-message fifo 'empty?))))))

(define dfs-node-tests
  (test-suite
   "dfs node"
   (test-case "correct name and no children"
     (let ([n (make-dfs-node 'name)])
       (check-equal? (send-message n 'get-name) 'name)
       (check-equal? (send-message n 'get-children) '())))
   (test-case "children in reverse order"
     (let ([n (make-dfs-node 'name)])
       (send-message n 'set-children! '(1 2 3))
       (check-equal? (send-message n 'get-children) '(3 2 1))))))

(define dfs-tests
  (test-suite
   "does dfs work"
   (check-equal? (dfs (list-ref graphs 0)) (list-ref dfs-traversals 0))
   (check-equal? (dfs (list-ref graphs 1)) (list-ref dfs-traversals 1))
   (check-equal? (dfs (list-ref graphs 2)) (list-ref dfs-traversals 2))
   (check-equal? (dfs (list-ref graphs 3)) (list-ref dfs-traversals 3))
   (check-equal? (dfs (list-ref graphs 4)) (list-ref dfs-traversals 4))
   (check-equal? (dfs (list-ref graphs 5)) (list-ref dfs-traversals 5))
   (check-equal? (dfs (list-ref graphs 6)) (list-ref dfs-traversals 6))
   (check-equal? (dfs (list-ref graphs 7)) (list-ref dfs-traversals 7))
   (check-equal? (dfs (list-ref graphs 8)) (list-ref dfs-traversals 8))
   (check-equal? (dfs (list-ref graphs 9)) (list-ref dfs-traversals 9))))

(define bfs-tests
  (test-suite
   "does bfs work"
   (check-equal? (bfs (list-ref graphs 0)) (list-ref bfs-traversals 0))
   (check-equal? (bfs (list-ref graphs 1)) (list-ref bfs-traversals 1))
   (check-equal? (bfs (list-ref graphs 2)) (list-ref bfs-traversals 2))
   (check-equal? (bfs (list-ref graphs 3)) (list-ref bfs-traversals 3))
   (check-equal? (bfs (list-ref graphs 4)) (list-ref bfs-traversals 4))
   (check-equal? (bfs (list-ref graphs 5)) (list-ref bfs-traversals 5))
   (check-equal? (bfs (list-ref graphs 6)) (list-ref bfs-traversals 6))
   (check-equal? (bfs (list-ref graphs 7)) (list-ref bfs-traversals 7))
   (check-equal? (bfs (list-ref graphs 8)) (list-ref bfs-traversals 8))
   (check-equal? (bfs (list-ref graphs 9)) (list-ref bfs-traversals 9))))

(run-tests (test-suite "all-tests" lifo-tests fifo-tests dfs-node-tests dfs-tests bfs-tests))
