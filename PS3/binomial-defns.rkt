#lang racket

(provide ints tack binomial-tree-order valid-heap?)
(define (ints from to)
  (if (> from to)
      '()
      (cons from (ints (+ from 1) to))))

(define (tack x l)
  (append l (list x)))

;;Here are some functions to check the validity of binomial trees, and binomial heaps.
;;You can view them as a restatement of the english definition given in the handout.

;A leaf's root is itself. A bigger tree's root is its car.
(define (tree-root t)
  (if (number? t) t (car t)))


;;If t is a valid heap-ordered binomial tree, returns the order of t.
;;Otherwise, returns #f.
;;A binomial tree of order 0 is a just a number (a leaf).
;;A binomial tree of order n is a number (the root) followed by n children.
;;The orders of its children range from 0...n-1.
;;The tree is heap ordered if it is order 0 or its root is bigger than all the roots of its
;;children, and (this is a recursive definition) the children are heap-ordered.
(define (binomial-tree-order t)
  (if (number? t)
      0
      (let* ([root (tree-root t)]
             [children (cdr t)]
             [num-children (length children)]
             [biggest-child-root (tree-root (argmax tree-root children))]
             [child-orders (map binomial-tree-order children)])
        (if (and (equal? child-orders (ints 0 (- num-children 1)))
                 (>= root biggest-child-root))
            num-children
            #f))))

(define (valid-heap? h)
  ;;A binomial heap is a list such that the element in the ith
  ;;position is either '() or a binomial tree of order i.
  ;;No trailing '()s are allowed.
  (let loop ([i 0]
             [remaining h])
    ;;Check out how we can define 'continue' as a regular old function.
    (define (continue) (loop (+ 1 i) (cdr remaining)))
    (cond [(null? remaining) #t] ;We've processed the whole list without encountering an error.
          [(null? (car remaining)) ;We've found an empty list.
           (if (null? (cdr remaining)) ;There are trailing '()s if and only if the last element is a '().
               #f
               (continue))] ;If the () isn't the last element, then there's nothing wrong so far.
          [(= i (binomial-tree-order (car remaining))) ;If it's nonempty, and is a good tree of the right order, nothing wrong so far.
           (continue)]
          [else #f])))

(define (descending? l)
  (equal? l (sort l >)))
