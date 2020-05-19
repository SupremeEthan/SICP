#lang racket
(require "binomial-defns.rkt")

(provide evensplit split tree max-queue insert merge remove-max)

(provide make-queue)
(define (make-queue)
  '())
(provide forest)

(define (forest l)
  (map tree (split l)))

;; params: an even-length list of elements
;; returns: a list whose elements are combinations of consecutive pairs of input elements
(define (evensplit l)
  ; if l is null, then we reach the end
  (if (null? l)
      ; return an empty list
      '()
      (if (number? (car l))
          (cons (list (car l) (car (cdr l))) (evensplit (cddr l))) ; car of l is number
          (cons (append (car l) (car (cdr l))) (evensplit (cddr l))) ; car of l is a list
          ))
  )

(define (split l)
  ; if null, meaning the list cannot be evensplited further, so return empty list
  (if (null? l)
      '()
      ; if mod 2 of length is 0, cons an empty list
      (if (even? (length l))
          (cons '() (split (evensplit l)))
          ; else, take the car of list out and evensplit the remaining list
          (cons (car l) (split (evensplit (cdr l)))))))

; construct a tree using a list
(define (tree l)
  ; if the tree is null, then that means split result has a '(), then just do nothing
  (if (null? l)
      '()
      ; if not number, apply the tree-helper to the list if its length is larger than 1
      (if (number? l)
          l
          (if (= (length l) 1)
              (car l)
              (tree (tree-helper l))
              ))
      )
  )

; recursively combine two trees to all the elements in the list
(define (tree-helper lst)
  (if (null? lst)
      '()
      (cons (combine-tree (car lst) (car (cdr lst))) (tree-helper (cddr lst)))))

; combine two heap-ordered binomial tree
(define (combine-tree t1 t2)
  ; if all nums, combine them as a list
  (if (and (number? t1) (number? t2))
      (if (> t1 t2)
          (list t1 t2)
          (list t2 t1))
      ; else tack
      (if (> (car t1) (car t2))
          (tack t2 t1)
          (tack t1 t2))
      )
  )

; finds the largest element in the q
(define (max-queue q)
  (max-helper q 0)
  )

; loop through all the queues and find the max in the forest
(define (max-helper q max)
  (if (null? q)
      max
      ; if current tree's max is larger than max, update max
      (max-helper (cdr q) (if (> (find-tree-max (car q)) max)
                              (find-tree-max (car q))
                              max))
  ))

; find max in one tree
(define (find-tree-max t)
  (if (null? t)
      0
      (if (number? t)
          t
          (car t))))

; insert element x into the queue
(define (insert x q)
  (if (null? q)
      (list x)
      ; if there is no B0 tree in the q, let x be the B0 tree
      (if (null? (car q))
          (cons x (cdr q))
          (cons '() (insert (combine-tree x (car q)) (cdr q)))
          ))
  )

; merge two binomial queues
(define (merge q1 q2)
  ; if any one of the two queues is empty, just return the other one
  (if (null? q1)
      q2
      (if (null? q2)
          q1
          ; if the cars of both queues are non-empty, then we need to handle carry-over
          (if (and (not (null? (car q1))) (not (null? (car q2))))
              (cons '() (merge-helper (cdr q1) (cdr q2) (combine-tree (car q1) (car q2))))
              ; else, merge and handle carry-over
              (if (null? (car q1))
                  (cons (car q2) (merge (cdr q1) (cdr q2)))
                  ; else, merge...
                  (if (null? (car q2))
                      (cons (car q1) (merge (cdr q1) (cdr q2)))
                      (error"should not work!")))
              ))
      )
  )

(define (merge-helper q1 q2 add)
  ; handle last digit
  (if (null? q1)
      (insert add q2)
      ; handle last digit
      (if (null? q2)
          (insert add q1)
          ; have a new add, 1 + 1 + 1(add) = 1 + (new add)
          (if (and (not (null? (car q1))) (not (null? (car q2))))
              (cons add (merge-helper (cdr q1) (cdr q2) (combine-tree (car q1) (car q2))))
              (if (null? (car q1))
                  ; 1 + 0 + 1(add) = 0 + (new add)
                  (merge (cons add (cdr q1)) q2)
                  (if (null? (car q2))
                      (merge q1 (cons add (cdr q2)))
                      (error"should not work"))))))
  )

; remove the max element in the binomial queue
(define (remove-max q)
  ; max-value in the queue
  (define max-value (max-queue q))
  ; forest after removing the max-value in the tree containing the max-value
  (define max-tree (find-tree q max-value))
  ; forest after removing the tree containing max-value
  (define remaining-q (find-remaining q max-value))
  (if (null? (cdr q))
      '()
      (shorten (merge remaining-q max-tree)))
  )

; returns the remaining forest after removing the max-tree
(define (find-remaining q max)
  (if (= max (find-tree-max (car q)))
      ; return the forest and use '() to represent the removed tree's pos
      (cons '() (cdr q))
      (cons (car q) (find-remaining (cdr q) max)))
  )

; find the tree with the max-value in the forest
(define (find-tree q max)
  (if (= max (find-tree-max (car q)))
      (remove-top (car q))
      (find-tree (cdr q) max))
  )

; if the tree is a number or say a B0, return empty list
; else, return the cdr
(define (remove-top tree)
  (if (number? tree)
      '()
      (cdr tree)))

; removes the preceding unecessary '()s in the q
(define (shorten q)
  (if (null? q)
      '()
      ; reverse the reversed result back to original order
      (reverse (find-next (reverse q)))))

; reverse the q, let the preceding 0s become the head of the q
; e.g. 0001111 becomes 1111000
(define (reverse finish remain)
  (if (null? remain)
      finish
      (reverse (cons (car remain) finish) (cdr remain))))

; find next non-empty element and return that as lst
; e.g. 000111 becomes 111
(define (find-next lst)
  (if (null? (car lst))
      (find-next (cdr lst))
      lst))