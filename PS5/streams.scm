;; This is the code for Streams and Lazy Evaluation

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define head car)
(define (tail s) (force (cdr s)))
(define stream-car car)
(define stream-cdr tail)

(define the-empty-stream (delay '()))
(define (empty-stream? s)
  (and (not (pair? s))
       (null? (force s))))

(define (1+ x) (+ 1 x))
(define (write-line x)
  (display x)
  (newline))

(define (divisible? x y) (= (remainder x y) 0))


;; Useful stream utility functions

(define (stream-filter pred stream)
  (cond ((empty-stream? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;;  Mapping functions

(define (stream-map proc stream)
  (if (empty-stream? stream)
      the-empty-stream
      (cons-stream (proc (stream-car stream))
                   (stream-map proc (stream-cdr stream)))))

;;  Iterating along a stream.

(define (stream-for-each proc stream)
  (if (empty-stream? stream)
      'done
      (begin (proc (stream-car stream))
             (stream-for-each proc (stream-cdr stream)))))

;;  Streams of numbers

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else (cons-stream (+ (stream-car s1) (stream-car s2))
                           (add-streams (stream-cdr s1) (stream-cdr s2))))))

(define (scale-stream c stream)
  (stream-map (lambda (x) (* x c)) stream))

;;  Differs from book by not checking for empty streams
(define (interleave s1 s2)
  (cons-stream (stream-car s1)
               (interleave s2
                           (stream-cdr s1))))

(define (merge s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (let ((h1 (stream-car s1))
               (h2 (stream-car s2)))
           (cond ((< h1 h2) (cons-stream h1 (merge (stream-cdr s1) s2)))
                 ((> h1 h2) (cons-stream h2 (merge s1 (stream-cdr s2))))
                 (else (cons-stream h1 
                                    (merge (stream-cdr s1) 
                                           (stream-cdr s2)))))))))



(define print-stream
  (let ()
    (define (iter s)
      (if (empty-stream? s)
          (display "]")
          (begin (write (stream-car s))
                 (write " ")
                 (iter (stream-cdr s)))))
    (lambda (s)
      (write "")
      (iter s))))
;; You may wonder why PRINT-STREAM has been written in such an obscure
;; way, when
;; (define (print-stream s)
;;   (write "[")
;;   (stream-for-each (lambda (x) (write x) (write " ")) s)
;;   (write "]"))
;; would have the same effect.  If you think about the "actor model"
;; and tail recursion, however, you may begin to see why.

;;  For exercise 3.43
(define (show x)
  (write-line x)
  x)

(define (nth-stream n s)
  (if (= n 0)
      (stream-car s)
      (nth-stream (- n 1) (stream-cdr s))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (enumerate-interval (1+ low) high))))

;; below are answers
;; ----------------------------------------------------------------------------
;; Problem 1
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
;; verify that integers stream work 
;; (print-stream integers)

(define div-predicate
  (lambda (x)
    (if (or (divisible? x 2) (divisible? x 3) (divisible? x 5))
        #f
        #t)))
;; the stream of all integers that are not divisible by 2, 3, 5
(define not-divisible-stream (stream-filter div-predicate integers))
(print-stream not-divisible-stream)

;; ----------------------------------------------------------------------------
;; Problem 2
;; stream of ints that are not divisible by 7
(define not-divisible-by-7 (stream-filter (lambda (x) (if (divisible? x 7) #f #t)) integers))
; (print-stream not-divisible-by-7)
(define not-divisible-by-3 (stream-filter (lambda (x) (if (divisible? x 3) #f #t)) integers))

;; First few elements of interleave-two-streams: "1" "1" "2" "2" "3" "4" "4" "5" "5" "7" "6" "8" "8"
;; I use print-stream and interleave procedures to find the first few elements
(define interleave-two-streams (interleave not-divisible-by-7 not-divisible-by-3))
;; (print-stream interleave-two-streams)

;; ----------------------------------------------------------------------------
;; Problem 3
(define alt (cons-stream 0 (interleave integers alt)))

;; (print-stream alt)

;; ----------------------------------------------------------------------------
;; Problem 4
(define (pairs s t)
  (cons-stream (cons (stream-car s) (stream-car t))
               (interleave
                (stream-map (lambda (x) (cons (stream-car s) x))
                            (stream-cdr t))
                (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave-pairs s t)
  (pairs s t))

; (print-stream (interleave-pairs integers integers))

;; ----------------------------------------------------------------------------
;; Problem 5

;; this is modified from merge, which takes an extra arguement weight, a proc to compute the
;; weight based on given formula
(define (merge-weighted s1 s2 weight)
  ;; (stream-car s1) is the param p for weight proc
  (let ((w1 (weight (stream-car s1)))
        (w2 (weight (stream-car s2))))
    (cond
      ((> w1 w2) (cons-stream (stream-car s2) (merge-weighted s1 (stream-cdr s2) weight)))
      ((< w1 w2) (cons-stream (stream-car s1) (merge-weighted (stream-cdr s1) s2 weight)))
      (else (cons-stream (stream-car s1)
                              (cons-stream (stream-car s2) (merge-weighted (stream-cdr s1) 
                                                                           (stream-cdr s2) weight))))
      )
    )
  )


;; This next procedure is to be used in forming streams of pairs,
;; once you have defined the procedure MERGE-WEIGHTED
(define (weighted-pairs s t pair-weight)
  (cons-stream (cons (stream-car s) (stream-car t))
               (merge-weighted
                (stream-map (lambda (x) (cons (stream-car s) x))
                            (stream-cdr t))
                (weighted-pairs (stream-cdr s) (stream-cdr t) pair-weight)
                (lambda (p) (pair-weight (car p) (cdr p))))))

;; ----------------------------------------------------------------------------
;; Problem 6

;; part(a)

(define order-by-pure-sum
  (lambda (i j)
    (+ i j)))
(define order-by-pure-sum-test (weighted-pairs integers integers order-by-pure-sum))
; (print-stream order-by-pure-sum-test)

;; part(b)

(define (cube x) (* x x x))
(define order-by-cube-sum
  (lambda (i j)
    (+ (cube i) (cube j))))
(define order-by-cube-sum-test (weighted-pairs integers integers order-by-cube-sum))
; (print-stream order-by-cube-sum-test)

;; part(c)
;; div-predicate is defined previously, which returns true if the number is not divisible by 2, 3, or 5

(define 235s-removed-stream (stream-filter
                 (lambda (n) (div-predicate n)) integers))
  
(define order-by-strange-order
  (lambda (i j)
    (+ (* 2 i) (* 3 j) (* 5 i j))
    )
  )

(define order-by-strange-order-test (weighted-pairs 235s-removed-stream 235s-removed-stream order-by-strange-order))
; (print-stream order-by-strange-order-test)

;; ----------------------------------------------------------------------------
;; Problem 7

;; call the helper function which has another param that is a stack to help cache pairs of the same weight
(define (combine-same-weights pair-stream pair-weight)
  (combine-same-weights-helper pair-stream pair-weight '())
  )

;; this is the helper method that cache the original pair-stream into a stack
;; and cons-stream the stack back to the output stream when all pairs of same weight is found
(define (combine-same-weights-helper pair-stream pair-weight stack)
  (let ((current-pair (stream-car pair-stream)))
    ;; if the stack is empty, that means (stream-car pair-stream) is a pair with a new pair weight
    (if (null? stack)
        ;; cons the weight with the pair
        (begin
          (set! stack (cons (pair-weight (car current-pair) (cdr current-pair)) (list (cons (car current-pair) (cdr current-pair)))))
          (combine-same-weights-helper (stream-cdr pair-stream) pair-weight stack))
        ;; if the stack is not empty, then we need to check if current-pair's weight is the same as the (car stack)
        ;; which is the weight of all pairs in the stack
        (if (= (pair-weight (car current-pair) (cdr current-pair)) (car stack))
            ;; if so, then we append this new pair to the stack
            (begin
              (set! stack (append stack (list (cons (car current-pair) (cdr current-pair)))))
              (combine-same-weights-helper (stream-cdr pair-stream) pair-weight stack))
            ;; otherwise, there is no more pairs to add, we cons the stack to output stream and clear the stack
            (cons-stream
             stack
             (combine-same-weights-helper pair-stream pair-weight '())))
        )
    )
  )
  
;; This procedure forms streams of weighted pairs, where pairs of the
;; same weight have been combined.  In order to use it, you must
;; define an appropriate procedure COMBINE-SAME-WEIGHTS
(define (same-weight-pairs s t pair-weight)
  (combine-same-weights (weighted-pairs s t pair-weight)
                        pair-weight))
; (print-stream (same-weight-pairs integers integers order-by-pure-sum))

;; ----------------------------------------------------------------------------
;; Problem 8

(define ram-filter
  (lambda (x)
    ;; length includes the pair weight. So, if the length is 3, that means there
    ;; are 2 pairs
    (if (> (length x) 2)
        #t
        #f)))

;; the first five Ramanujan numbers are: 1729, 4104 13832 20683 32832
(define ramanujan-nums (stream-filter ram-filter (same-weight-pairs integers
                                 integers
                                 (lambda (i j) (+ (cube i) (cube j))))))

; (print-stream ramanujan-nums)

;; ----------------------------------------------------------------------------
;; Problem 9

;; part(a)
(define filter-3-sum
  (lambda (x)
    ;; length includes the pair weight. So, if the length is 4, that means there
    ;; are 3 pairs
    (if (> (length x) 3)
        #t
        #f)))

;; below is the result stream

;; the first five elements are:
;; 1) "(87539319 (167 . 436) (228 . 423) (255 . 414))"
;; 2) "(119824488 (11 . 493) (90 . 492) (346 . 428))"
;; 3) "(143604279 (111 . 522) (359 . 460) (408 . 423))"
;; 4) "(175959000 (70 . 560) (198 . 552) (315 . 525))"
;; 5) "(327763000 (300 . 670) (339 . 661) (510 . 580))"

;; the numbers can be writtens as the cube sum of all pairs
;; e.g. 87539319 = 167^3 + 436^3 = 228^3 + 423^3 = 255^3 + 414^3

;; so, the five smallest numbers are: 87539319, 119824488, 143604279, 175959000, 327763000
(define 3-ways-sum (stream-filter filter-3-sum (same-weight-pairs integers
                                 integers
                                 (lambda (i j) (+ (cube i) (cube j))))))

; (print-stream 3-ways-sum)

;; part(b)

(define odd-even-filter
  (lambda (x)
    ;; have at least two pairs
    (if (> (length x) 2)
        ;; check if all pairs satisfy the requirement
        (filter-helper (cdr x))
        #f)))

;; helper method that iterate through all the pairs in the list
;; returns true if all pairs (i . j) in the list consists of an odd i and an even j
;; otherwise, return false
(define (filter-helper lst)
  (if (null? lst)
      #t
      ;; when list is not null
      (if (and (odd? (car (car lst))) (even? (cdr (car lst))))
          ;; recurse
          (filter-helper (cdr lst))
          #f
          )
      )
  )

(define (square x) (* x x))

;; below is the result stream
;; the pairs are in form (i . j) where i is odd and j is even

;; the first five elements are:
;; 1) "(4901 (1 . 70) (13 . 52))"
;; 2) "(6209 (5 . 78) (17 . 36))"
;; 3) "(6427 (3 . 80) (7 . 78))"
;; 4) "(6849 (5 . 82) (17 . 44))"
;; 5) "(7731 (11 . 80) (15 . 66))"

;; so, the five smallest numbers are: 4901, 6209, 6427, 6849, 7731

(define even-odd-sum (stream-filter odd-even-filter (same-weight-pairs integers
                                 integers
                                 (lambda (i j) (+ (cube i) (square j))))))


; (print-stream even-odd-sum)