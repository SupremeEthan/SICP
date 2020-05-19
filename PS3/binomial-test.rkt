#lang racket
(require rackunit rackunit/text-ui "binomial-defns.rkt" "binomial.rkt")
(require racket/pretty)
(define (random-shuffle! vec)
  (define len (vector-length vec))
  (define (swap! i j)
    (let ([ival (vector-ref vec i)]
          [jval (vector-ref vec j)])
      (vector-set! vec i jval)
      (vector-set! vec j ival)))  
  (let loop ([i 0])
    (if (= i len)
        (void)
        (swap! i (random i len)))))

(define test-lists
  (let ([v (list->vector (ints 0 99))])
    (random-seed 420)
    (for/list ([i (in-range 100)])
      (random-shuffle! v)
      (vector->list v))))

(define (heapsort l)
  (let loop ([heap (forest l)])
    (if (null? heap)
        '()
        (cons (max-queue heap)
              (loop (remove-max heap))))))

(define (remove-all heap)
  (if (null? heap)
      '()
      (cons (max-queue heap) (remove-max heap))))

(define (good-forest? forest original-list)
  
  (equal? (sort list) (sort (fringe forest))))


(define-check (equal-no-exn? val thunk)
  (let ([ret (with-handlers
               ;;Swallow every exception except breaks (e.g. ctrl c). If the thunk tries to raise an exn:test,
               ;;this it will still be reported as a test failure, just with some information lost. But we don't
               ;;expect that anyway.
               ([(lambda (exn) (not (exn:break? exn)))
                 (lambda (exn) (fail-check (format  "An exception was raised: ~v" exn)))])
               (thunk))])
    (unless (equal? ret val)
      (fail-check (format "Expected: ~v;  Got ~v" val ret)))))

(define (fringe t)
    (if (number? t)
        (list t)
        (append* (map fringe t))))

(define-check (check-forest input-lists)
  (with-handlers ([(lambda (exn) (not (exn:break? exn)))
                   (lambda (exn) (fail-check (format "exception thrown while checking forest: ~v\n" exn)))])
    (define heaps (map forest input-lists))
    (map (lambda (h)
                (unless (valid-heap? h)
                  (fail-check "forest returned an invalid heap on input. ~v" h)))
         heaps)

    (map (lambda (input-list heap)
           (unless (equal? (sort input-list >) (sort (fringe heap) >))
             (fail-check "forest contains wrong elements:\n Gave ~v\nGot ~v" input-list forest)))
         input-lists heaps)))

(define (test-forest l)
  (thunk
   (let ([f (forest l)])
     (and (valid-heap? f)
          (equal? (sort l >) (sort (fringe f) >))))))

(define forest-tests
  (test-suite
      "Does forest construct a correct heap?"
      (equal-no-exn? #t (test-forest (list-ref test-lists 0)))
      (equal-no-exn? #t (test-forest (list-ref test-lists 0)))
      (equal-no-exn? #t (test-forest (list-ref test-lists 6)))
      (equal-no-exn? #t (test-forest (list-ref test-lists 10)))))

(define (verify-evensplit in out)
  (define input-length (length in))
  (define input-elem-length
    (if (number? (car in)) ;assuming valid input
        2
        (length (car in))))
  (define (all-true? l) (foldl (lambda (x y) (and x y)) #t l))
  (define (good-element? elem)
    (and (all-true? (map number? elem))
         (= (length elem) (* 2 input-elem-length))))
  
  (and (= (length out) (/ (length in) 2)) ;;Is the output string half as long
       (all-true? (map good-element? out))
       (equal? (fringe out) in))) ;;does it have elements in the right order.


(define evensplit-tests
  (test-suite
   "Does evensplit work correctly?"
   (equal-no-exn? '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12) (13 14) (15 16) (17 18) (19 20))
                  (thunk (evensplit (ints 1 20))))
   (equal-no-exn? '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16) (17 18 19 20) (21 22 23 24) (25 26 27 28) (29 30 31 32))
                  (thunk (evensplit (evensplit (ints 1 32)))))))

(define split-tests
  (test-suite
   "Does split work correctly?"
   (equal-no-exn? '(() (1 2) () (3 4 5 6 7 8 9 10)) (thunk (split (ints 1 10))))
   (equal-no-exn? '(1 (2 3) (4 5 6 7) (8 9 10 11 12 13 14 15) (16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))
                  (thunk (split (ints 1 31))))))

(define tree-tests
  (test-suite
   "Does tree work correctly?"
   (equal-no-exn? 0 (thunk (binomial-tree-order (tree (ints 1 1)))))
   (equal-no-exn? 1 (thunk (binomial-tree-order (tree (ints 1 2)))))
   (equal-no-exn? 3 (thunk (binomial-tree-order (tree (ints 1 8)))))
   (equal-no-exn? 5 (thunk (binomial-tree-order (tree (ints 1 32)))))))


(define (correct-insert? x q)
  (define after-insert (insert x q))
  (cond [(not (valid-heap? q))
         #f]
        [(let ([before-elems  (fringe q)]
               [after-elems  (fringe after-insert)])
           (not (equal? (sort (cons x  before-elems) > ) (sort  after-elems >))))
         #f]
        [else #t]))

(define (correct-insert-list x l) (correct-insert? x (forest l)))
(define (correct-merge? a b)
   (define merged (merge a b))
   (cond [(not (valid-heap? merged))
          #f]
         [(let ([after-fringe (fringe merged)]
                [before-fringe (append (fringe a) (fringe b))])
            (not (equal? (sort after-fringe >) (sort after-fringe >))))
          #f]
         [else #t]))

(define (merge-test-lists i j)
  (thunk (correct-merge? (forest (list-ref test-lists i)) (forest (list-ref test-lists j)))))

(define (verify-heapsort i)
  (let ([l (list-ref test-lists i)])
    (thunk
     (equal? (sort l >) (heapsort l)))))

(run-tests (test-suite "all tests"
                       evensplit-tests
                       split-tests
                       tree-tests
                       forest-tests
                       (test-suite
                        "Does insert work?"
                        (let ([l1 (list-ref test-lists 7)]
                              [l2 (list-ref test-lists 50)])
                          (equal-no-exn? #t (thunk (correct-insert-list 100000 l1)))
                          (equal-no-exn? #t (thunk (correct-insert-list -1 l1)))
                          (equal-no-exn? #t (thunk (correct-insert-list 10000 l2)))
                          (equal-no-exn? #t (thunk (correct-insert-list -1 l2)))
                          (equal-no-exn? #t (thunk (correct-insert-list 50 l1)))
                          (equal-no-exn? #t (thunk (correct-insert-list 50 l2)))))
                       (test-suite
                        "Does merge work?"
                        (equal-no-exn? #t (merge-test-lists 1 2))
                        (equal-no-exn? #t (merge-test-lists 5 10))
                        (equal-no-exn? #t (merge-test-lists 31 90))
                        (equal-no-exn? #t (merge-test-lists 78 56))
                        (equal-no-exn? #t (merge-test-lists 34 45))
                        (equal-no-exn? #t (merge-test-lists 70 80)))
                       (test-suite
                        "Does remove work?"
                        (equal-no-exn? #t (verify-heapsort 75))
                        (equal-no-exn? #t (verify-heapsort 51))
                        (equal-no-exn? #t (verify-heapsort 42))
                        (equal-no-exn? #t (verify-heapsort 96))
                        (equal-no-exn? #t (verify-heapsort 62))
                        (equal-no-exn? #t (verify-heapsort 4))
                        (equal-no-exn? #t (verify-heapsort 7))
                        (equal-no-exn? #t (verify-heapsort 64))
                        (equal-no-exn? #t (verify-heapsort 80))
                        (equal-no-exn? #t (verify-heapsort 2))
                        (equal-no-exn? #t (verify-heapsort 83)))))

