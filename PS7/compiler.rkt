#lang racket
(require racket/pretty)

; Thank you for the semester! Take care and good luck!
;    __      _
;  @'')}____//
;   `_/      )
;   (_(_/-(_/


;; compiler
(define (compile exp env)
  (cond [(constant? exp)
         (translate-constant exp env)]
        [(variable? exp)
         (translate-variable exp env)]
        [(if? exp)
         (translate-if exp env)]
        [(and? exp)
         (translate-and exp env)]
        [(or? exp)
         (translate-or exp env)]
        [(lambda? exp)
         (translate-lambda exp env)]
        [(eq? exp)
         (translate-general exp env "equal")]
        [(add? exp)
         (translate-general exp env "add")]
        [(sub? exp)
         (translate-general exp env "sub")]
        [(mul? exp)
         (translate-general exp env "mul")]
        [(div? exp)
         (translate-general exp env "div")]
        [(car? exp)
         (translate-general exp env "car")]
        [(cdr? exp)
         (translate-general exp env "cdr")]
        [(cons? exp)
         (translate-general exp env "cons")]
        [(call? exp)
         (translate-call exp env)]
        ))

;;------- some basic procedures

;; returns true if starts with certain tag
(define (tagged-with? exp tag)
  (if (pair? exp)
      (equal? (car exp) tag)
      #f))

;; global var to generate unique label
(define count 0)

;; generate unique label
(define (gensym)
  (lambda ()
    (begin (set! count (+ 1 count))
           (string->symbol (string-append* "L" (list (number->string count)))))))

;; get the length of the procedure
(define (length lst)
  (cond ((null? lst)
         0)
        (else
         (+ 1 (length (cdr lst))))))

;; reverse a list
(define (reverse lst)
   (if (null? lst)
       '()
       (append (reverse (cdr lst))
               (list (car lst)))))

;;------- condition checks
(define (constant? exp)
  (or (number? exp) (boolean? exp)))

(define (variable? exp)
  (not (pair? exp)))

(define (call? exp)
  (if (and (and (list? exp) (inner-tagged-with? (car exp) 'lambda)) (> (length exp) 1))
      #t
      #f))

(define (inner-tagged-with? exp statement)
  (cond [(equal? (car exp) statement) #t]
        [(list? (car exp)) (inner-tagged-with? (car exp) statement)]
        [else #f]))

(define (if? exp)
  (tagged-with? exp 'if))

(define (eq? exp)
  (tagged-with? exp 'eq?))

(define (even? exp)
  (tagged-with? exp 'even?))

(define (and? exp)
  (tagged-with? exp 'and))

(define (or? exp)
  (tagged-with? exp 'or))

(define (add? exp)
  (tagged-with? exp '+))

(define (sub? exp)
  (tagged-with? exp '-))

(define (mul? exp)
  (tagged-with? exp '*))

(define (div? exp)
  (tagged-with? exp '/))

(define (car? exp)
  (tagged-with? exp 'car))

(define (cdr? exp)
  (tagged-with? exp 'cdr))

(define (cons? exp)
  (tagged-with? exp 'cons))

(define (lambda? exp)
  (tagged-with? exp 'lambda))

;; Problem 3
;;------- translate-constant
(define (translate-constant exp env)
  (list 'push-constant exp))

;;------- translate-variable
;; test: (compile 'v '(a b c v)) returns (env-lookup 3 0)
;; test: (compile 'v '((a b v) (d e))) returns (env-lookup 0 2)
(define (translate-variable var env)
  (define frame (find-frame var env))
  (define entry (find-entry var env))
  (if (null? frame)
      (error "environment empty or variable not found")
      (list 'env-lookup frame entry)) ;; if frame returns a number, then compile them to a list
  )

;; lookup # of frame in the env
(define (find-frame var env)
  (define (search env frame var num)
    (cond [(null? env) '()]
          [(null? frame)
           (if (null? (cdr env))
               '() ;; this is the last env
               (search (cdr env) (cadr env) var (+ 1 count)))]
          [(if (list? frame) ;; if frame is a list, check it, otherwise check its pure value
               (equal? (car frame) var)
               (equal? frame var))
            num]
          [else
           (if (list? frame)
               (search env (cdr frame) var num)
               (search (cdr env) (cadr env) var (+ 1 num)))
           ]))
  (search env (car env) var 0))

;; similar to above, finds the # of entry
(define (find-entry var env)
  (define (search env frame var num)
    (cond [(null? env) '()]
          [(null? frame)
           (if (null? (cdr env))
               '()
               (search (cdr env) (cadr env) var 0))]
          [(if (list? frame)
               (equal? (car frame) var)
               (equal? frame var))
           num]
          [else
           (if (list? frame)
               (search env (cdr frame) var (+ 1 num))
               (search (cdr env) (cadr env) var 0))
           ]))
  (search env (car env) var 0))

;;Problem 4
;; test: (compile '((lambda (x y) (- x y)) 1 2) '())
;; test: (compile '((lambda (n) (if (eq? n 4) (/ n 2) (* n 2))) 4) '())

;;------- translate-call
(define (translate-call exp env)
  (define lambda-part (car exp)) ;; lambda part of the expression
  (define params (cadr lambda-part)) ;; params in the lambda part
  (define values (cdr exp)) ;; values passed to be used as params in lambda
  ;; compile values to a list of instructions
  (define (compile-vals values)
    (if (null? values)
        '()
        ;; if (car values) is not a number, meaning lambda takes lambda as input
        (if (number? (car values))
            (append (list (list 'push-constant (car values))) (compile-vals (cdr values)))
            (append (list (compile (car values) env)) (compile-vals (cdr values)))
            )
        ))

  (define compiled (reverse (compile-vals values)))
  ;(display compiled)
  (define len (length params)) ;; length of params
  (define lambda-compile-res (list (compile lambda-part env)))
  (append compiled
          (car lambda-compile-res)
          (list (append (list 'call) (list len)))))

;;Problem 5
;;------- translate-if
;; test (compile '(if (eq? 1 1) 1 -1) '())
;; test (compile '(if (eq? 1 1) (+ 1 2) (+ 4 5)) '())
(define (translate-if exp env)
  (define (if-predicate exp)
    (cadr exp))
  (define (if-consequent exp)
    (caddr exp))
  (define (if-alternative exp)
    (cadddr exp))
  (define then-label ((gensym)))
  (define end-label ((gensym)))
  (define cond-code (compile (if-predicate exp) env))
  (define then-code (list (compile (if-consequent exp) env)))
  (define else-code (list (compile (if-alternative exp) env)))
  (define then-jump (list (list 'cjump then-label)))
  (define end-jump (list (list 'push-constant #t)
                         (list 'cjump end-label)))
  ;(display then-code)
  (if (list? (caar then-code))
      (set! then-code (car then-code))
      (set! then-code then-code))
  ;(display then-code)
  (if (list? (caar else-code))
      (set! else-code (car else-code))
      (set! else-code else-code))
  
  ;(display cond-code)
  (append cond-code
          then-jump
          else-code
          end-jump
          (list (list 'label then-label))
          then-code
          (list (list 'label end-label))))

;;------- translate-and
;; test: (compile '(and (eq? 1 1) (eq? 1 2)) '()) --> #f
;; test: (compile '(and (eq? 1 1) (eq? 1 1)) '()) --> #t
(define (translate-and exp env)
  ;; generate labels
  (define first-label ((gensym)))
  (define second-label ((gensym)))
  ;; code blocks
  (define first-code (car (list (compile (cadr exp) env))))
  (define second-code (car (list (compile (caddr exp) env))))
  (define first-and-jump (list (list 'cjump first-label)))
  (define second-and-jump (list (list 'push-constant #t)
                                (list 'cjump second-label)))
  (append first-code
          (list (list 'not))
          first-and-jump ;; if first-code evaluated as false, will jump to the end
          second-code
          second-and-jump
          (list (list 'label first-label))
          (list (list 'push-constant #f))
          (list (list 'label second-label))
          )
  )

;;------- translate-or
;; test: (compile '(or (eq? 1 2) (eq? 3 4)) '())
;; test: (compile '(or (eq? 1 2) (eq? 4 4)) '())
(define (translate-or exp env)
  ;; generate labels
  (define first-label ((gensym)))
  (define second-label ((gensym)))
  ;; code blocks
  (define first-code (car (list (compile (cadr exp) env))))
  (define second-code (car (list (compile (caddr exp) env))))
  (define first-or-jump (list (list 'cjump first-label)))
  (define second-or-jump (list (list 'push-constant #t)
                               (list 'cjump second-label)))
  (append first-code
          first-or-jump ;; if first-code evaluated as true, jump and push a true
          second-code ;; else, run the second part
          first-or-jump ;; if second part evaluates as true, jump and push a true
          (list (list 'push-constant #f)) ;; else, "not" will turn false into true, and push a #f
          second-or-jump ;; then, jump to the end
          (list (list 'label first-label))
          (list (list 'push-constant #t))
          (list (list 'label second-label))
   ))

;;------- translate general expressions which takes two elements and perform a atomic operation such as add, cons, etc.
;; test: (compile '(eq? 1 2) '())
(define (translate-general exp env indicator)
  (define first-element (list (compile (cadr exp) env)))
  (define second-element (list (compile (caddr exp) env)))
  (define last-statement (list (list (string->symbol indicator))))
  ;; append indicator as symbol
  (append second-element
          first-element
          last-statement))

;;Problem 6 translate-lambda
;; test: (compile '(lambda (x y) (- x y)) '())
;; test: (compile '(lambda (n) (if (eq? n 4) (/ n 2) (+ n 3))) '())
(define (translate-lambda exp env)
  (define proc ((gensym)))
  (define proc-end ((gensym)))
  (define params (list (cadr exp)))
  (define rest (list (compile (caddr exp) params))) ;;procedure content, modified env
  (define var-len (length params)) ;; length of params
  (define end-jump (list (list 'push-constant #t)
                           (list 'cjump proc-end)))
  ;; extract
  (if (list? (caar rest))
      (set! rest (car rest))
      (set! rest rest))
  
  (append (list (append
                 (list 'make-procedure)
                 (list proc)
                 (list var-len))) ;; make-procedure syntax
          end-jump ;;jump to end when not being called
          (list (list 'label proc)) ;;
          rest
          (list (list 'ret))
          (list (list 'label proc-end))
          ))