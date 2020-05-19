#lang racket
(require racket/pretty)
(define (build-symbol-table assembler-input)
  (let loop ([symtab '()]
             [ip 0]
             [statements assembler-input])
    (if (null? statements)
        symtab 
        (let ([statement (car statements)])
          (cond
            [(eq? 'label (car statement))
             (loop (cons (cons (cadr statement) ip) symtab)
                   ip
                   (cdr statements))]
            [else (loop symtab (+ 1 ip) (cdr statements))])))))



(define (assemble-instruction insn symtab)
  (match insn
    [(list 'make-procedure label arity)
     (list 'make-procedure (cdr (assq label symtab)) arity)]
    [(list 'cjump label)
     (list 'cjump (cdr (assq label symtab)))]
    [_ insn]))


(provide assemble)
(define (assemble input)
  (let ([symtab (build-symbol-table input)]
        [insns (filter (lambda (x) (not (eq? 'label (car x))))
                       input)])
    (values symtab 
            (map (lambda (i) (assemble-instruction i symtab))
                 insns))))

(provide assemble-and-link)
(define (assemble-and-link prog)
  (assemble (append bootstrap prog stdlib)))
(define bootstrap
  '((make-procedure not-asm 1)
    (make-procedure cdr-asm 1)
    (make-procedure car-asm 1)
    (make-procedure cons-asm 2)
    (make-procedure equal-asm 2)
    (make-procedure div-asm 2)
    (make-procedure times-asm 2)
    (make-procedure minus-asm 2)
    (make-procedure plus-asm 2)
    ;;After this, the standard library functions live on the stack.
    ;;We pass them as arguments to the user-supplied main function, thus loading them into the environment.
    (make-procedure main 9)
    (call 9)
    (halt)))
(define stdlib
  '(
    (label plus-asm)
    (env-lookup 0 0)
    (env-lookup 0 1)
    (add)
    (ret)
    (label minus-asm)
    (env-lookup 0 0)
    (env-lookup 0 1)
    (sub)
    (ret)
    (label times-asm)
    (env-lookup 0 0)
    (env-lookup 0 1)
    (mul)
    (ret)
    (label div-asm)
    (env-lookup 0 0)
    (env-lookup 0 1)
    (div)
    (ret)
    (label equal-asm)
    (env-lookup 0 0)
    (env-lookup 0 1)
    (equal)
    (ret)
    (label cons-asm)
    (env-lookup 0 0)
    (env-lookup 0 1)
    (equal)
    (ret)
    (label car-asm)
    (env-lookup 0 0)
    (car)
    (ret)
    (label cdr-asm)
    (env-lookup 0 0)
    (cdr)
    (ret)
    (label not-asm)
    (env-lookup 0 0)
    (not)
    (ret)))

(module* main #f
  (define testprog
    '((label main)
      (push-constant 1)
      (push-constant 0)
      (env-lookup 0 1) ;;load the + procedure from the global environment
      (call 2) ;;do (+ 1 2)
      (push-constant 3)
      (env-lookup 0 2) ;;load the * procedure from the global environment
      (call 2)
      (ret) ;;return 9
      ))
  (let-values ([(symtab result) (assemble-and-link testprog)])
    (pretty-print  result)))
