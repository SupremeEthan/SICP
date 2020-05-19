#lang racket

(require "assembler.rkt")

(require racket/pretty)
;;Machine insructions:
;;Output
;;add, sub, divide, etc.
;;push-constant, popn
;;call, ret
;;cjump

(provide (struct-out machine-state))
(struct machine-state
  (;;a vector of machine instructions
   code-memory 
   ;;the current environment. a list of frames.
   environment
   ;;holds temporary values. all instructions read their operands by popping from the stack and write their results by pushing to the stack.
   data-stack
   ;;A list of stack frames. When we call a procedure, we save our current address and environment to be restored when the procedure returns. A stack frame is this pair of instruction pointer and environment.
   call-stack
   ;;An index into the code-memory, specifying whicn instruction to execute.
   instruction-pointer) #:transparent)

(struct machine-procedure
  (address
   environment
   arity) #:transparent)

(struct call-stack-frame
  (return-address
   data-stack
   environment))
;;Utility functions for manipulating machine states
(define (step-ip state)
  (struct-copy machine-state state
               [instruction-pointer
                (+ 1 (machine-state-instruction-pointer state))]))

(define (pop-value state)
  (let ([stack (machine-state-data-stack state)])
    (values (car stack)
            (struct-copy machine-state state
                         [data-stack (cdr stack)]))))

(define (push-value #:state state #:val val)
  (struct-copy machine-state state
               [data-stack (cons val (machine-state-data-stack state))]))

;;Implementations of all non-control-flow opcodes, with the IP increment
;;factored out.
(define (do-unop #:state state #:op op)
  (let-values ([(operand state) (pop-value state)])
    (push-value #:state state #:val (op operand))))
(define (do-binop #:state state #:op op)
  (let*-values ([(a state) (pop-value state)]
                [(b state) (pop-value state)])
    (push-value #:state state #:val (op a b))))
(define (do-push-constant #:state state #:const constant)
  (push-value #:state state #:val constant))

(define (do-popn #:state state #:n n)
  (let ([stack (machine-state-data-stack state)])
    (struct-copy machine-state state
                 [data-stack
                  (list-tail stack n)])))

(define (do-env-lookup #:state state #:frame frame-index #:var var-index)
  (let* ([env (machine-state-environment state)]
         [frame (list-ref env frame-index)]
         [val (vector-ref frame var-index)])
    (push-value #:state state #:val val)))

(define (do-env-mutate #:state state #:frame frame-index #:var var-index)
  (let-values ([(val state) (pop-value state)])
    (let* ([env (machine-state-environment state)]
           [frame (list-ref env frame-index)])
      (vector-set! frame var-index val)
      state)))

(define (do-make-procedure #:state state #:arity arity #:address address)
  (let* ([env (machine-state-environment state)]
         [proc (machine-procedure address env arity)])
    (push-value #:val proc #:state state)))

(define (push-stack-frame state)
  (let ([new-stack-frame
         (call-stack-frame (+ 1 (machine-state-instruction-pointer state))
                      (machine-state-data-stack state)
                      (machine-state-environment state))]
        [old-call-stack (machine-state-call-stack state)])
    (struct-copy machine-state state
                 [call-stack (cons new-stack-frame old-call-stack)])))

(define (pop-stack-frame #:state state)
  (let ([frame (car (machine-state-call-stack state))])
    (struct-copy machine-state state
                 [instruction-pointer (call-stack-frame-return-address frame)]
                 [environment (call-stack-frame-environment frame)]
                 [data-stack (call-stack-frame-data-stack frame)]
                 [call-stack (cdr (machine-state-call-stack state))])))

(define (do-tail-call #:state state #:arity arity)
  (let-values ([(fun state) (pop-value state)])
    (let ([entry-point (machine-procedure-address fun)]
          [fun-env (machine-procedure-environment fun)]
          [arg-env-frame (list->vector (take (machine-state-data-stack state)
                                             arity))])
      (struct-copy machine-state state
                   [instruction-pointer entry-point]
                   [environment (cons arg-env-frame fun-env)]
                   [data-stack '()]))))

(define (do-call #:state state #:arity arity)
  (let-values ([(fun state) (pop-value state)])
    (let* ([entry-point (machine-procedure-address fun)]
           [fun-env (machine-procedure-environment fun)]
           [arg-env-frame (list->vector (take (machine-state-data-stack state)
                                              arity))]
           [saved-data (drop (machine-state-data-stack state)
                             arity)]
           [saved-state (struct-copy machine-state state
                                     [data-stack saved-data])])
      (struct-copy machine-state (push-stack-frame saved-state)
                   [instruction-pointer entry-point]
                   [environment (cons arg-env-frame fun-env)]
                   [data-stack '()]))))

(define (do-ret state)
  (let* ([return-value (car (machine-state-data-stack state))]
         [return-state (pop-stack-frame #:state state)]
         [saved-data-stack (machine-state-data-stack return-state)])
    (struct-copy machine-state return-state
                 [data-stack (cons return-value saved-data-stack)])))

(define (do-cjump #:state state #:address address)
  (let-values ([(condition state) (pop-value state)])
    (let ([next-ip (if condition
                       address
                       (+ 1 (machine-state-instruction-pointer state)))])
      (struct-copy machine-state state
                   [instruction-pointer next-ip]))))

(provide halted?)
(define (halted? state)
  (let ([ip (machine-state-instruction-pointer state)]
        [code-mem (machine-state-code-memory state)])
    (or (>= ip (vector-length code-mem))
        (< ip 0)
        (let ([insn (vector-ref code-mem ip)])
          (eq? 'halt (car insn))))))

(provide step-vm)
(define (step-vm state)
  (if (halted? state)
      state
      (let* ([insn (vector-ref (machine-state-code-memory state)
                               (machine-state-instruction-pointer state))]
             [opcode (car insn)]
             [operands (cdr insn)])
        (match opcode
          ['call
           (let ([arity (car operands)])
             (do-call #:state state #:arity arity))]
          ['tail-call
           (let ([arity (car operands)])
             (do-tail-call #:state state #:arity arity))]
          ['ret
           (do-ret state)]
          ['cjump
           (let ([address (car operands)])
             (do-cjump #:state state #:address address))]
          [_
           ;;All the non-control-flow instructions require a separate
           ;;IP increment after their handlers
           (step-ip
            (let ([binary-op (lambda (op) (do-binop #:state state #:op op))]
                  [unary-op (lambda (op) (do-unop #:state state #:op op))])
              (match opcode
                ['add (binary-op +)]
                ['sub (binary-op -)]
                ['mul (binary-op *)]
                ['div (binary-op /)]
                ['equal (binary-op equal?)]
                ['cons (binary-op cons)]

                ['not (unary-op not)]
                ['car (unary-op car)]
                ['cdr (unary-op cdr)]
                
                ['env-lookup
                 (do-env-lookup #:state state #:frame (car operands)
                                #:var (cadr operands))]
                ['env-mutate
                 (do-env-mutate #:state state #:frame (car operands)
                                #:var (cadr operands))]

                ['push-constant
                 (do-push-constant #:state state #:const (car operands))]
                ['popn
                 (do-popn #:state state #:n (car operands))]
                ['make-procedure
                 (do-make-procedure #:state state #:address (car operands)
                                    #:arity (cadr operands))])))]))))

(provide run-program)
(define (run-program code)
  (let loop ([state (machine-state code '() '() '() 0)])
    (if (halted? state)
        state
        (loop (step-vm state)))))

(define-values (test-code-symtab test-code-mem)
  (assemble-and-link
   '((label main)
     (make-procedure L3 1) (push-constant #t) (cjump L4) (label L3) (push-constant 4) (env-lookup 0 0) (equal) (cjump L5) (push-constant 3) (env-lookup 0 0) (add) (push-constant #t) (cjump L6) (label L5) (push-constant 2) (env-lookup 0 0) (div) (label L6) (ret) (label L4)
     ;(push-constant 2) (push-constant 1) (make-procedure L7 1) (push-constant #t) (cjump L8) (label L7) (env-lookup 0 1) (env-lookup 0 0) (sub) (ret) (label L8) (call 2)
     ;and test (push-constant 1) (push-constant 1) (equal) (not) (cjump L3) (push-constant 2) (push-constant 2) (equal) (push-constant #t) (cjump L4) (label L3) (push-constant #f) (label L4)
     ;or test (push-constant 2) (push-constant 1) (equal) (cjump L1) (push-constant 4) (push-constant 3) (equal) (cjump L1) (push-constant #f) (push-constant #t) (cjump L2) (label L1) (push-constant #t) (label L2)
     ;call test (push-constant 2) (push-constant 1) (make-procedure L7 1) (push-constant #t) (cjump L8) (label L7) (env-lookup 0 1) (env-lookup 0 0) (sub) (ret) (label L8) (call 2)
     ;call test(push-constant 4) (make-procedure L1 1) (push-constant #t) (cjump L2) (label L1) (push-constant 4) (env-lookup 0 0) (equal) (cjump L3) (push-constant 2) (env-lookup 0 0) (mul) (push-constant #t) (cjump L4) (label L3) (push-constant 2) (env-lookup 0 0) (div) (label L4) (ret) (label L2) (call 1)
     (ret)
     )))

(module* main #f
  ;;(pretty-print test-code-mem)
  (when #t
    (let loop ([state (machine-state (list->vector test-code-mem)
                                     '()
                                     '()
                                     '()
                                     0)])
      (pretty-print state)
      (if (halted? state)
          (void)
          (loop (step-vm state))))))
