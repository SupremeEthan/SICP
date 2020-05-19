#lang racket
(require rackunit rackunit/text-ui "parser-defns.rkt" "parser.rkt")

;;Test if thunk gives a value equal to val, without raising.
;;Meant for testing on valid input.
(define-check (equal-no-exn? val thunk)
  (let ([ret (with-handlers
               ;;Swallow every exception except breaks (e.g. ctrl c). If the thunk tries to raise an exn:test,
               ;;this it will still be reported as a test failure, just with some information lost. But we don't
               ;;expect that anyway.
               ([lex-error?
                 (lambda (exn)
                   ;;Specially handle spurious lex for better error messages
                   (fail-check (format "Spurious lex error at position ~v of ~v: ~a"
                                       (lex-error-input-pos exn)
                                       (lex-error-input-string exn)
                                       (lex-error-error-message exn))))]
                [parse-error?
                 (lambda (exn)
                   (fail-check (format "Spurious parse error: ~s\nCurrent Stack: ~v\nRemaining Tokens: ~v"
                                       (parse-error-error-message exn)
                                       (parse-error-current-stack exn)
                                       (parse-error-remaining-tokens exn))))]
                [(lambda (exn) (not (exn:break? exn)))
                 (lambda (exn) (fail-check (format  "An exception was raised: ~v" exn)))])
               (thunk))])
    (unless (equal? ret val)
      (fail-check (format "Expected: ~v;  Got ~v" val ret)))))

;; Produces a rackunit check verifying that (fun input) equals desired-result
(define (produces-output? fun input desired-result)
  (equal-no-exn? desired-result (thunk (fun input))))

(define valid-lexer-input
  (let ([lexes-to? (lambda (str result)
                     (equal-no-exn? result (thunk (lex str))))])
    (test-suite
     "Lexer gives correct result on valid inputs"
     (map (lambda (p) (produces-output? lex (car p) (cdr p)))
          ;;A list of input-output cons pairs.
          `(("" . ())
            ("(" . (,left-paren))
            (")" . (,right-paren))
            ("))))" . (,right-paren ,right-paren ,right-paren ,right-paren))
            ("123" . (123))
            ("asdf" . (asdf))
            ("\"asdf\"" . ("asdf"))
            ("\"\"" . (""))
            ("\"asdf\"1" . ("asdf" 1))
            ("asdf 1" . (asdf 1))
            ("asdf(" . (asdf ,left-paren))
            ("\"(\"(" . ("(" ,left-paren)))))))

(define invalid-lexer-input
  (test-suite
   "Lexer correctly rejects invalid input"
   (let ([errors-on? (lambda (str)
                       (check-exn lex-error? (thunk (lex str))
                                  (format "Didn't throw lex-error on invalid input: ~v" str)))])
     (map errors-on?
          (list "1a"
                "\""
                "\"aaaaaa\"\"")))))


(define valid-parser-input
  (test-suite
      "Parser handles valid token streams correctly."
    (map (lambda (p) (produces-output? parse (car p) (cdr p)))
         `(((,left-paren + 1 2 ,right-paren) . (+ 1 2))
           ((,left-paren + ,left-paren / 3 2 ,right-paren 1 ,right-paren) . (+ (/ 3 2) 1))
           ((,left-paren + 3 ,left-paren * 2 5 ,right-paren 1 ,right-paren) . (+ 3 (* 2 5) 1))
           ((,left-paren * 1 2 3 4 5 6 7 ,right-paren) . (* 1 2 3 4 5 6 7))
           ((123) . 123)
           ((,left-paren / ,left-paren + ,left-paren - 1 2 ,right-paren 3 ,right-paren 1 ,right-paren) . (/ (+ (- 1 2) 3) 1))))))

(define invalid-parser-input
  (test-suite
   "Parser rejects invalid input correctly."
   (let ([errors-on? (lambda (toks)
                       (check-exn parse-error? (thunk (parse toks))
                                  (format "Didn't throw parse-error on invalid input: ~v" toks)))])
     (map errors-on? 
          ;;List of token-lists.
          `((,left-paren)
            (,right-paren)
            (,left-paren + 1)
            (,left-paren + 3 ,left-paren * 2 5 1 ,right-paren))))))
(run-tests (test-suite "all tests" valid-lexer-input invalid-lexer-input valid-parser-input invalid-parser-input))
