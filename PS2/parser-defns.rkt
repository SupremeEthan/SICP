#lang racket

;; The following variables contain special values that are distinct from all other values in the program.

(provide left-paren right-paren)
(define left-paren (string->uninterned-symbol "("))
(define right-paren (string->uninterned-symbol ")"))

;;These struct types are defined so you can raise them as exceptions on invalid input. Racket allows you to raise any value as an exception, but we will check for these specific types when we test your code on invalid inputs. We won't check the data fields; they are just there for your debugging convenience.
(provide (struct-out lex-error) (struct-out parse-error))
(struct lex-error (input-string input-pos error-message))
(struct parse-error (current-stack remaining-tokens error-message))

(provide letters punctuation decimal-digits whitespace symbol-characters delimiters)
(define letters "abcdefghijklmnopqrstufwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define punctuation "?!.+-*/<=>:$%^&_~@")
(define decimal-digits "0123456789")
(define whitespace " \n\t")
(define symbol-characters (string-append letters decimal-digits punctuation))
(define delimiters (string-append "()" whitespace))

;; Returns the index of the first occurence of the character 'needle' in the string 'haystack'. If not found, returns #f.
;; This function can be used both as a straightforward membership test, since any value besides #f counts as true
;; when used as the predicate in a cond or if expression. It can also be used to convert decimal characters to their
;; numerical value, when haystack is decimal-digits.
(provide string-member)
(define (string-member needle haystack)
  (let loop ([idx 0])
    (cond [(>= idx (string-length haystack))
	   #f]
	  [(equal? (string-ref haystack idx) needle)
	   idx]
	  [else (loop (+ idx 1))])))
