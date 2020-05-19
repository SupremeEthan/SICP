#lang racket
(require "parser-defns.rkt")

;; Here is the sample tokenizer, provided as an example of string
;; processing and looping. It's structured not as one main loop
;; containing method calls, but as a handful of mutually recursive
;; procedures.
(provide lex)

(define (lex string)
  (dispatch string 0))

(define (dispatch string current-position)
  (if (>= current-position (string-length string))
      ;; the end of the string
      '()
      ;; The syntax for indexing a string is quite verbose, so we would like to
      ;; just keep the current character in a variable. But we need to make sure
      ;; that we're not out of bounds before we actually index the string to
      ;; determine the current character. So we need this pattern of nesting
      ;; a cond inside an if.
      (let ([current-character (string-ref string current-position)])
	(cond
          [(equal? current-character #\()
           (consume-left-paren string current-position)]
          [(equal? current-character #\))
           (consume-right-paren string current-position)]
          [(string-member current-character whitespace)
           (consume-whitespace string current-position current-position)]
          ;; consume symbol, must start with letter
          [(string-member current-character letters)
           (consume-symbol string current-position current-position)]
          ;; consume string
          ;; should start with \"
          [(equal? current-character #\")
           (consume-string string current-position (+ 1 current-position))]
          [else (consume-number string current-position current-position 0)]))))


;;This isn't to different from a single recursive procedure creating a list,
;;like map. But it's multiple procedures each consing on an element to the
;;result of a *different* procedure.
(define (consume-left-paren string current-position)
  (cons left-paren (dispatch string (+ 1 current-position))))

(define (consume-right-paren string current-position)
  (cons right-paren (dispatch string (+ 1 current-position))))


;;This procedure conses a big string of whitespace onto the output list, which is
;;*NOT* the desired behavior. So you will need to change it to merely skip the whitespace.
(define (consume-whitespace string whitespace-start current-position)
  (if (or (>= current-position (string-length string))
          (not (string-member (string-ref string current-position) whitespace))) ;; if out of bound or string-member returns false --> whitespace is not found
      ;;Specifically, you will need to change THIS LINE BELOW
      (dispatch string current-position) ;; terminate and skip all the whitespaces
      (consume-whitespace string whitespace-start (+ 1 current-position))))

;;You'll need to change this to actually convert a string to a number, and also to
;;add proper error checking.
(define (consume-number string number-start current-position accumulator)
  (cond
    ;; condition 1: index out of bound or seeing a delimiter
    [(or (>= current-position (string-length string))
         (string-member (string-ref string current-position) whitespace)
         (eq? #\( (string-ref string current-position))
         (eq? #\) (string-ref string current-position))
         )
     (cons accumulator
           (dispatch string current-position))]
    ;; condition 2: valid number
    [(string-member (string-ref string current-position) decimal-digits) ;; if current-position is a valid digit
     (consume-number string number-start (+ 1 current-position) (+ (* accumulator 10) (- (char->integer (string-ref string current-position)) 48)))]
    ;; condition 3: invalid input, say seeing a letter before a delimiter
    [else (raise (lex-error string current-position "bad number"))]))

(define (consume-symbol string string-start current-position)
  (cond
    ;; condition 1: index out of bound or seeing a delimiter
    [(or (>= current-position (string-length string))
         (string-member (string-ref string current-position) whitespace)
         (eq? #\( (string-ref string current-position))
         (eq? #\) (string-ref string current-position))
         )
     (cons (string->symbol (substring string string-start current-position))
           (dispatch string current-position))]
    ;; condition 2: valid symbol
    ;; check if current-position is a valid symbol-character
    ;; the first char is a letter, which is checked during dispatch, all the subsequent chars must be in symbol-characters
    [(string-member (string-ref string current-position) symbol-characters) 
     (consume-symbol string string-start (+ 1 current-position))]
    ;; condition 3: invalid input
    [else (raise (lex-error string current-position "bad symbol"))]))

(define (consume-string string string-start current-position)
  (cond
    ;; condition 1: index inbound and we find another "\""
    [(and (< current-position (string-length string))
          (equal? (string-ref string current-position) #\"))
     ;; cons the substring to the result and skip "\"", substring is exclusive
     (cons (substring string (+ 1 string-start) current-position)
           (dispatch string (+ 1 current-position)))]
    ;; condition 2: index inbound but haven't found a "\"" yet
    [(< current-position (string-length string))
     (consume-string string string-start (+ 1 current-position))]
    ;; condition 3: error, bad string
    [else (raise (lex-error string current-position "bad string"))]))

;;No example code here, but a parsing algorithm is described in the handout.
(provide parse)
(define (parse toks)
  ;; if token null, raise an error, else, call the helper function
  (if (null? toks)
      (raise (parse-error '() toks "token empty"))
      (parse-helper '() toks)
  )
  )
;; the help function that use a stack to track the tokens and
;; combine tokens to a list whenever we find a pair of parens match
(define (parse-helper stack tokens)
  (cond
    ;; if token null, we determine whether we the expression is valid or not
    [(null? tokens)
     (if (and (= (length stack) 1) ;; have only element inside the stack, we want to see if that's a list or not
              (not (eq? (car stack) left-paren)) ;; not parens
              (not (eq? (car stack) right-paren))
              (not (string-member (car stack) symbol-characters)) ;; not characters
              )
         ;; if true, then the only one element in the stack is a nested list
         (car stack) ;; the expression is valid, we return the only one list inside the stack
         ((raise (parse-error stack tokens "parens mismatch")))) ;; otherwise, we have a mismatch in parens
     ]
    ;; if token not null, if we meet a right-paren, we call another helper function;
    ;; else, we cons the top of the tokens with the stack and cdr tokens
    [else (if (eq? (car tokens) right-paren)
              (find-left-paren-in-stack stack '() (cdr tokens)) ;; skip the right-paren token
              (parse-helper (cons (car tokens) stack) (cdr tokens)))]
    )
  )

;; this function finds a left-paren in the stack and parse a valid pair of parens into a list
(define (find-left-paren-in-stack stack nested-list tokens)
  (cond
    ;; if stack is null, then there's a mismatch in parens
    [(null? stack) (raise (parse-error stack tokens "mismatch parens when finding valid pair!"))]
    ;; if not left-paren, keep finding
    [(not (eq? (car stack) left-paren))
     (find-left-paren-in-stack (cdr stack) (cons (car stack) nested-list) tokens)]
    ;; if is a left-paren, push the list into the cdr of the stack, skip the left-paren token
    [else
     (parse-helper (cons nested-list (cdr stack)) tokens)]))