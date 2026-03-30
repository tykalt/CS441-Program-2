#lang racket

;;; parser.rkt — Recursive Descent Parser (Phase 2)
;;; ============================================================

(require "scanner.rkt")
(provide parse-program)


;;; PART 1: PARSER STATE HELPERS
;;; ============================================================

(define (peek tokens)
  (if (null? tokens) 'EOF (token-type (car tokens))))

(define (peek-val tokens)
  (if (null? tokens) #f (token-val (car tokens))))

;; consume-end: consumes END-TOK, then optionally eats a following SEMI-TOK.
;; All of the professor's test files use "END;" even though the grammar
;; specifies bare "END" — this handles both forms gracefully.
(define (consume-end tokens)
  (let ([tokens1 (consume tokens 'END-TOK)])
    (if (eq? (peek tokens1) 'SEMI-TOK)
        (cdr tokens1)   ; eat the optional semicolon
        tokens1)))

(define (consume tokens expected-type)
  (cond
    [(null? tokens)
     (error 'syntax-error
            "Unexpected end of input — expected ~a" expected-type)]
    [(not (eq? (token-type (car tokens)) expected-type))
     (error 'syntax-error
            "Expected ~a but found ~a~a"
            expected-type
            (token-type (car tokens))
            (let ([v (token-val (car tokens))])
              (if v (format " ('~a')" v) "")))]
    [else (cdr tokens)]))

(define (consume-val tokens expected-type)
  (cond
    [(null? tokens)
     (error 'syntax-error
            "Unexpected end of input — expected ~a" expected-type)]
    [(not (eq? (token-type (car tokens)) expected-type))
     (error 'syntax-error
            "Expected ~a but found ~a~a"
            expected-type
            (token-type (car tokens))
            (let ([v (token-val (car tokens))])
              (if v (format " ('~a')" v) "")))]
    [else (values (token-val (car tokens)) (cdr tokens))]))



;;; PART 2: parse-factor
;;; ============================================================

(define (parse-factor tokens)
  (case (peek tokens)

    ;; Identifier: return it as a symbol, e.g. 'x
    [(ID-TOK)
     (values (string->symbol (peek-val tokens))
             (cdr tokens))]

    ;; Integer or floating-point literal: return the number itself
    [(INT-TOK FP-TOK)
     (values (peek-val tokens) (cdr tokens))]

    ;; Parenthesized sub-expression
    [(LPAREN-TOK)
     (let* ([tokens1 (consume tokens 'LPAREN-TOK)])
       (define-values (expr tokens2) (parse-expression tokens1))
       (values expr (consume tokens2 'RPAREN-TOK)))]

    ;; Anything else is a syntax error at this level
    [else
     (error 'syntax-error
            "Expected a value or '(' but found ~a" (peek tokens))]))



;;; PART 3: parse-term
;;; ============================================================

(define (parse-term tokens)
  (define-values (first-factor tokens1) (parse-factor tokens))
  (let loop ([ast  first-factor]
             [toks tokens1])
    (case (peek toks)
      [(STAR-TOK)
       (define-values (right toks2) (parse-factor (cdr toks)))
       (loop (list '* ast right) toks2)]
      [(SLASH-TOK)
       (define-values (right toks2) (parse-factor (cdr toks)))
       (loop (list '/ ast right) toks2)]
      [else
       (values ast toks)])))



;;; PART 4: parse-expression
;;; ============================================================

(define (parse-expression tokens)
  (define-values (first-term tokens1) (parse-term tokens))
  (let loop ([ast  first-term]
             [toks tokens1])
    (case (peek toks)
      [(PLUS-TOK)
       (define-values (right toks2) (parse-term (cdr toks)))
       (loop (list '+ ast right) toks2)]
      [(MINUS-TOK)
       (define-values (right toks2) (parse-term (cdr toks)))
       (loop (list '- ast right) toks2)]
      [else
       (values ast toks)])))



;;; PART 5: parse-comparison
;;; ============================================================

(define (relop-token->symbol tok-type)
  (case tok-type
    [(EQ-TOK)  'eq]
    [(NEQ-TOK) 'neq]
    [(GT-TOK)  'gt]
    [(GTE-TOK) 'gte]
    [(LT-TOK)  'lt]
    [(LTE-TOK) 'lte]
    [else #f]))

(define (parse-comparison tokens)
  (define-values (left tokens1) (parse-expression tokens))
  (let ([op-sym (relop-token->symbol (peek tokens1))])
    (unless op-sym
      (error 'syntax-error
             "Expected a comparison operator (=, !=, >, >=, <, <=) but found ~a"
             (peek tokens1)))
    (define-values (right tokens2) (parse-expression (cdr tokens1)))
    (values (list op-sym left right) tokens2)))



;;; PART 6: STATEMENT PARSERS
;;; ============================================================

;; Assignment: note consume-val returns both the identifier string
;; and the remaining tokens simultaneously via (values ...)
(define (parse-assign-stmt tokens)
  (define-values (id-str tokens1) (consume-val tokens 'ID-TOK))
  (let* ([tokens2 (consume tokens1 'ASSIGN-TOK)])
    (define-values (expr tokens3) (parse-expression tokens2))
    (values (list 'assign (string->symbol id-str) expr)
            (consume tokens3 'SEMI-TOK))))

;; IF: requires both ELSE and END — neither is optional
(define (parse-if-stmt tokens)
  (let* ([tokens1 (consume tokens 'IF-TOK)])
    (define-values (condition tokens2) (parse-comparison tokens1))
    (let* ([tokens3 (consume tokens2 'THEN-TOK)])
      (define-values (then-branch tokens4) (parse-stmt-list tokens3))
      (let* ([tokens5 (consume tokens4 'ELSE-TOK)])
        (define-values (else-branch tokens6) (parse-stmt-list tokens5))
        (values (list 'if condition then-branch else-branch)
                (consume-end tokens6))))))

;; WHILE: body is a StmtList between DO and END
(define (parse-while-stmt tokens)
  (let* ([tokens1 (consume tokens 'WHILE-TOK)])
    (define-values (condition tokens2) (parse-comparison tokens1))
    (let* ([tokens3 (consume tokens2 'DO-TOK)])
      (define-values (body tokens4) (parse-stmt-list tokens3))
      (values (list 'while condition body)
              (consume-end tokens4)))))

;; PRINT: single expression followed by semicolon
(define (parse-print-stmt tokens)
  (let* ([tokens1 (consume tokens 'PRINT-TOK)])
    (define-values (expr tokens2) (parse-expression tokens1))
    (values (list 'print expr)
            (consume tokens2 'SEMI-TOK))))



;;; PART 7: parse-statement
;;; ============================================================

(define (parse-statement tokens)
  (case (peek tokens)
    [(IF-TOK)    (parse-if-stmt    tokens)]
    [(WHILE-TOK) (parse-while-stmt tokens)]
    [(PRINT-TOK) (parse-print-stmt tokens)]
    [(ID-TOK)    (parse-assign-stmt tokens)]
    [else
     (error 'syntax-error
            "Expected a statement (IF/WHILE/PRINT/ID) but found ~a"
            (peek tokens))]))



;;; PART 8: parse-stmt-list
;;; ============================================================

(define (follow-token? tok-type)
  (member tok-type '(END-TOK ELSE-TOK EOF)))

(define (parse-stmt-list tokens)
  ;; We need at least one statement — check for premature stop token
  (when (follow-token? (peek tokens))
    (error 'syntax-error
           "Expected at least one statement but found ~a" (peek tokens)))
  ;; Parse statements, accumulating into a list
  (let loop ([toks  tokens]
             [stmts '()])
    (define-values (stmt toks2) (parse-statement toks))
    (let ([acc (append stmts (list stmt))])
      (if (follow-token? (peek toks2))
          ;; Done — wrap accumulated statements in a 'block node
          (values (cons 'block acc) toks2)
          ;; More statements follow — keep looping
          (loop toks2 acc)))))



;;; PART 9: parse-program — TOP-LEVEL ENTRY POINT
;;; ============================================================

(define (parse-program tokens)
  (define-values (stmts remaining) (parse-stmt-list tokens))
  ;; After the statement list, there must be nothing left
  (unless (eq? (peek remaining) 'EOF)
    (error 'syntax-error
           "Unexpected token after end of program: ~a~a"
           (peek remaining)
           (let ([v (peek-val remaining)])
             (if v (format " ('~a')" v) ""))))
  ;; Unwrap (block s1 s2 ...) -> (program s1 s2 ...)
  (cons 'program (cdr stmts)))
