#lang racket

;;; scanner.rkt — Lexical Analysis (Phase 1)
;;; ============================================================

(provide make-token token-type token-val
         strip-comments tokenize scan)


;;; PART 1: TOKEN CONSTRUCTORS & ACCESSORS
;;; ============================================================
(define (make-token type val) (list type val))
(define (token-type tok) (car  tok))
(define (token-val  tok) (cadr tok))

;; Keyword table: uppercase string -> token-type symbol
(define KEYWORDS
  '(("IF"    . IF-TOK)
    ("THEN"  . THEN-TOK)
    ("ELSE"  . ELSE-TOK)
    ("END"   . END-TOK)
    ("WHILE" . WHILE-TOK)
    ("DO"    . DO-TOK)
    ("PRINT" . PRINT-TOK)))


;;; PART 2: STRIP-COMMENTS
;;; ============================================================
(define (strip-comments src)
  (let loop ([chars      (string->list src)]
             [in-comment #f]
             [result     '()])
    (cond
      ;; Base case: end of input
      [(null? chars)
       (list->string (reverse result))]

      ;; ---- Inside a comment ----
      [in-comment
       (cond
         ;; Found closing */ — exit comment, emit one space
         [(and (char=? (car chars) #\*)
               (pair? (cdr chars))
               (char=? (cadr chars) #\/))
          (loop (cddr chars) #f (cons #\space result))]
         ;; Any other char inside comment — discard it
         [else
          (loop (cdr chars) #t result)])]

      ;; ---- Outside a comment ----
      [else
       (cond
         ;; Found opening /* — enter comment mode
         [(and (char=? (car chars) #\/)
               (pair? (cdr chars))
               (char=? (cadr chars) #\*))
          (loop (cddr chars) #t result)]
         ;; Normal character — keep it
         [else
          (loop (cdr chars) #f (cons (car chars) result))])])))



;;; PART 3: CHARACTER CLASSIFIERS
;;; ============================================================

(define (digit?        c) (char<=? #\0 c #\9))
(define (nonzero-digit? c) (char<=? #\1 c #\9))
(define (letter?       c) (or (char<=? #\a c #\z)
                               (char<=? #\A c #\Z)))
;; Identifier start: letter or underscore
(define (id-start?     c) (or (letter? c) (char=? c #\_)))
;; Identifier tail: letter, digit, underscore, or hyphen (per grammar)
(define (id-tail?      c) (or (letter? c) (digit? c)
                               (char=? c #\_) (char=? c #\-)))
(define (whitespace?   c) (member c '(#\space #\tab #\newline #\return)))



;;; PART 4: NUMBER SCANNING HELPERS
;;; ============================================================
(define (scan-digits chars)
  (let loop ([cs chars] [acc '()])
    (if (and (pair? cs) (digit? (car cs)))
        (loop (cdr cs) (cons (car cs) acc))
        (cons (list->string (reverse acc)) cs))))

(define (scan-number chars sign-char)
  (define sign-str (if sign-char (string sign-char) ""))

  (let* ([int-scan   (scan-digits chars)] ; scan digits before any '.'
         [before-str (car int-scan)] ; e.g. "3" in "3.14"
         [after-int  (cdr int-scan)]) ; remaining after those digits

    (cond

      ;; Conditions:
      ;;   1. There are digits before the '.'
      ;;   2. Next char is '.'
      ;;   3. The char after '.' is a digit ("0." invalid, "0.5" valid)
      [(and (> (string-length before-str) 0)
            (pair? after-int)
            (char=? (car after-int) #\.)
            (pair? (cdr after-int))
            (digit? (cadr after-int)))
       (let* ([after-dot  (cdr after-int)] ; skip the '.'
              [frac-scan  (scan-digits after-dot)]
              [frac-str   (car frac-scan)]
              [remaining  (cdr frac-scan)])
         ;; Guard: no second decimal point
         (when (and (pair? remaining) (char=? (car remaining) #\.))
           (error 'lexical-error
                  "Invalid FP: multiple decimal points in '~a~a.~a'"
                  sign-str before-str frac-str))
         (let ([val (string->number
                     (string-append sign-str before-str "." frac-str))])
           (cons (make-token 'FP-TOK val) remaining)))]

      ;; Trailing-dot error: digits followed by '.' but no digit after ("0." or "13.")
      ;; must be caught BEFORE the INT branch so we don't silently return INT 0 and leave '.' in the stream.
      [(and (> (string-length before-str) 0)
            (pair? after-int)
            (char=? (car after-int) #\.))
       (error 'lexical-error
              "Invalid FP: no digits after decimal point in '~a~a.'"
              sign-str before-str)]

      ;; INT branch: just digits 
      [(> (string-length before-str) 0)
       ;; Leading-zero rule: "0" alone is fine; "05" or "007" are not.
       (when (and (> (string-length before-str) 1)
                  (char=? (string-ref before-str 0) #\0))
         (error 'lexical-error
                "Invalid INT: leading zero in '~a~a'" sign-str before-str))
       (let ([val (string->number (string-append sign-str before-str))])
         (cons (make-token 'INT-TOK val) after-int))]

      ;; Nothing matched
      [else #f])))



;;; PART 5: TOKENIZE
;;; ============================================================
(define (tokenize src)

  ;; These prev-types signal that the upcoming +/- is BINARY
  (define (value-token-type? t)
    (member t '(ID-TOK INT-TOK FP-TOK RPAREN-TOK)))

  (let loop ([chars     (string->list src)]
             [tokens    '()]
             [prev-type #f])

    (cond
      ;; Base case
      [(null? chars) (reverse tokens)]

      ;; Whitespace: skip silently
      [(whitespace? (car chars))
       (loop (cdr chars) tokens prev-type)]

      ;; Unary +/- attached to a number literal
      ;; Conditions: current char is + or -, the PREVIOUS token
      ;; was NOT a value (so this is unary), and the NEXT char is
      ;; a digit (so this sign belongs to a number, not to an op).
      [(and (member (car chars) '(#\+ #\-))
            (not (value-token-type? prev-type))
            (pair? (cdr chars))
            (digit? (cadr chars)))
       (let ([result (scan-number (cdr chars) (car chars))])
         ;; result = (token . remaining-chars)
         (loop (cdr result)
               (cons (car result) tokens)
               (token-type (car result))))]

      ;; Digit: unsigned number
      [(digit? (car chars))
       (let ([result (scan-number chars #f)])
         (loop (cdr result)
               (cons (car result) tokens)
               (token-type (car result))))]

      ;; Stray '.' (FP with nothing before decimal)
      [(char=? (car chars) #\.)
       (error 'lexical-error
              "Invalid FP: no digits before decimal point")]

      ;; Identifier or keyword
      [(id-start? (car chars))
       (let id-loop ([cs  (cdr chars)]
                     [acc (list (car chars))])
         (if (and (pair? cs) (id-tail? (car cs)))
             (id-loop (cdr cs) (cons (car cs) acc))
             (let* ([word (list->string (reverse acc))]
                    [kw   (assoc word KEYWORDS)]
                    [tok  (if kw
                              (make-token (cdr kw) word)
                              (make-token 'ID-TOK word))])
               (loop cs (cons tok tokens) (token-type tok)))))]

      ;; Two-character operators (checked before single-char)

      ;; := (assignment)
      [(and (char=? (car chars) #\:)
            (pair? (cdr chars))
            (char=? (cadr chars) #\=))
       (loop (cddr chars)
             (cons (make-token 'ASSIGN-TOK ":=") tokens)
             'ASSIGN-TOK)]

      ;; != (not-equal)
      [(and (char=? (car chars) #\!)
            (pair? (cdr chars))
            (char=? (cadr chars) #\=))
       (loop (cddr chars)
             (cons (make-token 'NEQ-TOK "!=") tokens)
             'NEQ-TOK)]

      ;; >= (greater-or-equal)
      [(and (char=? (car chars) #\>)
            (pair? (cdr chars))
            (char=? (cadr chars) #\=))
       (loop (cddr chars)
             (cons (make-token 'GTE-TOK ">=") tokens)
             'GTE-TOK)]

      ;; <= (less-or-equal)
      [(and (char=? (car chars) #\<)
            (pair? (cdr chars))
            (char=? (cadr chars) #\=))
       (loop (cddr chars)
             (cons (make-token 'LTE-TOK "<=") tokens)
             'LTE-TOK)]

      ;; Single-character tokens

      [(char=? (car chars) #\=)
       (loop (cdr chars) (cons (make-token 'EQ-TOK    "=")  tokens) 'EQ-TOK)]
      [(char=? (car chars) #\>)
       (loop (cdr chars) (cons (make-token 'GT-TOK    ">")  tokens) 'GT-TOK)]
      [(char=? (car chars) #\<)
       (loop (cdr chars) (cons (make-token 'LT-TOK    "<")  tokens) 'LT-TOK)]
      [(char=? (car chars) #\+)
       (loop (cdr chars) (cons (make-token 'PLUS-TOK  "+")  tokens) 'PLUS-TOK)]
      [(char=? (car chars) #\-)
       (loop (cdr chars) (cons (make-token 'MINUS-TOK "-")  tokens) 'MINUS-TOK)]
      [(char=? (car chars) #\*)
       (loop (cdr chars) (cons (make-token 'STAR-TOK  "*")  tokens) 'STAR-TOK)]
      [(char=? (car chars) #\/)
       ;; Check for // — not a valid comment delimiter in this language
       (if (and (pair? (cdr chars)) (char=? (cadr chars) #\/))
           (error 'lexical-error
                  "'//' is not a valid comment delimiter — use /* ... */ instead")
           (loop (cdr chars) (cons (make-token 'SLASH-TOK "/") tokens) 'SLASH-TOK))]
      [(char=? (car chars) #\;)
       (loop (cdr chars) (cons (make-token 'SEMI-TOK  ";")  tokens) 'SEMI-TOK)]
      [(char=? (car chars) #\()
       (loop (cdr chars) (cons (make-token 'LPAREN-TOK "(") tokens) 'LPAREN-TOK)]
      [(char=? (car chars) #\))
       (loop (cdr chars) (cons (make-token 'RPAREN-TOK ")") tokens) 'RPAREN-TOK)]

      ;; Lexical errors for known-bad single chars

      ;; Lone colon (user probably meant :=)
      [(char=? (car chars) #\:)
       (error 'lexical-error "Bare ':' is invalid — did you mean ':='?")]

      ;; Lone ! (user probably meant !=)
      [(char=? (car chars) #\!)
       (error 'lexical-error "Bare '!' is invalid — did you mean '!='?")]

      ;; Anything else is unrecognized
      [else
       (error 'lexical-error
              "Unrecognized character: '~a'" (car chars))])))


;;; PART 6: scan — public entry point
;;; ============================================================
(define (scan source-string)
  (tokenize (strip-comments source-string)))
