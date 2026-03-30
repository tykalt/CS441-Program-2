#lang racket

;; main.rkt — Driver

(require "scanner.rkt"
         "parser.rkt")


;; PARSE ENTRY POINTS
(define (parse-string source)
  (parse-program (scan source)))

(define (parse-file filename)
  (displayln (string-append "--- " filename " ---"))
  (with-handlers ([exn:fail? (lambda (exn)
                               (displayln (string-append "ERROR: "
                                                         (exn-message exn))))])
    (define ast (parse-string (file->string filename)))
    (displayln "AST:")
    (pretty-print ast))
  (newline))


;;; ENTRY POINT
(define args (current-command-line-arguments))

(cond
  [(= (vector-length args) 1)
   (parse-file (vector-ref args 0))]
  [else
   (for-each parse-file
             '("Input1.txt"
               "Input2.txt"
               "Input3.txt"
               "Input4.txt"
               "Input5.txt"
               "Input6.txt"
               "Input7.txt"))])