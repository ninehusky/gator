#!/usr/bin/env racket
#lang errortrace racket

(require rosette
         "json-to-gator.rkt"
         "gator-interpret.rkt")

(define verilog-module-filepath (make-parameter #f))
(define top-module-name (make-parameter #f))

(define verilog-module-out-signal (make-parameter #f))
(define verilog-module-out-bitwidth (make-parameter #f))
(define output-dir (make-parameter #f))

(define egraph-json-filepath (make-parameter #f))

(define num-test-cases (make-parameter #f))
(define num-clock-cycles (make-parameter #f))
(define output-signal-name (make-parameter #f))

(require json)

(command-line
 #:program "gator"
 #:once-each
 ["--json-filepath" v "Path to the JSON representing the module." (egraph-json-filepath v)]
 ["--num-test-cases" v "Number of test cases to run for." (num-test-cases v)]
 ["--num-clock-cycles" v "Number of clock cycles to run for." (num-clock-cycles v)]
 ["--output-signal-name" v "Signal to interpret." (output-signal-name v)])

;;; Read from the JSON file.
(define egraph-json (call-with-input-file (egraph-json-filepath) read-json))

(match-define (cons gator-expr out-ids) (json->gator egraph-json))

(define (get-inputs)
  (make-hash (list (cons "I5" 1)
                   (cons "I4" 1)
                   (cons "I3" 1)
                   (cons "I2" 1)
                   (cons "I1" 1)
                   (cons "I0" 1)
                   (cons "INIT" 64))))

(pretty-print (car (json->gator egraph-json)))

(for ([test-num (in-range (string->number (num-test-cases)))])
  (define curr-env (make-hash))
  (for ([clock-cycle (in-range (string->number (num-clock-cycles)))])
    ;;; (displayln (format "Test case ~a, clock cycle ~a" test-num clock-cycle))
    (for ([input-name (hash-keys (get-inputs))])
      ;;; get {var-name}:{value} from stdin
      (define line (read-line (current-input-port)))
      (match-let* ([(list var-name value) (string-split line ":")]
                   [old-value (hash-ref curr-env var-name (list))]
                   [bitwidth (hash-ref (get-inputs) var-name)])
        (when (> (length old-value) clock-cycle)
          (error (format "Error: duplicate input for ~a at time ~a." var-name clock-cycle)))
        (when (not (member var-name (hash-keys (get-inputs))))
          (error
           (format "Error: invalid input ~a, valid names are: ~a" var-name (hash-keys (get-inputs)))))
        (hash-set! curr-env
                   var-name
                   (append old-value (list (bv (string->number value) bitwidth)))))))

  (for ([clock-cycle (in-range (string->number (num-clock-cycles)))])
    (let ([output (interpret gator-expr
                             (lambda (name time)
                               (define name-str (symbol->string name))
                               (when (not (member name-str (hash-keys curr-env)))
                                 (error 'interpret (format "Error: variable ~a not found." name-str)))
                               (hash-ref curr-env name-str))
                             14
                             clock-cycle
                             (make-hash))])
      ;;; (value, bitwidth)
      (displayln (format "(~a, ~a)" (bitvector->natural output) (length (bitvector->bits output)))))))
