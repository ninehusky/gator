#!/usr/bin/env racket
#lang errortrace rosette

(require rosette
         rosette/lib/synthax
         json
         (prefix-in gator: "gator-language.rkt")
         "json-to-gator.rkt"
         "gator-interpret.rkt")

(define egraph-json-filepath (make-parameter #f))
(define output-signal-name (make-parameter #f))

(command-line
 #:program "gator"
 #:once-each
 ["--json-filepath" v "Path to the JSON representing the module." (egraph-json-filepath v)]
 ["--output-signal-name" v "Signal to interpret." (output-signal-name v)])

;;; Read from the JSON file.
(define egraph-json (call-with-input-file (egraph-json-filepath) read-json))

(match-define (cons gator-expr out-ids) (json->gator egraph-json))

(define spec-expr
  ;;; Exactly one solution for INIT where this is possible:
  ;;; 0x800...0000, because first bit at index
  ;;; [I5, I4, I3, I2, I1, I0] is 1 and all others are 0.
  (list (gator:op bvmul '() '(1 2 3 4 5 6))
        (gator:var 'I0 1)
        (gator:var 'I1 1)
        (gator:var 'I2 1)
        (gator:var 'I3 1)
        (gator:var 'I4 1)
        (gator:var 'I5 1)))

(define impl-expr gator-expr)


;;; TODO(@ninehusky): can I create a macro for this?
(define-symbolic I5 (~> integer? (bitvector 1)))
(define-symbolic I4 (~> integer? (bitvector 1)))
(define-symbolic I3 (~> integer? (bitvector 1)))
(define-symbolic I2 (~> integer? (bitvector 1)))
(define-symbolic I1 (~> integer? (bitvector 1)))
(define-symbolic I0 (~> integer? (bitvector 1)))

(define-symbolic INIT (bitvector 64))
(define-symbolic t integer?)

(define (curr-env name time)
  (match name
    ['I5 (I5 time)]
    ['I4 (I4 time)]
    ['I3 (I3 time)]
    ['I2 (I2 time)]
    ['I1 (I1 time)]
    ['I0 (I0 time)]
    ['INIT INIT]))

(pretty-print gator-expr)

(interpret spec-expr curr-env 0 0 (list))
(interpret impl-expr curr-env 14 0 (list))

(synthesize #:forall (list I0 I1 I2 I3 I4 I5 t)
            #:guarantee (assert (bveq (interpret impl-expr curr-env 14 t (list))
                                      (interpret spec-expr curr-env 0 t (list)))))
