#!/usr/bin/env racket
#lang racket

(require "json-to-gator.rkt")

(define verilog-module-filepath (make-parameter #f))
(define top-module-name (make-parameter #f))

;;; inputs is an association list mapping input name to an integer bitwidth.
(define inputs (make-parameter '()))

(define verilog-module-out-signal (make-parameter #f))
(define verilog-module-out-bitwidth (make-parameter #f))
(define output-dir (make-parameter #f))

(define egraph-json-filepath (make-parameter #f))

(require json)

(command-line
 #:program "gator"
 #:once-each
 ["--json-filepath" v "Path to the JSON representing the module." (egraph-json-filepath v)])
;;;  ["--verilog-module-filepath" v "Path to the verilog module file." (verilog-module-filepath v)]
;;;  ["--top-module-name" v "Name of the top module." (top-module-name v)]
;;;  ["--output-dir" v "Output directory for the generated json." (output-dir v)]
;;;  ["--verilog-module-out-signal"
;;;   v
;;;   "Name of the output signal and its bitwidth, separated with a colon, e.g. \"out:8\"."
;;;   (let ([splits (string-split v ":")])
;;;     (when (not (equal? 2 (length splits)))
;;;       (error "Output signal must be specified as <name>:<bw>"))
;;;     (verilog-module-out-signal (first splits))
;;;     (verilog-module-out-bitwidth (string->number (second splits))))]
;;;  #:once-any
;;;  #:multi
;;;  [("--input-signal")
;;;   v
;;;   "Name of an input and its bitwidth, separated with a colon, e.g. \"a:8\"."
;;;   ;;; Parse --input arg: split <name>:<bw> into name and bw, construct Rosette symbolic input.
;;;   (let* ([splits (string-split v ":")] [name (first splits)] [bw (string->number (second splits))])
;;;     (when (not (equal? 2 (length splits)))
;;;       (error (format "Invalid input signal specification: ~a" v)))
;;;     (when (assoc name (inputs))
;;;       (error "Signal " name " already present; did you duplicate an --input?"))
;;;     (inputs (append (inputs) (list (cons name bw)))))])

;;; Read from the JSON file.
(define egraph-json (call-with-input-file (egraph-json-filepath) read-json))
(define gator-expr (json->gator egraph-json))
(displayln "done")
