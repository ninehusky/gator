;;; This is the main entry point for Gator.

#! /usr/bin/env racket
#lang racket

(define top-module-name (make-parameter #f))
(define verilog-module-filepath (make-parameter #f))
(define inputs (make-parameter '()))

(command-line
    #:program "gator"

)