#lang rosette

(require rosette/lib/synthax)
(require "utils.rkt")

(define-symbolic init (bitvector 32))

(define (register d)
  (lambda (t) (if (= t 0) 0 (d (- t 1)))))

(define (lift op . args)
  (lambda (t) (apply op (map (lambda (stream) (stream t)) args))))

(define (addand0 a b c)
  (lift bvand (register (lift bvadd (register a) (register b))) (register (register c))))

(define (addand1 a b c)
  (register (register (lift bvand (lift bvadd a b) c))))

(define (addand2 a b c)
  (lift bvand (register (register (lift bvadd a b))) (register (register c))))

(define-symbolic chooser boolean?)
(define (addand3 a b c)
  (register (register (lift bvand (lift bvadd (if chooser a #f) b) c))))

(define-symbolic a b c (~> integer? (bitvector 32)))
(define-symbolic t integer?)

(verify (begin
          (assume (> t 1))
          (assert (bveq ((addand0 a b c) t) ((addand3 a b c) t)))))
