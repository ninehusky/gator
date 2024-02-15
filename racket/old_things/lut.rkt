#lang rosette

(require "utils.rkt")

(define (and0 a b)
  (register (lift bvand a b)))

(define-symbolic INIT (bitvector 16))

(define (and1 a b)
  (register (LUT4 a b (lambda (t) (bv 0 1)) (lambda (t) (bv 0 1)) INIT)))

(define-symbolic a b (~> integer? (bitvector 1)))
(define-symbolic t integer?)

(synthesize #:forall (list a b t)
            #:guarantee (begin
                          (assume (> t 0))
                          (assert (not (bveq INIT (bv #x1000 16))))
                          (assert (bveq ((and1 a b) t) ((and0 a b) t)))))

