#lang rosette

(require rosette/lib/synthax)
(require "utils.rkt")

(define-symbolic init (bitvector 32))

(define (register d)
  (lambda (t) (if (= t 0) 0 (d (- t 1)))))

(define (lift op . args)
  (lambda (t) (apply op (map (lambda (stream) (stream t)) args))))

;;; (a + b) & c
(define (addand0 a b c)
  (register (lift bvand (lift bvadd a b) c)))

;;; (a + b) [& or | or ^] c
(define (addand1 a b c)
  (register (lift (choose bvand bvor bvxor) (lift bvadd a b) c)))

(define-symbolic chooser boolean?)
(define (addand3 a b c)
  (register (register (lift bvand (lift bvadd (if chooser a #f) b) c))))

(define-symbolic a b c (~> integer? (bitvector 32)))
(define-symbolic t integer?)

(synthesize #:forall (list a b c t)
            #:guarantee (begin
                          (assume (> t 1))
                          (assert (bveq ((addand0 a b c) t) ((addand1 a b c) t)))))
