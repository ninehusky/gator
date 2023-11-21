#lang rosette

(require rosette/lib/synthax)

(define-symbolic init (bitvector 32))

(define (register d)
  (lambda (t) (if (= t 0) init (d (- t 1)))))

(define (lift op . args)
  (lambda (t) (apply op (map (lambda (stream) (stream t)) args))))

(define (LUT4 a b c d)
  (define-symbolic* INIT (bitvector 16))
  (lambda (t)
    (bit 0
         ((lift bvlshr
                (lambda (t) INIT)
                (lambda (t) (zero-extend ((lift concat a b c d) t) (bitvector 16))))
          t))))

(define (and0 a b)
  (register (lift bvand a b)))

(define (and1 a b)
  (register (LUT4 a b (lambda (t) (bv 0 1)) (lambda (t) (bv 0 1)))))

(define-symbolic a b (~> integer? (bitvector 1)))
(define-symbolic t integer?)

(synthesize #:forall (list a b t)
            #:guarantee (begin
                          (assume (> t 0))
                          (assert (bveq ((and1 a b) t) ((and0 a b) t)))))
