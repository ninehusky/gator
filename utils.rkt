#lang rosette

(require rosette/lib/synthax)

(provide register
         lift
         LUT4)

(define (register d)
  (lambda (t) (if (= t 0) 0 (d (- t 1)))))

(define (lift op . args)
  (lambda (t) (apply op (map (lambda (stream) (stream t)) args))))

(define (LUT4 a b c d INIT)
  (lambda (t)
    (bit 0
         ((lift bvlshr
                (lambda (t) INIT)
                (lambda (t) (zero-extend ((lift concat a b c d) t) (bitvector 16))))
          t))))
