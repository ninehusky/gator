#lang rosette

(define (identity t)
  (displayln (format "t is ~a" t))
  (if (zero? t) 0 (+ 1 (identity (- t 1)))))

(define (cached-identity t cache)
  (if (zero? t)
      0
      (if (hash-has-key? cache t) (hash-ref cache t) (+ 1 (cached-identity (- t 1) cache)))))

(define-symbolic t integer?)

;;; this won't terminate
;;; (verify (begin
;;;           (assume (>= t 0))
;;;           (assert (= (identity t) t))))

;;; this will terminate
(define cache (make-hash))

(verify (begin
          (hash-set! cache t t)
          (assume (> t 0))
          (assert (= (cached-identity (+ t 10) cache) (+ t 10)))))
