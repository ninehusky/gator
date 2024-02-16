;;; Some examples of using Rosette to verify hardware designs.

#lang rosette

(require rosette/lib/synthax)
(require rosette/solver/smt/cvc5)
(current-solver (cvc5))

(define (interpret prog env id time cache bitwidth)
  (if (< time 0) (error 'interpret "time must be non-negative") 0)
  (match (list-ref prog id)
    [`(Var ,x)
     (begin
       (when (not (symbol? x))
         (error 'interpret "expected a symbol"))
       (hash-ref env x))]
    [`(Op ,op ,ids ...)
     (apply op
            (for/list ([id ids])
              (interpret prog env id time cache bitwidth)))]
    [`(Reg ,reg-id ,init)
     ;;;  (displayln (format "time is ~a" time))
     (if (= time 0)
         init
         (if (assoc time cache)
             (cdr (assoc time cache))
             (interpret prog env reg-id (- time 1) cache bitwidth)))]
    [node
     (begin
       (when (not (bv? node))
         (error 'interpret "expected a bitvector"))
       node)]))

;;; use rosette to verify that the program always evaluates to 1
;;; (define prog (list `(Reg ,0 ,(bv 1 1))))
(define-symbolic t integer?)

;;; (define cache (make-hash))

;;; (verify (begin
;;;           (hash-set! cache (- t 1) (bv 1 1))
;;;           (assume (>= t 0))
;;;           (assert (bveq (interpret prog '() 0 t cache 1) (bv 1 1)))))

(clear-vc!)

;;; counter[2] at time t is (t + 1)
(define counter (list `(Reg ,2 ,(bv 0 10)) (bv 1 10) `(Op ,bvadd ,0 ,1)))

(define-symbolic result (bitvector 10))
;;; clear the cache
(define new-cache (list (cons t result)))

;;; (output-smt "counter.smt")
(define sol
  (verify (begin
            (assume (bveq result (integer->bitvector (modulo t 1024) (bitvector 10))))
            (assume (> t 0))
            ;;; (assert (bveq (interpret counter '() 2 0 new-cache 10) (bv 1 10)))
            (assert (bveq (interpret counter '() 2 (+ t 1) new-cache 10)
                          (integer->bitvector (+ t 2) (bitvector 10)))))))

(displayln sol)
(evaluate (interpret counter '() 2 (+ t 1) new-cache 10) sol)

;;; (interpret counter '() 2 0 (list) 10)
;;; (interpret counter '() 2 1023 (list) 10)
;;; (interpret counter '() 2 1024 (list) 10)
