#lang rosette

(require rackunit
         rosette
         (prefix-in gator: "gator-language.rkt")
         "gator-interpret.rkt")

(provide gator-synthesize)

;;; spec-expr is spec
;;; impl-expr is sketch with holes
;;; both of these are written in gator
(define (gator-synthesize spec-expr impl-expr inputs)
  (define-symbolic t integer?)

  ;;; env should be an uninterpreted function that takes a name and a time
  ;;; and returns a symbolic bitvector

  (define soln
    (synthesize
     #:forall (list t)
     #:guarantee
     (begin
       (define (env name time)
         (if (equal? name "A")
             (begin
               (fresh-a time))
             (begin
               (fresh-b time))))
       (assert
        (not (bvzero? (bvsub (interpret (list (gator:op "Add" (list) (list 2 1))
                                              (gator:var "B" 1)
                                              (gator:var "A" 1))
                                        env
                                        0
                                        t
                                        '())
                             x)))
        ))))
  soln)

(gator-synthesize '(1 2 3) '(1 2 3) '(1 2 3))
