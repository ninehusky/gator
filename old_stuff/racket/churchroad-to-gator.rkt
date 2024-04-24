#lang rosette

;;; Returns a list of the gator versions of the nodes. Yeah
;;; nodes : list of nodes
(define (generate-gator-expr nodes)
  (define (generate-op op)
    (match op
      [`Extract extract]
      [`And bvand]))
  (define (generate-node node)
    (match node
      [`(Op1 (,op ... ,args) ,expr)
       `(,(generate-op op) ,@(map generate-node args) (generate-node expr))]
      [`(Op2 ,op ,expr1 ,expr2) `(,(generate-op op) ,(generate-node expr1) ,(generate-node expr2))]
      [token token]))

  (map generate-node nodes))

(module+ test
  (require rackunit)

  (define-syntax-rule (test-gator-expr #:name name #:nodes nodes #:expected expected)
    (test-case (format "generate-gator-expr ~s" name)
      (check-equal? (generate-gator-expr nodes) expected)))

  (test-gator-expr #:name "constants"
                   #:nodes (list `(Var "x" 1) `(Var "y" 1))
                   #:expected (list `(Var "x" 1) `(Var "y" 1))))
