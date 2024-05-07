#lang racket

(require rosette)

(provide churchroad->rosette-bvop)

;;; Takes Churchroad bvop and returns the Rosette equivalent.
(define churchroad->rosette-bvop
  (make-hash (list (cons "Concat" concat)
                   (cons "ZeroExtend" zero-extend)
                   (cons "Eq" bveq)
                   ;;; TODO(@ninehusky): handle signedness eventually
                   (cons "Shr" bvlshr)
                   (cons "Add" bvadd)
                   (cons "And" bvand)
                   (cons "Or" bvor)
                   (cons "Xor" bvxor)
                   (cons "Extract" extract))))
