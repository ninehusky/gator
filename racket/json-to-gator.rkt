#lang racket

(require json
         rosette
         (prefix-in gator: "gator-language.rkt"))

(provide json->gator)

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

;;; Returns a map from eclass->type, if there is one.
(define (type-inference egraph-json)
  (define nodes (dict-ref egraph-json 'nodes))
  ;;; Map from bv id to its bitwidth
  (define bv->bitwidth
    (for/hash ([(name node) (in-hash nodes)] #:when (equal? (dict-ref node 'op) "Bitvector"))
      (when (not (equal? (length (dict-ref node 'children)) 1))
        (error (format "there should be exactly 1 child in a bv node, there is ~a")))
      (let* ([children (dict-ref node 'children)]
             [bw-node (dict-ref nodes (string->symbol (first children)))]
             [bitwidth (string->number (dict-ref bw-node 'op))])
        (when (not (equal? (length (dict-ref bw-node 'children)) 0))
          (error
           (format
            "HasType of bv should be pointing to an int eclass, which has no children, but found ~a"
            (dict-ref bw-node 'children))))
        (values name bitwidth))))
  ;;; Map from id-node
  (for/hash ([(name type-node) (in-hash nodes)] #:when (equal? (dict-ref type-node 'op) "HasType"))
    (match-let ([(list node-name type) (dict-ref type-node 'children)])
      (values node-name (dict-ref bv->bitwidth (string->symbol type))))))

(define (json->gator egraph-json)
  (when (not (jsexpr? egraph-json))
    (error 'json->gator "expected a JSON object, got ~s" egraph-json))

  (define eclass-name->bitwidth (type-inference egraph-json))

  (define class-data (dict-ref egraph-json 'class_data))
  (define nodes (dict-ref egraph-json 'nodes))

  ;;; This returns true when we do NOT want an eclass to correlate to a Gator expression.
  (define (prune-eclass? eclass-name eclass-type)
    (cond
      [(equal? eclass-type "PortDirection") #t]
      [(equal? eclass-type "Op")
       (let ([nodes (for/list ([(name node) (in-hash (dict-ref egraph-json 'nodes))]
                               #:when (equal? (dict-ref node 'eclass) (symbol->string eclass-name)))
                      node)])
         (begin
           (when (not (equal? (length nodes) 1))
             (error 'json->gator
                    "expected exactly one node with eclass ~s, got ~s"
                    eclass-name
                    nodes))
           (define node (first nodes))
           (member (dict-ref node 'op) (hash-keys churchroad->rosette-bvop))))]
      [else
       (or (equal? eclass-name (string->symbol "Unit-0"))
           (member eclass-type
                   (append (list "Type" "Bitvector" "String" "i64")
                           (hash-keys churchroad->rosette-bvop))))]))

  (define pruned-class-data
    (for/hash ([(eclass type) (in-hash (dict-ref egraph-json 'class_data))]
               #:when (not (prune-eclass? eclass (dict-ref type 'type))))
      (values eclass type)))

  (define pruned-nodes
    (for/hash ([(name node) (in-hash (dict-ref egraph-json 'nodes))]
               #:when (member (string->symbol (dict-ref node 'eclass)) (hash-keys pruned-class-data)))
      (values name node)))

  ;;; eclass->id maps the keys in json['class_data'] to natural numbers
  (define eclass->gator-id
    (let ([eclass->id (make-hash)])
      (begin
        (for ([(key value) (in-hash pruned-class-data)])
          (hash-set! eclass->id key (hash-count eclass->id)))
        eclass->id)))

  (define gator-id->eclass
    (for/hash ([(eclass id) (in-hash eclass->gator-id)])
      (values id eclass)))

  ;;; node-name->id maps
  (define node-name->gator-id
    ;;; egraph-json['nodes'][name]['eclass']
    (for/hash ([(name node) (in-hash pruned-nodes)])
      (define eclass (dict-ref node 'eclass))
      (define gator-id (dict-ref eclass->gator-id (string->symbol eclass)))
      (values (symbol->string name) gator-id)))

  (define (gen-gator-op node)
    (define children (dict-ref node 'children))
    (define op-name (car children))
    (define operands (map (lambda (child) (dict-ref node-name->gator-id child)) (cdr children)))
    (define op-node (dict-ref nodes (string->symbol op-name)))
    ;;; Now, let's get the children for the function call
    (define op-children-values
      (map (lambda (name) (dict-ref (dict-ref nodes (string->symbol name)) 'op))
           (dict-ref op-node 'children)))
    (gator:op (dict-ref churchroad->rosette-bvop (dict-ref op-node 'op)) op-children-values operands))

  (define (gen-gator-stmt eclass)
    ;;; find the nodes that have the given eclass
    (define eclass-nodes
      (for/list ([(_ node) (in-hash pruned-nodes)]
                 #:when (equal? (dict-ref node 'eclass) (symbol->string eclass)))
        node))
    (when (not (equal? (length eclass-nodes) 1))
      (error 'json->gator "expected exactly one node with eclass ~s, got ~s" eclass eclass-nodes))
    (define node (first eclass-nodes))
    (define children (dict-ref node 'children))
    (match (dict-ref node 'op)
      ["Var"
       (match-let ([(list name bw) (map (lambda (val)
                                          (dict-ref (dict-ref nodes (string->symbol val)) 'op))
                                        children)])
         (gator:var name bw))]
      [(or "Op1" "Op2") (gen-gator-op node)]
      [else (dict-ref node 'op)]))

  ;;; for i = 0 to len(eclass->gator-id) - 1
  (for/list ([(eclass _) (in-hash pruned-class-data)])
    (displayln (gen-gator-stmt eclass))))
