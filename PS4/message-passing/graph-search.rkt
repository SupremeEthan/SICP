#lang racket
(require "message-passing-defns.rkt")
(require "message-passing.rkt")


(provide bfs dfs)

;;This takes one of the nodes created by build-graph and does a
;;graph search.
;;Returns a list of nodes in the order we mark them.
(define (graph-search start-node container-constructor)
  (let ([container (container-constructor)])
    (send-message container 'enqueue! start-node)
    (let loop ()
      (if (send-message container 'empty?)
          '()
          (let ([curr (send-message container 'dequeue!)])
            (if (send-message curr 'marked?)
                (loop)
                (begin
                  (send-message curr 'mark!)
                  (map (lambda (c) (send-message container 'enqueue! c))
                       (send-message curr 'get-children))
                  (cons curr (loop)))))))))

;;Takes a directed graph in an adjacency list format.
;;Node-entries is a list of lists, and each node entry is a list of symbols.
;;The first symbol is the name of a node, and the rest are names of its
;;children.
;;Each node (or rather, the name of each node) appears at the head of exactly
;;one list.
(define (build-graph node-entries node-constructor)
  ;;For each list, we extract the node name with car, and
  ;;create a node object with that name.
  ;;the result is a list of (name . object) pairs.
  (let ([node-objects
         (map (lambda (entry)
                (let ([name (car entry)])
                  (cons name (node-constructor name))))
              node-entries)])

    ;;Given a name, finds the node we just built with that name.
    (define (lookup-node-object node-name)
      ;;assq is a built-in procedure for looking through lists of
      ;;(key . value) pairs. It returns the full pair, so
      ;;we must apply cdr to its result to get the value.
      (cdr (assq node-name node-objects)))

    ;;We need to set the children property of each node we've just created.
    ;;To do that we need to convert each node-entry from names to objects.
    ;;The car will be the node, the cdr will be the children.
    (define (populate-children node-entry)
      (let ([parent (lookup-node-object (car node-entry))]
            [children (map lookup-node-object (cdr node-entry))])
        (send-message parent 'set-children! children)))
    ;;Actually populate the children field of every node.
    (map populate-children node-entries)
    
    ;;node-objects is a list of (name, node) pairs. Return the actual nodes.
    (map cdr node-objects)))

(define (search-wrapper
         adjacency-list node-constructor queue-constructor)
  (define nodes (build-graph adjacency-list node-constructor))
  (graph-search (car nodes) queue-constructor))

(define (bfs g)
  (define out  (search-wrapper g make-node make-fifo))
  (map (lambda (n) (send-message n 'get-name)) out))

(define (dfs g)
  (define out (search-wrapper g make-dfs-node make-lifo))
  (map (lambda (n) (send-message n 'get-name)) out))

