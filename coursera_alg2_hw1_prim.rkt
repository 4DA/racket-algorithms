#lang racket

(require racket/vector)
(require data/heap)

(struct edge (from to weight)   #:mutable)
(struct node (number adj-edges) #:mutable)

(define (print-edge e)
  (printf "#<edge>: ~a - ~a (~a)\n" (edge-from e) (edge-to e) (edge-weight e)))


(define (make-key edg)
  (set (edge-from edg) (edge-to edg)))



(define (eq-edges? e1 e2)
(or (and (eq? (edge-from e1) (edge-from e2))
	 (eq? (edge-to e1) (edge-to e2)))
    (and (eq? (edge-from e1) (edge-to e2))
	 (eq? (edge-to e1) (edge-from e2)))))


(define (is-bridge? cur-edge vis-nodes mst)
  ;; (printf "is-bridge?: ~a ~a\n" (edge-from cur-edge) (edge-to cur-edge))
    (let* ([n1-num (edge-from cur-edge)]
	   [n2-num (edge-to cur-edge)]
	   [n1-visited? (hash-has-key? vis-nodes n1-num)]
	   [n2-visited? (hash-has-key? vis-nodes n2-num)])

      ;; (printf "n1-visited: ~a n2-visited: ~a\n" n1-visited? n2-visited?)

      (and (not (hash-has-key? mst (make-key cur-edge)))
	   (or (and n1-visited? (not n2-visited?))
	       (and n2-visited? (not n1-visited?))))))

(define (find-next-edge vis-nodes mst)
  (let ([edg-heap 
	 (make-heap (lambda (e1 e2) 
		      (< (edge-weight e1) 
			 (edge-weight e2))))])
    (for* ([(num cur-node) vis-nodes]
	   [cur-edge (node-adj-edges cur-node)])
      (if (is-bridge? cur-edge vis-nodes mst)
	  ;; (begin (printf "bridge: ") (print-edge cur-edge)
	  (heap-add! edg-heap cur-edge)
		 ;; )
	  #f ))
    (if (eq? (heap-count edg-heap) 0) (edge 0 0 0)
	(heap-min edg-heap))))

(define (build-mst edges nodes)
  (let ([vis-nodes (make-hash (list (cons 1 (vector-ref nodes 1))))]
	[mst (make-hash)])
    (for ([iter (- (vector-length nodes) 2)])
      ;; (newline)
      (let* ([new-bridge (find-next-edge vis-nodes mst)]
	     [node-num (if (hash-has-key? vis-nodes (edge-to new-bridge))
			   (edge-from new-bridge)
			   (edge-to new-bridge))]
	     [new-node (vector-ref nodes node-num)])
	;; (printf "-----\nadded: ") (print-edge new-bridge)
	;; (printf "nnode: ~a\n " node-num)
	
	(hash-set! mst (make-key new-bridge) new-bridge)
	(hash-set! vis-nodes node-num new-node)))
    mst))

(define (append-adj-edge! vec node-num edg)
  (let* ([vec-node (vector-ref vec node-num)]
	 [node-edge-list (node-adj-edges vec-node)])
    (set-node-adj-edges! vec-node (append node-edge-list (list edg)))))


(define (read-edge in)
  (let ([from (read in)]
	[to (read in)]
	[weight (read in)])
    (edge from to weight)))

(define (init-node-vector nodes-num)
  (let ([vec (make-vector (+ 1 nodes-num))])
    (for ([n (+ 1 nodes-num)])
      (vector-set! vec n (node n '())))
    vec))

(define (read-edges file-name)
  (let* ([in (open-input-file file-name)]
	 [nodes-num (read in)]
	 [edges-num (read in)]
	 [edges (make-hash)]
	 [nodes (init-node-vector nodes-num)])
    (for ([enum (in-range 1 (+ 1 edges-num))])
      (let* ([cur-edge (read-edge in)]
	     [from (edge-from cur-edge)]
	     [to   (edge-to cur-edge)])
	(hash-set! edges (make-key cur-edge) cur-edge)
	(append-adj-edge! nodes to cur-edge)
	(append-adj-edge! nodes from cur-edge)))
    (values nodes edges)))

(define (count-weight mst)
  (for/fold ([S 0]) ([(k e) mst]) 
    (+ S (edge-weight e))))

(define (run)
  (let-values ([(nodes edges) (read-edges "edges.txt")]) 
    (build-mst edges nodes)))


(define (print-edge-list el)
  (for ([(k e) el])
    (print-edge e)))

;; (count-weight (run))
;; correct answer: {A,B,C,D,E,F,G}

;; 1 2 7     
;; 1 4 5     
;; 2 3 8      
;; 2 4 9     
;; 2 5 7     
;; 3 5 5     
;; 4 5 15    
;; 4 6 6     
;; 5 6 8      
;; 5 7 9      
;; 6 7 11    

; answers:
; 69119377652 - minus
; 67311454237 - division
; -3612829 - prim

