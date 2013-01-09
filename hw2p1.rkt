#lang racket

;; Max-spacing k-clustering: maximize summ of distances between separated points
;;  Each point in separate cluster
;;  Repeat until k clusters:
;;     - let (p, q) = closest pair of separated points
;;       (determines the current spacing)
;;     - merge the clusters containing (p, q) into single cluster

;; answer: 106



(require racket/vector)
(require racket/pretty)
(require data/heap)

(define file-name "clustering1.txt")
;; (define file-name "clustering-tst1.txt")
(define k-clusters 4)

(struct edge (dist nodes) #:transparent)
(struct cluster (id nodes) #:transparent)
(struct node (id clid)	#:transparent)

(define (edge-shorter? e1 e2) 
  (< (edge-dist e1) 
     (edge-dist e2)))

(define (read-pt)
  (let* ([in (open-input-file file-name)]
	 [N (read in)]
	 [edg-num (/ (* N (- N 1)) 2)]
	 [edge-hash (make-immutable-hash (for/list ([x edg-num])
			    (let* ([p1 (read in)]
				   [p2 (read in)]
				   [w (read in)]
				   [sp (set p1 p2)])
			      (cons sp (edge w sp)))))]
	 [min-el (sort (hash-values edge-hash) edge-shorter?)])
    (values min-el edge-hash N)))    


(define (read-points)
  (let* ([in (open-input-file file-name)]
	 [N (read in)]
	 [edges (make-heap edge-shorter?)]
	 [edge-hash (make-hash)]
	 [edg-num (/ (* N (- N 1)) 2)])
    (for ([x edg-num])
      (let* ([p1 (read in)]
	     [p2 (read in)]
	     [w (read in)]
	     [sp (set p1 p2)])
	(heap-add! edges (edge w sp))
	(hash-set! edge-hash sp (edge w sp))))
    (values edges edge-hash N)))

(define (init-nodes node-num)
  (for/fold ([nodes (hash)]) ([x (in-range 1 (+ node-num 1))])
    (hash-set nodes x x)))

; each nodes is in it's own cluster
(define (init-clusters node-num)
  (for/fold ([clusters (hash)]) ([x (in-range 1 (+ node-num 1))])
    (hash-set clusters x (cluster x (set x)))))

;; binary heap custom stream
(define-struct hstream (v)
  #:methods gen:stream
  [(define (stream-empty? st)
     (eq? (heap-count (hstream-v st)) 0))
   (define (stream-first st)
     (heap-min (hstream-v st)))
   (define (stream-rest st)
     (begin (heap-remove-min! (hstream-v st))
   	    (hstream (hstream-v st))))])

;; return union of two clusters and update corresponding nodes
(define (union-clusters cls1 cls2 nodes clusters)
  (let* ([set1 (cluster-nodes cls1)] [set2 (cluster-nodes cls2)]
	 [cl1 (cluster-id cls1)]     [cl2 (cluster-id cls2)]
	 [new-set (set-union set1 set2)]
	 [new-cluster (cluster (cluster-id cls1) new-set)]
	 [new-nodes (for/fold ([new-n nodes]) ([n new-set])
		      (hash-set new-n n (cluster-id new-cluster)))]
	 [clm2 (hash-remove clusters cl2)]
	 [new-clusters (hash-set clm2 cl1 new-cluster)])
    (values new-nodes new-clusters (hash-count new-clusters))))

;; cluster union unit tests
;; (define (test-cl-union)
;;   (define c1 (cluster 1 (set 1 3 5 8)))
;;   (define c2 (cluster 2 (set 2 4 6)))
;;   (union-clusters c1 c2
;; 		  (make-immutable-hash 
;; 		   (list (cons 1 1) (cons 3 1) 
;; 			 (cons 5 1) (cons 8 1) 
;; 			 (cons 2 2) (cons 4 2) 
;; 			 (cons 6 2)))
;; 		  (hash 1 c1 2 c2)))

;; (define (cl-tst1)
;;   (cluster-nodes (hash-ref 
;; 		  (let-values ([(a b) (test-cl-union)]) b) 
;; 		  1)))

(define (get-n1 edg)
  (set-first (edge-nodes edg)))

(define (get-n2 edg)
  (set-first (set-rest (edge-nodes edg))))


(define (k-clusterize edge-list node-num)
  (let* ([node-hash (init-nodes node-num)])
    (for/fold ([nodes node-hash] 
	       [clusters (init-clusters node-num)]
	       [cl-num node-num]) 
	([x edge-list])
      #:break (eq? cl-num k-clusters)
      (let* ([n1 (get-n1 x)] [n2 (get-n2 x)]
      	     [cl1  (hash-ref nodes n1)] 
	     [cl2  (hash-ref nodes n2)])

	(if (not (eq? cl1 cl2))
	    (begin 
	      (union-clusters (hash-ref clusters cl1) 
			      (hash-ref clusters cl2) nodes clusters))
	      (values nodes clusters cl-num))))))

(define (count-cl-dist c1 c2 vis edges min-dist)
  (for*/fold ([S min-dist] [new-vis vis]) 
      ([x (cluster-nodes c1)] [y (cluster-nodes c2)])
    (if (hash-has-key? new-vis (set x y))
	(values S new-vis)
	(values (let ([cd (edge-dist (hash-ref edges (set x y)))])
		  (if (< cd S) cd S)) 
		(hash-set new-vis (set x y) #t)))))

(define (count-distance edges clusters)
  (for*/fold ([min-dist 9999999] 
	      [new-vis (hash)]) 
      ([(k c1) clusters] [(k c2) clusters])

    (if (eq? c1 c2) (values min-dist new-vis)
    	(count-cl-dist c1 c2 new-vis edges min-dist))))

(define (run)
  (let*-values ([(edge-list edge-hash node-num) (read-pt)]
		[(nodes clusters cl-num) (k-clusterize edge-list node-num)]
		[(sum nv) (count-distance edge-hash clusters)]
		)
    sum))

