#lang racket

(require racket/vector)
(require data/heap)

(struct vert (id adjs index lowlink) #:mutable)
(struct scc (verts size))
;; vertex stack + hash for fast checking
(struct hstack (s h) #:mutable)

(define (pop! hs)
  (let ([s (hstack-s hs)] [h (hstack-h hs)])
    (let ([hd (car s)])
      (hash-remove! h hd)
      (set-hstack-s! hs (cdr s))
      hd)))

(define (push! hs v)
  (let ([s (hstack-s hs)] [h (hstack-h hs)])
    (hash-set! h v #t)
    (set-hstack-s! hs (append (list v) s))))

(define (contains? hs v)
  (let ([h (hstack-h hs)])
    (hash-has-key? h v)))


(define (append-adj-vert! vec vid1 vid2)
  (let* ([src (vector-ref vec vid1)]
	 [adjs (vert-adjs src)])
    (set-vert-adjs! src (append (vert-adjs src) (list vid2)))))

(define (init-vert-vector verts-num)
  (let ([vec (make-vector (+ 1 verts-num))])
    (for ([n (+ 1 verts-num)])
      (vector-set! vec n (vert n '() '() '())))
    (set-vert-index! (vector-ref vec 0) 0)
    vec))

(define (read-verts file-name verts-num)
  (let* ([in (open-input-file file-name)]
	 [verts (init-vert-vector verts-num)])
    (let loop () 
      (let ([v1 (read in)] [v2 (read in)])
	(if (eof-object? v1)
	    (begin
	      (close-input-port in)
	      verts)
	    (begin (append-adj-vert! verts v1 v2)
		   (loop)))))))


(define (find-sccs verts)
  (define index 0)
  (define S (hstack '() (make-hash)))
  (define scc-heap (make-heap 
		    (lambda (s1 s2) (<= (scc-size s1) 
					(scc-size s2)))))
  (define min-scc-size 1)

  (define (scc-heap-add! new-scc)
    (when (or (< (heap-count scc-heap) 5)
	      (and (>= (heap-count scc-heap) 5)
		   (< min-scc-size (scc-size new-scc))))
      (heap-add! scc-heap new-scc)

      (when (> (heap-count scc-heap) 5)
	;; (printf "popping ~a from heap\n" (scc-size (heap-min scc-heap)))
	(heap-remove-min! scc-heap))

      (set! min-scc-size (scc-size (heap-min scc-heap)))
      ;; (printf "added\n")
      ))


  (define (strong-connect v depth)
    (define vid (vert-id v))

    ;; Set the depth index for v to the smallest unused index
    (set-vert-index! v index)
    (set-vert-lowlink! v index)
    (set! index (+ 1 index))
    (push! S vid)

    (for ([wid (in-list (vert-adjs v))])
      (let ([w (vector-ref verts wid)])
	(if (empty? (vert-index w))
	    ;; Successor w has not yet been visited; recurse on it
	    (begin
	      (strong-connect w (+ 1 depth))
	      (set-vert-lowlink! v (min (vert-lowlink v) (vert-lowlink w))))
	    (when (contains? S (vert-id w))
	      ;; Successor w is in stack S and hence in the current SCC
	      (set-vert-lowlink! v (min (vert-lowlink v) (vert-index w)))))))

    (when (eq? (vert-lowlink v) (vert-index v))

      (scc-heap-add! 
    		 (let ([newscc 
    			(let loop (;; [vlist (list vid)]
				   [sz 1])
    			  (let ([wid (pop! S)])
    			    (if (eq? vid wid)
    				(scc '() sz)
    				(loop 
				      ;; (append vlist (list wid)) 
    				      (+ sz 1)))))])
    		   newscc
    		   )))

    )

  (for ([v (in-vector verts)] [num (in-range 700000)])
      ;; (printf "vertex ~a\n" num)
    (when (empty? (vert-index v))

      (strong-connect v 0)))

  scc-heap)

;; correct answer for SCC:
;; scc 0 size: 211
;; scc 1 size: 313
;; scc 2 size: 459
;; scc 3 size: 968
;; scc 4 size: 434821

(define (print-5-sccs sccs)
  (for ([n (in-range 5)]
	#:break (eq? (heap-count sccs) 0))
    (printf "scc ~a size: ~a\n" n (scc-size (heap-min sccs)))
    (heap-remove-min! sccs)))


(define (run)
  (let ([verts (read-verts "/home/dc/code/algs/SCC.txt" 875714)])
  ;; (let ([verts (read-verts "scct1.txt" 50)])  
  ;; (let ([verts (read-verts "scc2.txt" 11)])  
    (printf "vertex array loaded\n")
    (print-5-sccs (find-sccs verts))))

(time (run))
