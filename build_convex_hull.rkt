#lang racket
(require racket/gui)  
(require racket/match)

(struct vec2 (x y) #:transparent)

(define acc 0.03)
(define eps acc)

(define (v* v sc)
  (vec2 (* sc (vec2-x v)) (* sc (vec2-y v))))

(define (v+ v1 v2 v3)
  (vec2 
   (+ (vec2-x v1) (vec2-x v2) (vec2-x v3))
   (+ (vec2-y v1) (vec2-y v2) (vec2-y v3))))

(define (draw-vecs v1 v2 v3 bmp-dc)

  (match-let ([(vec2 v1x v1y) v1]
	      [(vec2 v2x v2y) v2]
	      [(vec2 v3x v3y) v3]
	      [(vec2 Ax Ay) A]
	      [(vec2 Bx By) B]
	      [(vec2 Cx Cy) C])

  (send bmp-dc set-pen (make-pen 
		    #:color (send the-color-database find-color "Black")
		    #:width 2))

    (send bmp-dc draw-line 0 0  Ax Ay)
    (send bmp-dc draw-line 0 0  Bx By)
    (send bmp-dc draw-line 0 0  Cx Cy)

    (send bmp-dc set-pen (make-pen 
		    #:color (send the-color-database find-color "Red")
		    #:width 2))

    (send bmp-dc draw-line 0 0 v1x v1y)

    (send bmp-dc set-pen (make-pen 
		    #:color (send the-color-database find-color "Green")
		    #:width 2))

    (send bmp-dc draw-line 0 0 v2x v2y)
    (send bmp-dc draw-line v1x v1y (+ v1x v2x) (+ v1y v2y))
    
    (send bmp-dc set-pen (make-pen
                      #:color (send the-color-database find-color "DarkOrange")
                      #:width 2))

    (send bmp-dc draw-line 0 0 v3x v3y)
    (send bmp-dc draw-line (+ v1x v2x) (+ v1y v2y) (+ v1x v2x v3x) (+ v1y v2y v3y))
))

(define (enumerate-vecs p1 p2 p3 yield-func vec-draw-func render-func) 
  (for* ([l1 (range 0 1.0 acc)]
	 [l2 (range 0 1.0 acc)]
	 [l3 (range 0 1.0 acc)])
    (let ([S (+ l1 l2 l3)])
      (when (<= (abs (- 1 S)) eps)
	(yield-func (v+ (v* p1 l1)
		  (v* p2 l2)
		  (v* p3 l3)))
	(vec-draw-func (v* p1 l1)
		       (v* p2 l2)
		       (v* p3 l3))
        (render-func)))))

(define pts-list '())
(define A (vec2 290 150))
(define B (vec2 90 90))
(define C (vec2 120 300))

(define (make-scene-bitmap w h)
  (new bitmap-dc% [bitmap (make-object bitmap% w h)]))


(define (run)
  (let* ([frame (new frame% [label "3 points convex combination demo"]
		     [width 640]
		     [height 480])]
	 [canvas (new canvas% [parent frame])]
	 [dc (send canvas get-dc)]
         [bmp-dc (make-scene-bitmap 640 480)])

    (send frame show #t)
    (sleep/yield 1)


    (enumerate-vecs A B C
		    (lambda (v)

		      (set! pts-list (append (list v) pts-list))
                      
                      (send bmp-dc set-pen (make-pen 
                                            #:color (send the-color-database find-color "Blue")
                                            #:width 3))
                      (send bmp-dc draw-point (vec2-x v) (vec2-y v))
                      (send dc draw-bitmap (send bmp-dc get-bitmap) 0 0))

		    (lambda (v1 v2 v3)
		      (draw-vecs v1 v2 v3 dc))
                    
                    (lambda () 
                            (sleep 0.02)))
    
    frame))
