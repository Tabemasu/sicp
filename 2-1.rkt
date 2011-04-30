#lang racket

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) 
  (let ([g (gcd n d)])
    (if (or (and (< n 0) (< d 0))
            (and (> n 0) (< d 0)))
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;Exercise 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (make-point c1 c2) (cons c1 c2))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (mid-point s)
  (let ([x1 (x-point (start-segment s))]
        [x2 (x-point (end-segment s))]
        [y1 (y-point (start-segment s))]
        [y2 (y-point (end-segment s))])
    (make-point (average x1 x2) (average y1 y2))))
(define (average x y)
  (/ (+ x y) 2))

;Exercise 2.3
;(define (make-rect width height) (cons width height))
;(define (get-width r) (car r))
;(define (get-length r) (cdr r))
(define (area-of-rect r)
  (* (get-width r) (get-length r)))
(define (perimeter-of-rect r)
  (+ (* 2 (get-width r)) (* 2 (get-length r))))
(define (make-rect p1 p2) 
  (if (or (= (x-point p1) (x-point p2))
          (= (y-point p1) (y-point p2)))
      (error "~s and ~s do not define a rectangle" p1 p2)
      (cons p1 p2)))
(define (get-width r) 
  (abs (- (x-point (car r)) (x-point (cdr r)))))
(define (get-length r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))

;Exercise 2.4
(define (cons2 x y)
  (lambda (m) (m x y)))
(define (car2 z)
  (z (lambda (p q) p)))
(define (cdr2 z)
  (z (lambda (p q) q)))

;Exercise 2.5
(define (count-divisions n divisor)
  (lambda (try) (if (= (remainder n (expt divisor try)) 0)
                    ((count-divisions n divisor) (+ try 1))
                    (- try 1))))
(define (cons3 a b)
  (* (expt 2 a) (expt 3 b)))
(define (car3 c)
  ((count-divisions c 2) 1))
(define (cdr3 c)
  ((count-divisions c 3) 1))

;Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (addition a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;Extended Exercise
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval-old x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (error "Division error, interval spans 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
;Exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))

;Exercise 2.8
(define (sub-interval a b)
  (add-interval a (make-interval (- (upper-bound b))
                                 (- (lower-bound b)))))
;Exercise 2.9
;width i = (ub-lb)/2
;i = (lb . ub)

;width i1 + width i2 = ([ub1-lb1] + (ub2-lb2)]/2
;                      (ub1-lb1+ub2-lb2)/2
;                      ([ub1+ub2]-[lb1+lb2])/2

;Exercise 2.11
(define (mul-interval i1 i2)
  (define (pp-or-nn i1 i2)
    (make-interval (* (lower-bound i1)
                      (lower-bound i2))
                   (* (upper-bound i1)
                      (upper-bound i2))))
  (define (pos-neg i1 i2)
    (make-interval (* (upper-bound i1)
                      (lower-bound i2))
                   (* (lower-bound i1)
                      (upper-bound i2))))
  (define (pos-span0 i1 i2)
    (make-interval (* (upper-bound i1)
                      (lower-bound i2))
                   (* (upper-bound i1)
                      (upper-bound i2))))
  (define (neg-span0 i1 i2)
    (make-interval (* (lower-bound i1)
                      (upper-bound i2))
                   (* (lower-bound i1)
                      (lower-bound i2))))
  (define (span0x2 i1 i2)
    (let ([p1 (* (lower-bound i1) (lower-bound i2))]
          [p2 (* (lower-bound i1) (upper-bound i2))]
          [p3 (* (upper-bound i1) (lower-bound i2))]
          [p4 (* (upper-bound i1) (upper-bound i2))])
      (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
  (define (reverse-args f)
    (lambda (i1 i2) (f i2 i1)))
  (define (test-second-interval a b pos neg els)
    (cond [(positive-interval? i2) (pos a b)]
          [(negative-interval? i2) (neg a b)]
          [else (els a b)]))
  (cond [(positive-interval? i1)
         (test-second-interval i1 i2 pp-or-nn pos-neg pos-span0)]
        [(negative-interval? i1)
         (test-second-interval i2 i1 pos-neg pp-or-nn (reverse-args neg-span0))]
        [else (test-second-interval i2 i1 pos-span0 neg-span0 span0x2)]))
(define (positive-interval? i)
  (and (< 0 (lower-bound i)) (< 0 (upper-bound i))))
(define (negative-interval? i)
  (and (> 0 (lower-bound i)) (> 0 (upper-bound i))))
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100.0))) (+ c (* c (/ p 100.0)))))
(define (percent i)
  (* (/ (width i) (center i)) 100))
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))