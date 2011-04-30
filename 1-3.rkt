#lang racket

(require "1-2.rkt")

(define (sum-rec term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-rec term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a add1 b))

(define (cube x) (* x x x))

(define (sum-int a b)
  (sum identity a add1 b))

(define (identity x) x)

(define (pi-sum a b)
  (define (pi-term a)
    (/ 1.0 (* a (+ 2 a))))
  (define (pi-next a)
    (+ a 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;Exercise 1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (cond [(or (= k 0) (= k n)) (y k)]
          [(even? k) (* 2 (y k))]
          [else (* 4 (y k))]))
  (define (helper k)
    (sum term k add1 n))
  (* (/ h 3) (helper 0)))

;Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;Exercise 1.31
(define (product-rec term a next b)
  (if (< b a)
      1
      (* (term a)
         (product-rec term (next a) next b))))

(define (factorial n)
  (product identity 1 add1 n))

(define (pi-product n)
  (define (pi-term a)
    (if (even? a)
        (/ (+ 2 a) (+ 1 a))
        (/ (+ 1 a) (+ 2 a))))
  (product pi-term 1.0 add1 n))

(define (product-iter term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;Exercise 1.32

(define (accumulate-rec combiner null-value term a next b)
  (if (< b a)
      null-value
      (combiner (term a)
                (accumulate combiner 
                            null-value 
                            term 
                            (next a) 
                            next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (< b a)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;Exercise 1.33
(define (filt-acc filter combiner null-value term a next b)
  (define (iter a result)
    (cond [(< b a) result]
          [(filter a) 
           (iter (next a) (combiner (term a) result))]
          [else (iter (next a) result)]))
  (iter a null-value))

(define (sum-of-sq-primes a b)
  (filt-acc prime? + 0 square a add1 b))

(define (product-rel-primes n)
  (define (filter a)
    (= (gcd a n) 1))
  (filt-acc filter * 1 identity 1 add1 n))

;Exercise 1.34
(define (f g)
  (g 2))

;It will provide an error saying that it expects the argument to f (g in the body of f) to be a procedure.

(define (search f neg-point pos-point)
  (let ([midpoint (average neg-point pos-point)])
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ([test-value (f midpoint)])
          (cond [(positive? test-value)
                 (search f neg-point midpoint)]
                [(negative? test-value)
                 (search f midpoint pos-point)]
                (else midpoint))))))

(define (average x y)
  (/ (+ x y) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond [(and (negative? a-value) (positive? b-value))
           (search f a b)]
          [(and (negative? b-value) (positive? a-value))
           (search f b a)]
          [else (error "Values are not of opposite sign" a b)])))

(define (fixed-point-old f first-guess)
  (let ([tolerance 0.00001])
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (let ([next (f guess)])
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (cont-frac-rec n d k)
  (define (helper i)
    (if (> i k) 
        0
        (/ (n i) (+ (d i) (helper (add1 i))))))
  (helper 1))

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (square x))))
             (lambda (i) (- (* 2 i) 1))
             k))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (my-sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newtons-method g guess)
  (fixed-point (lambda (x) (- x (/ (g x) ((deriv g) x))))
               guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;Exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

;(((double (double double)) inc) 5)
;(((double (lambda (x) (double (double x)))) inc) 5)
;Let (lambda (x) (double (double x)))) = f
;(((double f) inc) 5)
;(((lambda (x) (f (f x))) inc) 5)
;((f (f inc)) 5)
;((f (double (double inc))) 5)
;((f (double (lambda (x) (inc (inc x))))) 5)
;Let (lambda (x) (inc (inc x))) = g
;((f (double g)) 5)
;((f (lambda (x) (g (g x)))) 5)
;Let (lambda (x) (g (g x))) = h
;((f h) 5)
;((double (double h)) 5)
;((double (lambda (x) (h (h x)))) 5)
;Let (lambda (x) (h (h x))) = i
;((double i) 5)
;((lambda (x) (i (i x))) 5)
;(i (i 5))
;(i (h (h 5)))
;(i (h (g (g 5))))
;(i (h (g (inc (inc 5)))))
;(i (h (g 7)))
;(i (h (inc (inc 7))))
;(i (h 9))
;(i (g (g 9)))
;(i (g (inc (inc 9))))
;(i (g 11))
;(i (inc (inc 11)))
;(i 13)
;(h (h 13))
;(h (g (g 13)))
;(h (g (inc (inc 13))))
;(h (g 15))
;(h (inc (inc 15)))
;(h 17)
;(g (g 17))
;(g (inc (inc 17)))
;(g 19)
;(inc (inc 19))
;21

;Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;Exercise 1.43
(define (repeated-rec f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define (repeated f n)
  (define (iter n result)
    (if (< n 1)
        result
        (iter (- n 1) (compose f result))))
  (iter n (lambda (x) x)))

;Exericise 1.44
(define (smooth f)
  (let ([dx 0.000001])
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx)))))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;Exercise 1.45

(define (nth-root n x)
  (let ([av (floor (log2 n))])
    (fixed-point ((repeated average-damp av)
                  (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))

(define (log2 x) (/ (log x) (log 2)))

;Exercise 1.46
(define (iterative-improve improve good-guess?)
  (lambda (x)
    (let ([next (improve x)])
      (if (good-guess? x next)
          next
          ((iterative-improve improve good-guess?) next)))))

(define (close-enough? v1 v2)
  (let ([tolerance 0.00001])
    (< (abs (- v1 v2)) tolerance)))
  
(define (fixed-point f guess)
  ((iterative-improve f close-enough?) guess))

(define (sqrt n)
  ((iterative-improve (average-damp (lambda (x) (/ n x)))
                      close-enough?)
   1.0))