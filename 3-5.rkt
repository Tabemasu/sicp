#lang racket

;Section 3.5.1

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b)
     (cons a (delay b)))))
;(define-syntax delay
;  (syntax-rules ()
;    ((_ expr)
;     (lambda () expr))))
(define-syntax delay
  (syntax-rules ()
    ((_ expr)
     (memo-proc (lambda () expr)))))
(define (force delayed-expression)
  (delayed-expression))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-null? stream)
  (equal? stream the-empty-stream))
(define the-empty-stream '())
(define (stream-ref s n)
  (if (= n 0) 
      (stream-car s)
      (stream-ref (stream-cdr s) (sub1 n))))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (add1 low) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin (proc (stream-car stream))
             (stream-for-each proc (stream-cdr stream)))))
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (display x)
  (newline))

;Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;Exercise 3.51
(define (show x)
  (display-line x)
  x)

;Exercise 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

;Section 3.5.2
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
;(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
;(define no-sevens (stream-filter (lambda (x) (not (divisible? x 7)))
;                                 integers))
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
;(define fibs (fibgen 0 1))
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x) (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
;(define primes (sieve (integers-starting-from 2)))
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs 
  (cons-stream 0
               (cons-stream 1
               (add-streams (stream-cdr fibs) fibs))))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))
(define primes
  (cons-stream 2
               (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))
(define (square x) (* x x))
(define (print-s stream n)
  (if (or (< n 0) (= n 0)) 
      'done
      (begin (display (stream-car stream))
             (display " ")
             (print-s (stream-cdr stream) (sub1 n)))))
;Exercise 3.53
(define s (cons-stream 1 (add-streams s s)))

;Exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams (integers-starting-from 2)
                                                factorials)))

;Exercise 3.55
(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-sums stream)
                            (stream-cdr stream))))

;Exercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2))
               (s1cdr (stream-cdr s1))
               (s2cdr (stream-cdr s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge s1cdr s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 s2cdr)))
                 (else
                  (cons-stream s1car (merge s1cdr s2cdr))))))))
(define S (cons-stream 1 (merge (merge (scale-integers 2)
                                       (scale-integers 3))
                                (scale-integers 5))))
(define (scale-integers scale-factor)
  (scale-stream integers scale-factor))

;Exercise 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;Exercise 3.59
(define (integrate-series stream)
  (define (form-stream stream n)
    (cons-stream (* (/ 1 n) (stream-car stream))
                 (form-stream (stream-cdr stream) (add1 n))))
  (form-stream stream 1))
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-stream 1 (integrate-series (negate sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
(define (negate stream)
  (cons-stream (- (stream-car stream))
               (negate (stream-cdr stream))))

;Exercise 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

;Section 3.5.3
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (average x y . z)
  (let* ((items (cons x (cons y z)))
         (num-items (length items)))
    (/ (apply + items) num-items)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

;Exercise 3.64
(define (stream-limit stream tolerance)
  (define (good-enough? x y)
    (< (abs (- x y)) tolerance))
  (let ((s0 (stream-car stream))
        (s1 (stream-car (stream-cdr stream))))
    (if (good-enough? s0 s1)
        s1
        (stream-limit (stream-cdr stream) tolerance))))

;Exercise 3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (add1 n)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))

;End exercise

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;Exercise 3.67
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (all-pairs (stream-cdr s) (stream-cdr t))))))

;Exercise 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
     (stream-map (lambda (x) (cons (stream-car s) x))
                 (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
(define pythagorean-triples
  (let ((tri (triples integers integers integers)))
    (stream-filter (lambda (x) (= (+ (square (car x))
                                     (square (cadr x)))
                                  (square (caddr x))))
                   tri)))

;Exercise 3.70
(define (merge-weighted f s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let* ((e1 (stream-car s1))
                     (e2 (stream-car s2))
                     (w1 (f e1))
                     (w2 (f e2)))
                (if (<= w1 w2) 
                    (cons-stream e1 (merge-weighted f (stream-cdr s1) s2))
                    (cons-stream e2 (merge-weighted f s1 (stream-cdr s2))))))))
(define (weighted-pairs f s1 s2)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    f
    (stream-map (lambda (x) (list (stream-car s1) x))
                (stream-cdr s2))
    (weighted-pairs f (stream-cdr s1) (stream-cdr s2)))))
(define (weighta x) (+ (car x) (cadr x)))
(define ex3.70a (weighted-pairs weighta integers integers))
(define (weightb x) (let ((i (car x))
                          (j (cadr x)))
                      (+ (* 2 i) (* 3 j) (* 5 i j))))
(define no235 (stream-filter (lambda (x) (not (or (divisible? x 2)
                                                  (divisible? x 3)
                                                  (divisible? x 5))))
                                     integers))
(define ex3.70b (weighted-pairs weightb no235 no235))

;Exercise 3.71
(define (ramanujan)
  (define (weigh x) (let ((i (car x)) 
                          (j (cadr x)))
                      (+ (* i i i) (* j j j))))
  (define sum-cubes (weighted-pairs weigh integers integers))
  (define (loop s)
    (let ((w1 (weigh (stream-car s)))
          (w2 (weigh (stream-car (stream-cdr s)))))
      (if (= w1 w2)
          (cons-stream w1 (loop (stream-cdr s)))
          (loop (stream-cdr s)))))
  (loop sum-cubes))

;Exercise 3.72
(define (sum-square-three)
  (define (weigh x) (let ((i (car x))
                          (j (cadr x)))
                      (+ (* i i) (* j j))))
  (define sum-square (weighted-pairs weigh integers integers))
  (define (loop s)
    (let ((w1 (weigh (stream-car s)))
          (w2 (weigh (stream-car (stream-cdr s))))
          (w3 (weigh (stream-car (stream-cdr (stream-cdr s))))))
      (if (and (= w1 w2) (= w1 w3))
          (cons-stream w1 (loop (stream-cdr s)))
          (loop (stream-cdr s)))))
  (loop sum-square))

;Section 3.5.4
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;Exercise 3.77
(define (explicit-integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt)))))

;Exercise 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)