#lang racket

(provide prime? square fast-fib)

(define (factorial-rec n)
  (if (<= n 1)
      n
      (* n (factorial-rec (sub1 n)))))

(define (factorial n)
  (define (iter counter product)
    (if (> counter n)
        product
        (iter n (add1 counter) (* product counter))))
  (iter n 1 1))

;Exercise 1.9
(define (my+1 a b)
  (if (= a 0)
      b
      (add1 (my+1 (sub1 a) b))))

(define (my+2 a b)
  (if (= a 0)
      b
      (my+2 (sub1 a) (add1 b))))

;(my+1 4 5)
;(add1 (my+1 3 5))
;(add1 (add1 (my+1 2 5)))
;(add1 (add1 (add1 (my+1 1 5))))
;(add1 (add1 (add1 (add1 (my+1 0 5)))))
;(add1 (add1 (add1 (add1 5))))
;(add1 (add1 (add1 6)))
;(add1 (add1 7))
;(add1 8)
;9

;(my+2 4 5)
;(my+2 3 6)
;(my+2 2 7)
;(my+2 1 8)
;(my+2 0 9)
;9

;Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;(A 1 10)
;returns 1024
;(A 2 4)
;returns 65536
;(A 3 3)
;returns 65536

(define (f n) (A 0 n))
;computes 2n
(define (g n) (A 1 n))
;computes n^2 for n > 0
(define (h n) (A 2 n))
;computes 2^h(n-1) for n > 0, h(1) = 2
(define (k n) (* 5 n n))
;computes 5n^2

(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond [(= amount 0) 1]
          [(or (< amount 0) (= kinds-of-coins 0)) 0]
          [else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount (first-denomination kinds-of-coins)) 
                       kinds-of-coins))]))
  (define (first-denomination kinds-of-coins)
    (cond [(= kinds-of-coins 1) 1]
          [(= kinds-of-coins 2) 5]
          [(= kinds-of-coins 3) 10]
          [(= kinds-of-coins 4) 25]
          [(= kinds-of-coins 5) 50]))
  (cc amount 5))

;Exercise 1.11
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) 
         (* 2 (f-rec (- n 2))) 
         (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (iter count a b c)
    (if (< n count)
        a
        (iter (+ count 1)
              (+ a (* 2 b) (* 3 c))
              a
              b)))
  (if (< n 3)
      n
      (iter 3 2 1 0)))

;Exercise 1.12
(define (pascal row col)
  (cond [(= row 1) 1]
        [(= col 1) 1]
        [else (+ (pascal (- row 1) col)
                 (pascal row (- col 1)))]))

;Exercise 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (my-sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (my-sine (/ angle 3.0)))))

; a. 5 times.
; b. time = O(log3 a)
;    space = O(log3 a)

(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt-iter b n)
  (define (iter counter product)
    (if (= counter 0)
        product
        (iter (sub1 counter)
                   (* b product))))
  (iter n 1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square x) (* x x))

;Exercise 1.16
(define (fast-expt-iter b n)
  (define (iter a b n)
    (cond [(= n 0) a]
          [(even? n) (iter a (square b) (/ n 2))]
          [else (iter (* a b) b (- n 1))]))
  (iter 1 b n))

;Exercise 1.17
(define (my* a b)
  (if (= b 0)
      0
      (+ a (my* a (sub1 b)))))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (my*-fast-rec a b)
  (cond [(= b 0) 0]
        [(even? b) (double (my*-fast-rec a (halve b)))]
        [else (+ a 
                 (my*-fast-rec a (sub1 b)))]))

(define (my*-fast-iter a b)
  (define (iter n a b)
    (cond [(= b 0) n]
          [(even? b) (iter n (double a) (halve b))]
          [else (iter (+ n a) a (sub1 b))]))
  (iter 0 a b))

;Exercise 1.19
(define (fast-fib n)
  (define (iter a b p q count)
    (cond [(= count 0) b]
          [(even? count)
           (iter a
                 b
                 (+ (square p) (square q))
                 (+ (square q) (* 2 p q))
                 (/ count 2))]
          [else (iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1))]))
  (iter 1 0 0 1 n))

(define (my-gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (next test-divisor))]))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ 2 test-divisor)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (current-inexact-milliseconds) start-time))
      (display " Not prime")))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start stop)
  (define (helper start stop count)
    (cond [(or (<= stop start) (= count 3)) void]
          [(prime? start) 
           (timed-prime-test start)
           (helper (+ 2 start) stop (+ count 1))]
          [else (helper (+ 2 start) stop count)]))
  (if (even? start)
      (helper (add1 start) stop 0)
      (helper start stop 0)))

(define c (sqrt 10))

; 1009  0.008056640625
; 1013  0.008056640625
; 1019  0.008056640625
; 10007 0.025878906250
; 10009 0.020996093750
; 10037 0.021972656250
; 0.008056640625 * sqrt(10) = 0.025477334664442512
