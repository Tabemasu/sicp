#lang racket

;Section 3.5.1

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b)
     (cons a (delay b)))))
(define-syntax delay
  (syntax-rules ()
    ((_ expr)
     (lambda () expr))))
;(define-syntax delay
;  (syntax-rules ()
;    ((_ expr)
;     (memo-proc (lambda () expr)))))
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