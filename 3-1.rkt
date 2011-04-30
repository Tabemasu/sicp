#lang racket
(define withdraw
  (let ([balance 100])
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "insufficent funds"))))
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funs")))
;(define (make-account balance)
;  (define (withdraw amount)
;    (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;               balance)
;        "Insufficient funds"))
;  (define (deposit amount)
;    (set! balance (+ balance amount))
;    balance)
;  (define (dispatch m)
;    (cond [(eq? m 'withdraw) withdraw]
;          [(eq? m 'deposit) deposit]
;          [else (error "Unknown request -- MAKE-ACCOUNT"
;                       m)]))
;  dispatch)

;Exercise 3.1
(define (make-accumulator initial-value)
  (lambda (n) (begin (set! initial-value (+ initial-value n))
                     initial-value)))

;Exercise 3.2
(define (make-monitored f)
  (let ([counter 0])
    (define (dispatch x)
      (cond [(eq? x 'how-many-calls?) counter]
            [(eq? x 'reset-count) (set! counter 0)
                                  "Count reset"]
            [else (set! counter (+ counter 1))
                  (f x)]))
    dispatch))

;Exercise 3.3 & 3.4
;(define (make-account balance password)
;  (let ([counter 0])
;    (define (withdraw amount)
;      (if (>= balance amount)
;          (begin (set! balance (- balance amount))
;                 balance)
;          "Insufficient funds"))
;    (define (deposit amount)
;      (set! balance (+ balance amount))
;      balance)
;    (define (reset-counter) (set! counter 0))
;    (define (dispatch p m)
;      (if (eq? p password)
;          (begin (reset-counter)
;                 (cond [(eq? m 'withdraw) withdraw]
;                       [(eq? m 'deposit) deposit]
;                       [else (error "Unknown request -- MAKE-ACCOUNT"
;                                    m)]))
;          (lambda (m) (begin (set! counter (+ counter 1))
;                             (if (>= counter 7)
;                                 (call-the-cops)
;                                 "Incorrect password")))))
;    dispatch))
(define (call-the-cops) "Calling cops!")

;Exercise 3.5
(define (estimate-integral p x1 x2 y1 y2 trials)
  (let ([fraction (monte-carlo trials (area-test p x1 x2 y1 y2))]
        [area-of-rec (abs (* (- x2 x1) (- y2 y1)))])
    (* fraction area-of-rec)))
(define (area-test p x1 x2 y1 y2)
  (lambda () (p (random-in-range x1 x2) (random-in-range y1 y2))))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (* (random) range))))
(define (unit-circle-predicate x y)
  (<= (+ (square x) (square y)) 1))
(define (square x) (* x x))

;Exercise 3.7
(define (make-account balance password)
  (let ([counter 0])
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (password-error m)
      (set! counter (+ counter 1))
      (if (>= counter 7)
          (call-the-cops)
          "Incorrect password"))
    (define (make-alias new-password)
      (lambda (p m) (if (eq? new-password p)
                        (dispatch password m)
                        password-error)))
    (define (reset-counter) (set! counter 0))
    (define (dispatch p m)
      (if (eq? p password)
          (begin (reset-counter)
                 (cond [(eq? m 'withdraw) withdraw]
                       [(eq? m 'deposit) deposit]
                       [(eq? m 'make-alias) make-alias]
                       [else (error "Unknown request -- MAKE-ACCOUNT"
                                    m)]))
          password-error))
    dispatch))
(define (make-joint acc password new-password)
  ((acc password 'make-alias) new-password))
(define peter-acc (make-account 200 'os))
(define paul-acc (make-joint peter-acc 'os 'rose))

;Exercise 3.8
(define f
  (let ([counter 0])
    (lambda (n) (if (= counter 0)
                    (begin (set! counter (+ counter 1))
                           n)
                    0))))