#lang racket

(require "1-2.rkt" "2-3.rkt")
(provide install-rectangular-package install-polar-package put get
         real-part imag-part magnitude angle)

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
;(define (real-part-polar z)
;  (* (magnitude-polar z) (cos (angle-polar z))))
;(define (imag-part-polar z)
;  (* (magnitude-polar z) (sin (angle-polar z))))
;(define (magnitude-polar z) (car z))
;(define (angle-polar z) (cdr z))
;(define (make-from-real-imag-polar x y)
;  (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
;                           (atan y x))))
;(define (make-from-mag-ang-polar r a) 
;  (attach-tag 'polar (cons r a)))
;(define (real-part-rectangular z) (car z))
;(define (imag-part-rectangular z) (cdr z))
;(define (magnitude-rectangular z)
;  (sqrt (+ (square (real-part-rectangular z)) 
;           (square (imag-part-rectangular z)))))
;(define (angle-rectangular z)
;  (atan (imag-part-rectangular z) 
;        (real-part-rectangular z)))
;(define (make-from-real-imag-rectangular x y) 
;  (attach-tag 'rectangular (cons x y)))
;(define (make-from-mag-ang-rectangular r a)
;  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
;(define (rectangular? z)
;  (eq? (type-tag z) 'rectangular))
;(define (polar? z)
;  (eq? (type-tag z) 'polar))
;(define (tag-sift tfs z)
;  (if (null? tfs) 
;      'failed
;      (let ([tf (car tfs)])
;        (if ((car tf) z)
;            ((cdr tf) (contents z))
;            (tag-sift (cdr tfs) z)))))
;(define (real-part z)
;  (let ([result (tag-sift (list (cons rectangular? real-part-rectangular)
;                                (cons polar? real-part-polar)) z)])
;        (if (not (eq? result 'failed))
;            result
;        (error "Unknown type -- REAL-PART" z))))
;(define (imag-part z)
;  (let ([result (tag-sift (list (cons rectangular? imag-part-rectangular)
;                                (cons polar? imag-part-polar)) z)])
;        (if (not (eq? result 'failed))
;            result
;            (error "Unknown type -- IMAG-PART" z))))
;(define (magnitude z)
;  (let ([result (tag-sift (list (cons rectangular? magnitude-rectangular)
;                                (cons polar? magnitude-polar)) z)])
;        (if (not (eq? result 'failed))
;            result
;            (error "Unknown type -- MAGNITUDE" z))))
;(define (angle z)
;  (let ([result (tag-sift (list (cons rectangular? angle-rectangular)
;                                (cons polar? angle-polar)) z)])
;        (if (not (eq? result 'failed))
;            result
;            (error "Unknown type -- ANGLE" z))))
;(define (make-from-real-imag x y)
;  (make-from-real-imag-rectangular x y))
;(define (make-from-mag-ang r a)
;  (make-from-mag-ang-polar r a))
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
;(define (make-from-mag-ang r a)
;  ((get 'make-from-mag-ang 'polar) r a))
;stub implementations for put and get
(define (put a b c) '())
(define (get a b) '())

;Exercise 2.73
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp) (if (same-variable? exp var) 1 0)]
        [else ((get (operator exp) 'deriv) (operands exp) var)]))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (install-deriv-package)
  ;sum
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  ;make-sum is included from 2-3.rkt
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  ;product
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  ;make-product is included from 2-3.rkt
  (define (deriv-product exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))
  ;Exercise 2.73c exponentiation
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  ;make-exponentiation is included from 2-3.rkt
  (define (deriv-exponentiation exp var)
    (make-product (make-product (exponent exp) 
                                (make-exponentiation (base exp)
                                                     (make-sum (exponent exp) -1)))
                  (deriv (base exp) var)))
  ;Exercise 2.73d simply reverse the op and type tags
  (put '+ 'deriv deriv-sum)
  (put '* 'deriv deriv-product)
  (put '** 'deriv deriv-exponentiation)
  'done)
;End exercise

;Exercise 2.74
(define (get-record employee personnel-file)
  ((get 'get-record (type-tag personnel-file)) employee (contents personnel-file)))
(define (get-salary employee personnel-file)
  (let ([employee-record (get-record employee personnel-file)])
    (if employee-record
        ((get 'employee-salary (type-tag employee-record)) (contents employee-record))
        (error "Unknown employee -- GET-RECORD" employee))))
(define (find-employee-record employee files)
  (if (not (null? files)) 
      (let ([employee-record (get-record employee (car files))])
        (if employee
            employee
            (find-employee-record employee (cdr files))))
      (error "Unknown employee -- FIND-EMPLOYEE-RECORD" employee)))

;Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond [(eq? op 'real-part) (* r (cos a))]
          [(eq? op 'imag-part) (* r (sin a))]
          [(eq? op 'magnitude) r]
          [(eq? op 'angle) a]
          [else (error "Unknown op -- MAKE-FROM-MAG-ANG" op)]))
  dispatch)
