#lang racket

(require "2-4.rkt")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equal? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero-rat? x)
    (= (numer x) 0))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equal? '(rational rational)
       (lambda (x y) (equal-rat? x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero-rat? x)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
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
  (define (equal-complex? z1 z2)
    (or (and (= (real-part z1) (real-part z2))
             (= (imag-part z1) (imag-part z2)))
        (and (= (magnitude z1) (magnitude z2))
             (= (angle z1) (angle z2)))))
  (define (=zero-complex? z1)
    (= (magnitude z1) 0))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equal? '(complex complex)
       (lambda (z1 z2) (equal-complex? z1 z2)))
  (put '=zero? '(complex)
       (lambda (z) (=zero-complex? z)))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;Exercise 2.78
(define (attach-tag type-tag contents)
  (if (pair? contents)
      (cons type-tag contents)
      contents))
(define (type-tag datum)
  (cond [(pair? datum) (car datum)]
        [(number? datum) 'scheme-number]
        [else (error "Bad tagged datum -- TYPE-TAG" datum)]))
(define (contents datum)
  (cond [(pair? datum) (cdr datum)]
        [(number? datum) datum]
        [else (error "Bad tagged datum -- TYPE-TAG" datum)]))

;Exercise 2.79
(define (equ? x y) (apply-generic 'equal? x y))

;Exercise 2.80
(define (=zero? x) (apply-generic '=zero? x))

;End exercise
(define (put-coercion x) '()) ;stub implementations
(define (get-coercion x) '())
;(define (scheme-number->complex n)
;  (make-complex-from-real-imag (contents n) 0))
;(put-coercian 'scheme-number 'complex scheme-number-complex)

;Exercise 2.82
;(define (apply-generic op . args)
;  (let ([type-tags (map type-tag args)]
;        [arg-contents (map contents args)])
;    (let ([proc (get op type-tags)])
;      (if proc
;          (apply proc arg-contents)
;          (apply-with-coercion op type-tags arg-contents)))))
;(define (apply-with-coercion op type-tags arg-contents)
;  (define (try-to-coerce type not-tried tried)
;    (define (loop type other-types new-type-list)
;      (if (null? other-types)
;          (get op (cons type new-type-list))
;          (let* ([next-type (car other-types)] 
;                 [nt->t (get-coercion next-type type)])
;            (if nt->t
;                (loop type (cdr other-types) (cons (nt->t next-type) new-type-list))
;                #f))))
;    (let ([proc (loop (type (append tried not-tried) '()))])
;      (if proc
;          (apply proc arg-contents)
;          (if (null? not-tried)
;              (error "No method for these types"
;                     (list op type-tags))
;              (try-to-coerce (car not-tried) (cdr not-tried) (cons type tried))))))
;  (try-to-coerce (car type-tags) (cdr type-tags) '()))

;Exercise 2.83
; in interger package
(define (raise-integer i) 
  (make-rational i 1))
(put 'raise 'integer raise-integer)
; in rational package
(define (raise-rational r)
    (make-real (/ (numer r) (denom r))))
(put 'raise 'rational raise-rational)
; in real package
(define (raise-real n)
  (make-complex-from-real-imag n 0)) 
(put 'raise 'real raise-real)
; generic operation raise
(define (raise x) (apply-generic 'raise x))

;Exercise 2.84
(define (get-tier type)
  (cond [(eq? type 'integer) 0]
        [(eq? type 'rational) 100]
        [(eq? type 'real) 200]
        [(eq? type 'complex) 300]))
(define (compare-tiers t1 t2)
  (if (< (get-tier t1) (get-tier t2)) t2 t1))
(define (get-highest-tier types)
  (define (loop highest-so-far types)
    (if (null? types)
        highest-so-far
        (loop (compare-tiers (car types) highest-so-far) (cdr types))))
  (loop (car types) (cdr types)))
(define (same-tier? t1 t2)
  (= (get-tier t1) (get-tier t2)))
;(define (apply-generic op . args)
;  (let ([type-tags (map type-tag args)]
;        [arg-contents (map contents args)])
;    (let ([proc (get op type-tags)])
;      (if proc
;          (apply proc arg-contents))
;          (apply-with-coercion op type-tags args))))))
(define (apply-with-coercion op type-tags args)
  (let* ([top-tier (get-highest-tier type-tags)]
         [coerced-list (coerce-list top-tier args)]
         [proc (get op (map type-tag coerced-list))])
    (if proc
        (apply proc (map contents coerced-list))
        (error "No method for these types"
               (list op type-tags)))))
(define (coerce-list top-tier tc-pairs)
  (cons (coerce (car tc-pairs) top-tier)
        (coerce-list top-tier (cdr tc-pairs))))
(define (coerce tc-pair t)
  (if (same-tier? t (type-tag tc-pair))
      tc-pair
      (coerce (raise (contents tc-pair)) t)))

;Exercise 2.85
;in rational package
(define (project-rational r)
  (make-integer (numer r)))
(put 'project 'rational project-rational)
;in real package
(define (project-real n)
  (make integer (round n)))
(put 'project 'real project-real)
;in complex package
(define (project-complex z)
  (make-real (real-part z)))
(put 'project 'complex project-complex)
;generic project operation
(define (project x) (apply-generic 'project x))
(define (drop x) 
  (let ([simplified-value (project x)])
    (if (or (lowest-tier? (type-tag x)) (not (equ? (raise simplified-value) x)))
        x
        (drop simplified-value))))
(define (lowest-tier? x)
  (same-tier? x 'integer))
(define (apply-generic op . args)
  (let ([type-tags (map type-tag args)]
        [arg-contents (map contents args)])
    (let ([proc (get op type-tags)])
      (if proc
          (drop (apply proc arg-contents))
          (drop (apply-with-coercion op type-tags args))))))
;End exercise



(define (install-polynomial-package)
  ;; internal procedures
  (define (add-terms L1 L2)
  (cond [(empty-termlist? L1) L2]
        [(empty-termlist? L2) L1]
        [else
         (let ([t1 (first-term L1)] [t2 (first-term L2)])
           (cond [(> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2))]
                 [(< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2)))]
                 [else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))]))]))
  (define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ([t2 (first-term L)])
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? x y) (eq? x y))
  (define (variable? x) (symbol? x))
  (define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLU" (list p1 p2))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

;Exercise 2.87
;in polynomial package
(define (=zero?-poly p)
  (if (empty-termlist? (term-list p))
      #t
      (and (=zero? (coeff (first-term p)))
           (=zero?-poly (rest-terms p)))))
(put '=zero? 'polynomial =zero?-poly)

;Exercise 2.88
;given a generic negation operation
;in polynomial package
(define (sub-poly p1 p2)
  (add-poly p1 (negate p2)))
(put 'sub '(polynomial polynomial) 
     (lambda (x y) (tag (sub-poly x y))))
;Exercise 2.89
;in polynomial package
(define (adjoin-term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons (coeff term) term-list)))
(define (first-term term-list)
  (make-term (- (length term-list) 1) (car term-list)))

;Exercise 2.91
;in polynomial package
(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (let ([div-result (div-terms (term-list p1) (term-list p2))]
            [var (variable p1)])
        (list (make-poly var (car div-result))
              (make-poly var (cadr div-result))))
      (error "Poly not in the same variable -- DIV-POLY" (list p1 p2))))
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ([t1 (first-term L1)]
            [t2 (first-term L2)])
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let* ([new-c (div (coeff t1) (coeff t2))]
                   [new-o (- (order t1) (order t2))]
                   [new-t (make-term new-c new-o)]
                   [mult (mul-terms L2 (adjoin-term new-term (the-empty-termlist)))]
                   [diff (add-terms L1 (negate-terms mult))])
              (let ([rest-of-result (div-terms diff L2)])
                (cons (adjoin-term new-term (car rest-of-result))
                      (cadr rest-of-result))))))))
(put 'div '(polynomial polynomial)
     (lambda (p1 p2) 
       (let ([div-result (div-poly p1 p2)])
         (list (tag (car div-result))
               (tag (cadr div-reuslt))))))
  
      