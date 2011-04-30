#lang racket

(provide variable? same-variable? =number? make-sum make-product make-exponentiation)

;Exercise 2.54
(define (equality? ob1 ob2 type-test eq-test)
  (and (type-test ob1) (type-test ob2) (eq-test ob1 ob2)))
(define (my-equal? items1 items2)
  (cond ((and (null? items1) (null? items2)) #t)
        ((and (pair? items1) (pair? items2))
         (and (my-equal? (car items1) (car items2))
              (my-equal? (cdr items1) (cdr items2))))
        ((or (equality? items1 items2 symbol? eq?)
             (equality? items1 items2 number? =))
         #t)
        (else #f)))

;End exercise
(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))]
        [(exponention? exp)
         (make-product (make-product
                        (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sum (exponent exp) (- 1))))
                       (deriv (base exp) var))]
        [else
         (error "unknown expression type -- DERIV" exp)]))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (equality? v1 v2 variable? eq?))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;Exercise 2.56
(define (make-exponentiation base n)
  (cond [(=number? base 0) 0]
        [(=number? n 0) 1]
        [(=number? n 1) base]
        [(and (number? base) (number? n)) (expt base n)]
        [else (list '** base n)]))
(define (exponention? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

;Exercise 2.57
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (addend s) (cadr s))
(define (augend s) 
  (if (null? (cdddr s))
      (caddr s)
      (remove (cadr s) s)))
(define (multiplier p) (cadr p))
(define (multiplicand p) 
  (if (null? (cdddr p))
      (caddr p)
      (remove (cadr p) p)))

;Exercise 2.58a
;(define (make-sum a1 a2)
;  (cond [(=number? a1 0) a2]
;        [(=number? a2 0) a1]
;        [(and (number? a1) (number? a2)) (+ a1 a2)]
;        [else (list a1 '+ a2)]))
;(define (addend s) (car s))
;(define (augend s) (caddr s))
;(define (sum? exp) (and (pair? exp) (eq? (cadr exp) '+)))
;(define (make-product m1 m2)
;  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
;        [(=number? m1 1) m2]
;        [(=number? m2 1) m1]
;        [(and (number? m1) (number? m2)) (* m1 m2)]
;        [else (list m1 '* m2)]))
;(define (multiplier p) (car p))
;(define (multiplicand p) (caddr p))
;(define (product? p) (and (pair? p) (eq? (cadr p) '*)))        

;Exercise 2.58b
;(define (addend s)
;  (define (helper s a)
;    (if (eq? (cadr s) '+)
;        (if (null? a) (car s) (append a (list (car s))))
;        (helper (cddr s) (append a (list (car s) (cadr s)))))) 
;  (helper s '()))
;(define (augend s)
;  (if (eq? (cadr s) '+)
;      (if (null? (cdddr s)) (caddr s) (cddr s))
;      (augend (cddr s))))
;(define (sum? s) (member? '+ s))
;(define (multiplicand p)
;  (if (null? (cdddr p))
;      (caddr p)
;      (cddr p)))
;(define (member? item items)
;  (cond [(null? items) #f]
;        [(eq? (car items) item) #t]
;        [else (member item (cdr items))]))

;End exercise

(define (element-of-set? x set)
  (cond [(null? set) #f]
         [(equal? x (car set)) #t]
         [else (element-of-set? x (cdr set))]))
;(define (adjoin-set x set)
;  (if (element-of-set? x set)
;      set
;      (cons x set)))
;(define (intersection-set set1 set2)
;  (cond [(or (null? set1) (null? set2)) '()]
;        [(element-of-set? (car set1) set2)
;         (cons (car set1) (intersection-set (cdr set1) set2))]
;        [else (intersection-set (cdr set1) set2)]))

;Exercise 2.59
;(define (union-set set1 set2)
;  (cond [(null? set1) set2]
;        [(null? set2) set1]
;        [(element-of-set? (car set1) set2)
;         (union-set (cdr set1) set2)]
;        [else (cons (car set1) (union-set (cdr set1) set2))]))

;Exercise 2.60
;(define (element-of-set? x set)
;  (cond [(null? set) #f]
;         [(equal? x (car set)) #t]
;         [else (element-of-set? x (cdr set))]))
;(define (adjoin-set x set)
;  (cons x set))
;(define (intersection-set set1 set2)
;  (cond [(or (null? set1) (null? set2)) '()]
;        [(element-of-set? (car set1) set2)
;         (cons (car set1) (intersection-set (cdr set1) set2))]
;        [else (intersection-set (cdr set1) set2)]))
;(define (union-set set1 set2)
;  (append set1 set2))
;End exercise

;(define (element-of-set? x set)
;  (cond [(null? set) #f]
;        [(= x (car set)) #t]
;        [(< x (car set)) #f]
;        [else (element-of-set? x (cdr set))]))
(define (intersection-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ([x1 (car set1)] [x2 (car set2)])
        (cond [(= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2)))]
              [(< x1 x2)
               (intersection-set (cdr set1) set2)]
              [(< x2 x1)
               (intersection-set set1 (cdr set2))]))))

;Exercise 2.61
;(define (adjoin-set x set)
;  (cond [(null? set) (list x)]
;        [(= (car set) x) set]
;        [(< x (car set)) (cons x set)]
;        [else (cons (car set) (adjoin-set x (cdr set)))]))

;Exercise 2.62
(define (union-ordered set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [else (let ([x1 (car set1)] [x2 (car set2)])
                (cond [(= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2)))]
                      [(< x1 x2) (cons x1 (union-set (cdr set1) set2))]
                      [(< x2 x1) (cons x2 (union-set set1 (cdr set2)))]))]))
;End exercise

(define (entry tree) (car tree))
;(define (left-branch tree) (cadr tree))
;(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
;(define (element-of-set? x set)
;  (cond [(null? set) #f]
;        [(= x (entry set)) #t]
;        [(< x (entry set)) (element-of-set? (left-branch set))]
;        [(> x (entry set)) (element-of-set? (right-branch set))]))
;(define (adjoin-set x set)
;  (cond [(null? set) (make-tree x '() '())]
;        [(= x (entry set)) set]
;        [(< x (entry set)) 
;         (make-tree (entry set)
;                    (adjoin-set x (left-branch set))
;                    (right-branch set))]
;        [(> x (entry set))
;         (make-tree (entry set)
;                    (left-branch set)
;                    (adjoin-set x (right-branch set)))]))

;Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ([left-size (quotient (- n 1) 2)])
        (let ([left-result (partial-tree elts left-size)])
          (let ([left-tree (car left-result)]
                [non-left-elts (cdr left-result)]
                [right-size (- n (+ left-size 1))])
            (let ([this-entry (car non-left-elts)]
                  [right-result (partial-tree (cdr non-left-elts)
                                              right-size)])
              (let ([right-tree (car right-result)]
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;Exercise 2.65
(define (op-set s1 s2 op)
  (let* ([list-1 (tree->list-2 s1)]
         [list-2 (tree->list-2 s2)]
         [new-list (op list-1 list-2)])
    (list->tree new-list)))
(define (intersection-set s1 s2)
  (cond [(or (null? s1 s2) '())]
        [else (op-set intersection-ordered s1 s2)]))
(define (union-set s1 s2)
  (cond [(null? s1) s2]
        [(null? s2) s1]
        [else (op-set union-ordered s1 s2)]))
;End exercise

;(define (lookup given-key set-of-records)
;  (cond [(null? set-of-records) #f]
;        [(equal? given-key (key (car set-of-records)))
;         (car set-of-records)]
;        [else (lookup given-ket (cdr set-of-records))]))

;Exercise 2.66
(define (lookup given-key set-of-records)
  (if (null? set-of-records) 
      #f
      (let ([current-key (key (entry set-of-records))])
        (cond [(= given-key current-key) (entry set-of-records)]
              [(< given-key current-key)
               (lookup given-key (left-branch set-of-records))]
              [(> given-key current-key)
               (lookup given-key (right-branch set-of-records))]))))
(define (key record) '()) ;stub implementation for key
;End exercise

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left 
        right 
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
        [(= bit 1) (right-branch branch)]
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (make-ordered-set (make-leaf (car pair) (cadr pair))
                          (make-leaf-set (cdr pairs))))))
(define (make-ordered-set element set)
  (cond [(null? set) (list element)]
        [(<= (weight element) (weight (car set))) (cons element set)]
        [else (cons (car set) (make-ordered-set element (cdr set)))]))
    
;Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'E 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (cond [(pair? tree) 
         (let* ([bit-branch (get-branch symbol tree)]
                [bit (car bit-branch)]
                [branch (cdr bit-branch)])
           (if (leaf? branch)
               (list bit)
               (cons bit (encode-symbol symbol branch))))]))
(define (get-branch symbol tree)
  (let ([left (left-branch tree)]
        [right (right-branch tree)])
    (cond [(element-of-set? symbol (symbols left))
           (cons 0 left)]
          [(element-of-set? symbol (symbols right))
           (cons 1 right)]
          [else (error "No such symbol in tree!" symbol)])))

;Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge pairs)
  (let ([new-tree (make-code-tree (cadr pairs) (car pairs))])
    (if (= (length pairs) 2) 
        new-tree
        (successive-merge (make-ordered-set new-tree (cddr pairs))))))

;Exercise 2.70
(define 1950-song-tree (generate-huffman-tree '((NA 16) (YIP 9) (SHA 3) (A 2) (GET 2) (JOB 2) (BOOM 1) (WAH 1))))
(define (1950-song)
  (let* ([gaj (encode '(GET A JOB) 1950-song-tree)]
         [sna (encode '(SHA NA NA NA NA NA NA NA NA) 1950-song-tree)]
         [way (encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) 1950-song-tree)]
         [shb (encode '(SHA BOOM) 1950-song-tree)]
         [message (list gaj sna gaj sna way shb)])
    (for-each print-it! message)))
(define (print-it! x)
  (display x)
  (newline))
   


        
         
         
               
        
      
      
         