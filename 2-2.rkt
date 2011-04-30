#lang racket

(require "1-2.rkt")

;Exercise 2.17
(define (my-last-pair list)
  (let ([next (cdr list)])
    (if (null? next)
      list
      (my-last-pair next))))

;Exercise 2.18
(define (my-reverse items)
  (if (null? items)
      null
      (append (my-reverse (cdr items)) (list (car items)))))

;Exericse 2.19
(define (count-change amount)
  (define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))
  (define (first-denomination coin-values) (car coin-values))
  (define (except-first-denomination coin-values) (cdr coin-values))
  (define (no-more? coin-values) (null? coin-values))
  (let ([us-coins (list 1 10 5 25 50)])
    (cc amount us-coins)))

;Exercise 2.20
(define (same-parity i . l)
  (define (helper i l)
    (cond ((null? l) null)
          ((not (odd? (- (car l) i)))
           (cons (car l) (helper i (cdr l))))
          (else (helper i (cdr l)))))
  (cons i (helper i l)))

;Exercise 2.21
(define (square-list1 items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list1 (cdr items)))))
(define (square-list2 items)
  (map square items))

;Exercise 2.23
(define (my-for-each f list)
  (cond [(null? list) #t]
        [else 
         (f (car list))
         (my-for-each f (cdr list))]))

(define (count-leaves-old x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;Exercise 2.27
(define (deep-reverse items)
  (cond [(null? items) null]
        [(pair? (car items)) (append (deep-reverse (cdr items))
                                     (list (deep-reverse (car items))))]
        [else (append (deep-reverse (cdr items))
                      (list (car items)))]))

;Exercise 2.28
(define (fringe tree)
  (cond [(null? tree) '()]
        [(pair? tree)
         (append (fringe (car tree)) (fringe (cdr tree)))]
        [else (list list)]))

;Exercise 2.29
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))
(define (total-weight mobile)
  (cond [(null? mobile) 0]
        [(pair? mobile)
         (+ (total-weight (branch-structure (left-branch mobile)))
            (total-weight (branch-structure (right-branch mobile))))]
        [else mobile]))
(define (balanced? mobile)
  (cond [(null? mobile) #t]
        [(pair? mobile)
         (let* ([left (left-branch mobile)]
                [right (right-branch mobile)]
                [left-struct (branch-structure left)]
                [right-struct (branch-structure right)])
           (and (= (* (branch-length left) 
                      (total-weight left-struct))
                   (* (branch-length right)
                      (total-weight right-struct)))
                (balanced? left-struct)
                (balanced? right-struct)))]
        [else #t]))

;Exercise 2.30
(define (square-tree1 tree)
  (cond [(null? tree) '()]
        [(pair? tree) (cons (square-tree1 (car tree))
                            (square-tree1 (cdr tree)))]
        [else (square tree)]))
(define (square-tree2 tree)
  (map (lambda (element)
         (if (pair? element)
             (square-tree2 element)
             (square element))) tree))

;Exercise 2.31
(define (tree-map f tree)
  (map (lambda (element)
         (if (pair? element)
             (tree-map f element)
             (f element))) tree))

;Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (e) (cons (car s) e)) rest)))))

(define (my-filter predicate sequence)
  (cond [(null? sequence) null]
        [(predicate (car sequence))
         (cons (car sequence)
               (my-filter predicate (cdr sequence)))]
        [else (my-filter predicate (cdr sequence))]))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(define (enumerate-tree tree)
  (cond [(null? tree) null]
        [(not (pair? tree)) (list tree)]
        [else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))]))
(define (sum-odd-squares tree)
  (accumulate + 0 (map square (my-filter odd? (enumerate-tree tree)))))
(define (even-fibs n)
  (accumulate cons null (my-filter even?
                                   (map fast-fib (enumerate-interval 1 n)))))

;Exercise 2.33
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;Exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (e)
                         (if (not (pair? e))
                             1
                             (count-leaves e))) t)))

;Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))
(define (transpose mat)
  (accumulate-n cons null mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (w) (matrix-*-vector cols w)) m)))

;Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;Exercise 2.39
(define (reverse-right sequence)
  (accumulate (lambda (x y) (append y (list x))) null sequence))
(define (reverse-left sequence)
  (fold-left (lambda (x y) (append (list y) x)) null sequence))
;end exercise

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs-old n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap 
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;Exercise 2.41
(define (ordered-triple-sum n s)
  (filter (lambda (triple)
            (triple-sum triple s))
          (unique-triples n)))
(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (triple-sum triple s)
  (= (+ (car triple) (cadr triple) (caddr triple)) s))

;Exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(define empty-board '())
(define (adjoin-position new-row k rest-of-queens) 
  (let ([queen-pos (list new-row k)])
    (cons queen-pos rest-of-queens)))
(define (get-row pos)
  (car pos))
(define (get-col pos)
  (cadr pos))
(define (safe? k positions)
  (define (helper k k-row rest-pos)
    (cond [(null? rest-pos) #t]
          [(let* ([test-pos (car rest-pos)]
                  [test-row (get-row test-pos)]
                  [test-col (get-col test-pos)])
             (or (= k-row test-row) 
                 (= (abs (/ (- test-row k-row) 
                            (- test-col k))) 1))) #f]
          [else (helper k k-row (cdr rest-pos))]))
  (helper k (get-row (car positions)) (cdr positions)))

;Exercise 2.44
(define (up-split-old painter n)
  (if (= n 0)
      painter
      (let ([smaller (up-split painter (- n 1))])
        (below painter (beside smaller smaller)))))
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ([top (beside (tl painter) (tr painter))]
          [bottom (beside (bl painter) (br painter))])
      (below bottom top))))
(define (flipped-pairs painter)
  (let ([combine4 (square-of-four identity flip-vert
                                  identity flip-vert)])
    (combine4 painter)))
(define (square-limit painter n)
  (let ([combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)])
    (combine4 (corner-split painter n))))
(define (split s1 s2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ([smaller ((split s1 s2) painter (sub1 n))])
          (s1 painter (s2 smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin frame)
     (add-vect (scale-vect (x-cor-vect v)
                           (edge-1 frame))
               (scale-vect (y-cor-vect v)
                           (edge-2 frame))))))
(define (make-vect x y) (cons x y))
(define (x-cor-vect v) (car v))
(define (y-cor-vect v) (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (x-cor-vect v1) (x-cor-vect v2))
             (+ (y-cor-vect v1) (y-cor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (x-cor-vect v1) (x-cor-vect v2))
             (- (y-cor-vect v1) (y-cor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (x-cor-vect v))
             (* s (y-cor-vect v))))
(define (make-frame-1 origin edge1 edge2) (cons origin (cons edge1 edge2)))
(define (origin-1 f) (car f))
(define (edge-1a f) (cadr f))
(define (edge-2b f) (cddr f))
(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define (origin f) (car f))
(define (edge-1 f) (cadr f))
(define (edge-2 f) (caddr f))
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define outline
  (let* ([bl (make-vect 0 0)]
         [br (make-vect 1 0)]
         [tl (make-vect 0 1)]
         [tr (make-vect 1 1)]
         [edge1 (make-segment bl br)]
         [edge2 (make-segment bl tl)]
         [edge3 (make-segment br tr)]
         [edge4 (make-segment tl tr)])
    (segments->painter (list edge1 edge2 edge3 edge4))))
(define painter-X
  (let* ([bl (make-vect 0 0)]
         [br (make-vect 1 0)]
         [tl (make-vect 0 1)]
         [tr (make-vect 1 1)]
         [edge1 (make-segment bl tr)]
         [edge2 (make-segment tl br)])
    (segments->painter (list edge1 edge2))))
(define diamond
  (let* ([m1 (make-vect 0 0.5)]
         [m2 (make-vect 0.5 0)]
         [m3 (make-vect 1 0.5)]
         [m4 (make-vect 0.5 1)]
         [edge1 (make-segment m1 m2)]
         [edge2 (make-segment m2 m3)]
         [edge3 (make-segment m3 m4)]
         [edge4 (make-segment m4 m1)])
    (segments->painter (list edge1 edge2 edge3 edge4))))
