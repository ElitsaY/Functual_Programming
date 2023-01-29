(define (accumulate-i op bottom from to term next)
    (if (> from to) bottom
        (accumulate-i op (op bottom (term from)) (next from) to term next)))

(define (id x) x)
(define (next x)(+ x 1))
;----------------------------------------------------------------------------------------------

;Задача 1
(define (arg-extr extr f a b) (accumulate-i (lambda(x y)(if(extr (f x) (f y)) y x)) a (+ a 1) b id next))
(define (argmin f a b)(arg-extr > f a b))

;Задача 2
(define (div-count n)(accumulate-i (lambda(bottom x)(if(zero? (remainder n x)) (+ bottom 1) bottom)) 0 1 n id next))

(define (best-pair a b)
  (let ((max-div (arg-extr < div-count (+ 1 (* a 2)) (- (* b 2) 1))))
       (if (odd? max-div)
           (cons (quotient max-div 2) (+ (quotient max-div 2) 1))
           (cons (- (quotient max-div 2) 1) (+ (quotient max-div 2) 1)) )))

;Задача 4:
(define(integrate2 f a b c d dx dy)
  (define (integrate-y y)
    (* dx (accumulate-i (lambda(bottom x)(+ bottom (f x y))) 0 a b id (lambda(x)(+ x dx)))))
  (* dy (accumulate-i + 0 c d integrate-y (lambda(y)(+ y dy))))
)

(define (all-rows board n rotate-state)
  (define (rook-count row)
    (accumulate-i (lambda(bottom x)(if(and x #t) (+ bottom 1) bottom))
                                             0 0 (- n 1)
                                             (lambda (x)(if(= rotate-state 0)
                                                           (if(board row x n) #t #f)
                                                           (if(board x row n) #t #f) ) ) next))
  (accumulate-i (lambda(bottom x)(if(= x 1) (and bottom #t) #f)) #t 0 (- n 1) rook-count next))
(define(n-rooks board n) (and (all-rows board n 0) (all-rows board n 1)) )

;----------------------------------------------------------------------------------------------------
;Задача 1: тестове
(define (mod7 x) (modulo x 7))
(define (sqf x)(+ (* x x) (- 5 (* 6 x))))

(equal? (argmin mod7 45 50) 49)
(equal? (argmin sqf 2 5) 3)

;Задача 2: тестове
(best-pair 10 20)
(best-pair 20 50) ;-> 60 - 12 делителя - 1 2 3 4 5 6 10 12 15 20 30 60

;Задача 3:
(define pi 3.14159265359)
(define (f x y) (+ x (sin y) 1))
(let ((res (integrate2 f 0 2 (- pi) pi 0.01 0.01)))
  (/ res pi)) ;-> приблизително 8; забележете по-високите стойности за dx и dy

;Задача 4:
(define (board1 x y n)
  (= (remainder (+ x 2) n) y))
(define (board2 x y n)
  (= (min (+ x 2) (- n 1)) y))
(n-rooks board1 5) ;-> #t
(n-rooks board2 5) ;-> #f