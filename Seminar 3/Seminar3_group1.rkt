;(compose f g), която връща композицията на две дадени едноместни функции:
(define (compose f g)(lambda(x)(f(g x))))
(define f (compose (lambda (x) (+ x 1)) (lambda (x) (* x x))))
;((x^2)+1) (f 3) -> 10

;Да се напише функцията от по-висок ред (repeat n f), която връща n-кратната композиция на дадена функция f:
; (f (f (f x)))
(define (repeat f n)(if(= n 1) f (lambda(x)(f ((repeat f (- n 1)) x)))))

(define f (repeat (lambda (x) (+ x 1)) 5))
;(f 10) -> 15

;Да се напише функция (twist k f g), която за дадени едноместни функции f и g и четно число k връща функция,
;еквивалентна на f(g(f(g(...(x)...)))), където общият брой извиквания на f и g е k.

;(define (twist k f g)
;  ((if(even? k) (lambda(x)(g ((twist (- 1 k) f g) x))) (lambda(x)(f ((twist (-1 k) f g) x ))))
;                      ))

(define (twist k f g)
  (define(to-repeat k) (if(even? k) f g))
  (if(= k 1) g (lambda(x)((to-repeat k) ((twist (- k 1) f g) x)))))

(define (++ x) (+ x 1))
(define (sq x) (* x x))
(define foo (twist 4 ++ sq))
; това ще смята ((((x^2)+1)^2)+1)
(define bar (twist 2 ++ sq))
; това ще смята ((x^2)+1)
;(foo 2) -> 26
;(bar 2) -> 5

(define (accumulate bottom from to term next operation)
  (if(> from to) bottom (accumulate  (operation bottom (term from)) (next from) to term next operation)))

(define (id x) x)

;Да се напише функция (!! n), която по дадено естествено число n изчислява n!! - произведението на всички числа,
;по-малки или равни на n, със същата четност:
(define (double-fact n) (accumulate 1 (if(even? n) 2 1) n id (lambda(from)(+ from 2)) *))

;Да се напише функция (nchk n k), която за дадени естествени числа n и k изчислява биномния коефициент 'n над k', използвайки:
;само accumulate
; името идва от n-choose-k - по колко начина можем да изберем k неща измежду n

(define (binom n k)(/ (accumulate 1 (+ 1 (- n k)) n id (lambda(x)(+ x 1)) *) (accumulate 1 1 k id (lambda(x)(+ x 1)) *)))
;(accumulate 1 1 (- n k) id (lambda(x)(+ x 1)) *)

;Да се напише функция (2^ n), която изчислява 2n (където n е естествено), използвайки:
;само accumulate

(define (next x)(+ x 1))

(define (pow-two n)(accumulate 1 1 n id next (lambda(bottom value)(+ (* bottom 2) (* 0 value)))))

(define (pow-two2 n) (accumulate 1 1 n (lambda(x)(+ (* 0 x) 2)) next *))

;Да се напише функция (sum-powers k n), която намира сумата на всички степени на числото k, не по-големи от n:

(define (sum-powers k n)(accumulate 1 k n id (lambda(from)(* from k)) +))

;Да се напише функция (divisors-sum n), която намира сумата на всички делители на естественото число n.
(define (divisors-sum n)(accumulate 0 1 (sqrt n) (lambda(x)(if(zero? (remainder n x)) ( if(= x (/ n x)) x (+ x (/ n x))) 0)) next +))
; 1 + 2 + 4 + 8 = 15
; 1 + 2 + 3 + 4 + 6 + 12 = 13 + 8 + 7

;проверява дали число е просто
(define (isPrime? n) (if(= (next n) (divisors-sum n)) #t #f))

;Да се напише функцията от по-висок ред (repeat n f), използвайки accumulate
