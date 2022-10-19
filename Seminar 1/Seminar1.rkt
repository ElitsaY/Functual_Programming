; функция, която връща следващото число
(define (succ n) (+ n 1))

; функция, която връща предходното число
(define (pred n) (- n 1))

; 3. За дадено число n, връща:
;     1) n/2, ако n е четно
;     2) n, в противен случай
; пример
; if е специална форма и е израз.
; Можем да го свържем със символ.
; (if <condition> <then-value> <else-value>)
; Условието е #t ако не е #f
;(define one-eq-two?
;    (if (= 1 2)
;      "Im the pope!"
 ;     "Expected"))
(define (safe-div n) (if(odd? n) n (/ n 2)))

; 4. Намира n!.
(define (factorial n) (if(= n 0) 1 (* n (factorial(- n 1)))))
;(define (factorial n)(if(= n 0) 1(* n (factorial(- n 1)))))

; 5. Намира n-тото число на Фибоначи.
(define (fib n)(if(< n 2) 1 (+ (fib(- n 1)) (fib(- n 2)))))

; 6. Намира сумата на 2 естествени числа.
; Използвайте succ и pred.
(define (sum n m) (+ (succ n) (pred m)))

; 7. намира произведението на 2 естествени числа.
; Използвайте sum/succ и pred.
;(- (* (succ b) (pred a) (- succ(b) a)) = 
(define (product a b) (+ (* (succ b) (pred a) ) (- (succ b) a)))

; 7.1 рекусрсия по идея на наката
(define (product x y)
  (cond ((= 0 x) 0)
        ((= 1 x) y)
        (else (sum y (product (pred x) y)))))

; 8. За даден едноместен предикат p, връща отрицанието му.
; Не отрицанието на резултата,
; а нов предикат който е отрицание на p.
(define (complement p) (lambda (x)(not (p x))))

; 9. За дадена функция на два аргумента f,
; връща функцията над разменени аргументи.
(define (flip f) (lambda (x y) (f y x)))

; 10. За дадени едноаргументни функции f и g
; връща композицията им (f.g)
; Пример: ((compose f g) x) -> (f (g x))
;(define (compose f g)(lambda (x) (f (g x)))) - naka
(define (compose f g) (lambda (x) (f (g x))))

; 11. За дадена едноаргументна функция f и число n,
; връща n-тото прилагане на f. Тоест f^n.
; Пример: ((repeat f 3) x) -> (f (f (f x))) за някоя f
(define (repeat f n)
  (if (= n 1) f ;функция
      (lambda (x) (f ((repeat f (- n 1)) x)))))