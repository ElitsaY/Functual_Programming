; 1. Да се напише итеративна функция, която за дадени:
; - [from, to] - интервал от числа
; - term - едноаргументна функция над цели числа
; - operation - бинарна операция над acc и term(x)
; - bottom - начална стойност
; Пресмята натрупаната стойност в bottom, получена чрез
; обхождане на интервала [from, to] с операция op
; над стойностите получени чрез term(x)
(define (id x) x)
;рекурсивно
(define (next x) (+ x 1))
;(define (accumulate from to term next operation bottom)
; (if (> from to) bottom (operation (term from) (accumulate (next from) to term next operation bottom))))

;итеративно

(define (accumulate bottom from to term next operation)
  (if(> from to) bottom (accumulate  (operation bottom (term from)) (next from) to term next operation)))

; Реализирайте следните функции чрез accumulate:
;-----------------------------------------------
; 2. Факториел.
(define (fact n) (accumulate 1 1 n id next *))

; 3. Проверява дали даден предикат е верен за всички числа
; в даден интервал.
; Hint: вместо да ползвате директно and и or -
; (define (and2 x y) (and x y))
; p2 returns true or false;

;------------------------------------------------------------------------------------------------------------------
(define (and2 x y) (and x y))
(define (for-all? from to p?) (accumulate #t from to p? next and2))
;-------------------------------------------------------------------------------------------------------------------

; 4. Проверява дали някое число в даден интервал
; изпълнява даден предикат.
;------------------------------------------------------------------------------------------------------------------
(define (isEven? x) (if(even? x) #t #f))
(define (or2 x y) (or x y))
(define (exists? from to p?) (accumulate #f from to p? next or2))
;------------------------------------------------------------------------------------------------------------------

; 5. Намира броя на целите числа в интервал,
; които изпълняват даден предикат.

(define (count-p from to p?)
  (define (my-operation bottom value)
    (if(p? value) (+ bottom 1) bottom))
  (accumulate 0 from to id next my-operation))

; 6. Проверява дали в целочисления интервал [a,b]
; съществуват две различни цели числа x и y, такива че:
; f(x) = g(x) и f(y) = g(y)
; Примери:
;   (meetTwice? id (lambda (x) (- x)) -3 1) -> #f
;   (meetTwice? id sqrt 0 5) -> #t (за 0 и 1)

;(accumulate bottom from to term next operation)

(define (meet-twice f g from to) (> 1
        (accumulate 0 from to id next (λ(value bottom) (if(= (f value) (g value)) (+ bottom 1) bottom)))))

; 7. Обръща записа на дадено естествено число
; Hint: (count-digits n) <=> (+ 1 (floor (log n 10)))c xc 
(define (count-digits-i n)
  (define (digits n count)
    (if (zero? n) count (digits (quotient n 10) (+ count 1))))
  (digits n 0))

(define (pow-i x n)
  (define (iter i step x)
    (if(zero? i) step (iter (- i 1) (*  step x) x)))
  (iter n 1 x))

;reverse за всяка цифра правя натрупване *10 и + next e всяка цифра на n
;(define (reverse-digits n)
 ; (define (term from) (remainder (quotient n (pow-i 10 from)) 10))
 ; (define (my-operation bottom value)(+ (* bottom 10) value))
 ; (accumulate 0 0 (- (count-digits-i n) 1) term next my-operation))

(define (reverse-digits n)
  (accumulate 0 0 (- (count-digits-i n) 1) (lambda(from)(remainder (quotient n (pow-i 10 from)) 10)) next
              (lambda(bottom value)(+ (* bottom 10) value))))

; 8. Намира броя на палиндромите в интервала [a,b]
(define (count-palindromes from to)
  (accumulate 0 from to id next (lambda(bottom value)(if(= value (reverse-digits value))(+ 1 bottom) bottom))))


; 9. Намира средната цифра на записа на дадено число.
; Ако n има четен брой цифри, функцията връща -1.
; Примери:
;   (middle-digit 452) -> 5
;   (middle-digit 4712) -> -1
(define (middle-digit n)
  (define digits-count (count-digits-i n))
  (if(even? digits-count) -1 (remainder (quotient n (pow-i 10 (quotient digits-count 2))) 10)))
