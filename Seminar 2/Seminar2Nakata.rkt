
; Ще намираме числата на фибоначи последователно
; и ще помним предходните две.
; Така няма да трябва да ги изчисляваме всеки път
; когато ни потрябват.
;(define (fib-iter n)
;  (define (iter n1 n2 i)
;    (if (zero? i)
;      n2
;      (iter n2
;            (+ n1 n2)
;            (- i 1))))
;  (iter 0 1 n))

(define (q n) (quotient n 10))

; 1. Намира броя на цифрите на дадено естествено число n.
; Реализирайте я рекурсивно.
(define (count-digits n)
  (cond
    ((< n 10) 1)
    (else (+ 1 (count-digits(quotient n 10))))))
  ;(if(zero? n) 0 (+ 1 (count-digits(quotient n 10)))))

; 2. За дадени цяло число x и естествено число n връща x^n.
; Реализирайте я рекурсивно.
(define (pow x n) (if(zero? n) 1 (* x (pow x (- n 1)))))

;3. За дадени числа a и b (a < b)
; намира сумата на целите числа в интервала [a,b]
; Реализирайте я рекурсивно.
(define (interval-sum a b) (if(< a b) (+ a (interval-sum (+ 1 a) b)) 0))

; 4. За дадени цели числа x и n връща x^n.
; Реализирайте я чрез линейна рекурсия (итерация).
(define (pow-i x n)
  (define (iter i step x)
    (if(zero? i) step (iter (- i 1) (*  step x) x)))
  (iter n 1 x))


; 5. Намира броя на цифрите на дадено цяло число n.
; Реализирайте я чрез линейна рекурсия (итерация).
(define (count-digits-i n)
  (define (digits n count)
    (if (zero? n) count (digits (quotient n 10) (+ count 1))))
  (digits n 0))

; 6. За дадени цели числа a и b
; намира сумата на целите числа в интервала [a,b].
; Трябва да работи и за a > b.
; Реализирайте я чрез линейна рекурсия (итерация).
(define (interval-sum-i a b)
  (define (summ num current-sum b)
    (if(< num b) (summ (+ num 1) (+ current-sum num) b) current-sum))
  (summ a 0 b))

; 7. За дадено цяло число n връща число,
; чийто цифри са в обратен ред.
; Реализирайте го чрез линейна рекурсия (итерация).
(define (reverse-digits-i n)
  (define(reverse-i rev n)
    (if(zero? n) rev (reverse-i (+ (remainder n 10) (* rev 10)) (quotient n 10))))
  (reverse-i 0 n))

; 8. За дадени цели числа x и n връща x^n,
; но ако n е четно, то x^n = (x^(n/2))^2
; Реализирайте я чрез линейна рекурсия (итерация).
(define (fast-pow x n)
  (define (step current-step result n)
    (if(> n current step) result (if(even? current-step) (step (* current-step 2)))
  (step 0 1 n))
;x = 5
; n = 0 - 1
; n = 2 ->
; n = 4 -> 