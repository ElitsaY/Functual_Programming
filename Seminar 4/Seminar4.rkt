; функция map -> for each element in list(a1, a2, ..., an) -> returnn list(f(a1), f(a2), f(a3)....f(an))
;(define (map f l) (if(null? l) '() (cons (f (car l))  map ( f (cdr l)))))

; filter -> f - func list(a1 a2... an); if (f ai) = #t -> in the list

;(define (filter p? l)(cond((null? l) l)
;                          ((p? (car l)) (cons (car l) (filter p? (cdr l))))
;                          (else (filter p? (cdr l)))))

;foldl -> accumulate iterative
(define (foldl op bottom l)(if(null? l)
                             bottom
                             (foldl op (op bottom (car l)) (cdr l))))

;foldr -> accumulate recursively
(define (foldr op bottom l)(if(null? l) bottom (op (car l) (foldr op bottom (cdr l)))))

(define (foldr-better condition? op bottom l)
  (if(condition? bottom)
       (op (car l) (foldr-better condition? op (+ bottom 1) (cdr l)))
        '()
        ))

; 1. Връща списък от първите n елемента
(define (take* n lst)(foldr-better (lambda(x)(if(< x n) #t #f)) cons 0 lst))

(define (take** n lst)
  (if(zero? n) '()
     (cons (car lst)(take** (- n 1)(cdr lst)))))


; 2. Връща списък като lst, но без първите n елемента
(define (drop* n lst) (if(zero? n) lst (drop* (- n 1) (cdr lst))))
(define (drop** n lst)(list-tail lst n))

; 3. Генерира списък от целите числа в интервала [a,b]
(define (from-to a b) (if(> a b) '() (cons a (from-to (+ a 1) b))))

; 4. По даден списък от числа - намира сумата им.
(define (sum lst)(foldl + 0 lst))

; 5. Връща последния елемент на списъка lst.
(define (last* lst)(if (equal? '() (cdr lst)) (car lst) (last* (cdr lst))))

; 6. Връща n-тия елемент на списъка lst.
(define (nth n lst) (if(zero? n) (car lst) (nth (- n 1) (cdr lst))))

; 7. Залепя l1 и l2 в списък от наредени 2ки.
; Може списъците да са с различна дължина.
; Пример: (zip '(1 2 3) '(4 5)) -> '((1 . 4) (2 . 5))
(define (mapp f l1 l2)(if(or (null? l1) (null? l2)) '() (cons (f (car l1) (car l2)) (mapp f (cdr l1) (cdr l2)))))
(define (zip l1 l2) (mapp cons l1 l2))

; 8. Връща конкатенацията на lst1 и lst2.
; Реализирайте с рекурсия
(define (append* lst1 lst2)
  (cond ((null? lst2) '())
        ((null? lst1) (cons (car lst2) (append* lst1 (cdr lst2))))
        (else (cons (car lst1) (append* (cdr lst1) lst2)))))

; 9. Връща lst след прилагане на f върху всеки елемент.
(define (map* f lst) (mapp f lst '()))

; 10. Връща списък от елементите на lst,
; за които предиката p е верен
;(define (filter* p lst))

; 11. Като функцията accumulate, но за списъци
; Пример: (foldl* - 0 '(1 2 3 4)) -> -10
;(define (foldl* op acc lst) 'undefined)
