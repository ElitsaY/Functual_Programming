;Задача 1: Едно естествено число наричаме свършено, ако е с 2 по-малко от сумата на всичките си делители по-малки от него.
; Да се реализира функция done?, която проверява дали дадено число е свършено.
; 24 = 1 + 2 + 12 + 4 + 6 + 3 + 8  
; 20 ; 1 + 2 + 5 + 10 = 18
; 10 = 1 + 2 + 5 = 8
; 12 = 1 + 2 + 3 + 4 + 6 = 16
; 18 = 1 + 2 + 3 + 6 + 9 = 10 + 8 + 3 = 21  

;(define (done? n)
;  (define (sum-div i n summ)
;    (cond ( (= i n) summ )
;          ( (= (remainder n i) 0) (sum-div (+ i 1) n (+ summ i)))
;          ( else (sum-div (+ i 1) n summ) )
;          )
;    )
;  (if(= (sum-div 1 n 0) (+ n 2) ) #t #f)
;  )

;(define (done2? n)
;  (define (sum-div2 i numm summ)
;    (if(> i (sqrt n)) 0
;       (if(= (remainder num i) 0) (sum-div (+ i 1) numm (+ summ (+ i (quotient numm i))))))))

(define (next x)(+ x 1))

(define (accumulate op bottom a b term next)
  (if (> a b) bottom
          (op (term a) (accumulate op bottom (next a) b term next))))

(define (accumulate-i op bottom a b term next)
  (if (= a b) bottom
          (accumulate-i op (op bottom (term a)) (next a) b term next)))

;working version
;-----------------------------------------------------------------------------------------------------------------
(define (done? n) (if(= (+ n 2) (accumulate-i + 0 1 n (lambda(x)( if(= (remainder n x) 0) x 0 )) next)) #t #f))
;-----------------------------------------------------------------------------------------------------------------

; Да се реализира функция sum-almost-done, която по подадени естествени числа a и b намира сумата на всички числа
;в интервала [a;b]
; 1 list ot perfect numbers -> foldl
;(define (foldl op bottom l)
;  (if (null? l) bottom
;          (foldl op (op bottom (car l)) (cdr l))))

;(define (filter p l)
;  (cond ((null? l) l)
;               ((p (car l)) (cons (car l) (filter p (cdr l))))
;               (else (filter p (cdr l)))))

;(define (accumulate op bottom a b term next) 
(define (sum-almoast-done a b)
  (define (generate-list from to lst)
    (cond ( (> from to) lst)
          ((done? from) (generate-list (+ from 1) to (cons from lst)))
          (else (generate-list (+ from 1) to lst)))
    )
  (define (do-ends from to lst)
  (cons (+ 1 (quotient (+ (car lst) to) 2)) (+ 1 (quotient (+ from (list-ref lst (- (length lst) 1))) 2))))
  (accumulate + 0 (car (do-ends a b (generate-list a b '()))) (cdr (do-ends a b (generate-list a b '()))) (lambda(x)(+ x 1)) next)
)

;working version
;-----------------------------------------------------------------------------------------------------------
(define (id x) x)
(define (sum-almoast-done2 a b)
  (define (find from next-value) (if(done? from) from (find (next-value from 1) next-value)))
  (accumulate + 0 (+ (quotient (+ a (find a +)) 2) 1) (- (quotient (+ b (find b -)) 2) 1) id next))
;-----------------------------------------------------------------------------------------------------------

;Задача 2: 
;Разглеждаме стекова машина, която представя паметта си като списък от числа и символи и приема списък от инструкции,
;които интерпретира по следния начин: - ако поредната инструкция е число или символ, то се добавя на върха на стека -
;ако поредната инструкция е функция, тя се прилага над всички числа в стека (допуска се, че функцията приема само един параметър),
;променяйки стойностите им в стека - част 1
;- ако поредната инструкция е наредена двойка от операция (двуместна функция) и число n,
;то горните две числа на стека се изваждат и обратно на върха на стека се записва резултат от прилагането на операцията над тях.
;Прилагането се повтаря до изчерпване на стека или достигане до символ, но не повече от n пъти. - всички останали инструкции се
;игнорират.

;Да се реализира функция run-machine, която връща като резултат списък, представящ паметта на машината след последователно
;обработване на всички инструкции. Първоначално машината се инициализира с празен стек.

;(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6))                       → (6 5 4 3 a 2 x 1)
;(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5))) → (45 a 2 x 1)

; пример1 ; stack(1) -> stack(x 1) -> stack (4 x 1) -> stack(a 4 x 1) -> stack( 25 16 9 a 4 x 1) -> (sqrt  stack(5 4 3 a 2 x 1))
; -> stack( 6 5 4 3 a 2 x 1); - easy

; пример 2: наредена двойка(op, n) - n - брой пъти, в които да се изпълни операцията
;(stack(6 5 4 3 a 2 x 1) -> (+ 2) -> (11 4 3 a 2 x 1) -> (+ 1) -> (15 3 a 2 x 1)-> (* 5)-> (45 a 2 x 1)-> (* 4) ->(* 45 a)->end
; stack(45 a 2 x 1) done
;(map f lst)

;(define (run-machine lst)
;  (define (cut lst)
;    (if(procedure? (car lst))
;       lst
;       (cut (cdr lst))
;       )
;    )
;  (define (fill lst) (if(or (number? (car lst)) (symbol? (car lst))) (cons (car lst) (fill (cdr lst))) '() )); нов лист
;   (mapp (car (cut lst)) (fill lst))
;)

; (list 1 'x 4 'a 9 16 25 sqrt 6) -> (1 (x ( a (9 (16 (25 ))))))
; (1 'x 4 'a 9 16 25)(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6))

;------------------- различни подходи към задачта ---------------------------
;(define (filter p? l)
;  (cond ((null? l) l)
;        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
;        (else (filter p? (cdr l))))


;(define (filter-cons lst indx)
;  (cond ((null? lst) '())
;        ((procedure? (car lst)) (cons (cons (car lst) indx) (filter-cons (cdr lst) (+ 1 indx))) )
;        ((pair? (car lst)) (cons (car lst) (filter-cons (cdr lst) (+ 1 indx))))
;        (else (filter-cons (cdr lst) (+ 1 indx)))))


;(define (run-machine lst)
;  (define execute indx)
;  (filter-cons lst 0)
;  (filter (lambda(x)(if(or (number? x) (symbol? x)) #t #f)) lst)
;)

;(define (foldr op nv l)
;  (if (null? l) nv
;          (op (car l) (foldr op nv (cdr l)))))
;

;working version
;------------------------------------------------------------------------------------------------
(define (mapp f lst)
  (cond ( (null? lst) '())
        ( (number? (car lst)) (cons (f (car lst) ) (mapp f (cdr lst))))
        ( else (cons (car lst) (mapp f (cdr lst))))
  )
)

(define (applyy op n lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) lst)
        ((zero? n)  (cons (car lst) (applyy op n (cdr lst))))
        ((and (number? (car lst)) (number? (car (cdr lst)) ) ) (applyy op
                                                                 (- n 1)
                                                                 (cons (op (car lst) (car (cdr lst))) (cdr (cdr lst)) ) )) 
        (else (cons (car lst) (applyy op n (cdr lst)) ))
        )
)

(define (foldl-better op bottom l)
  (cond ((null? l) bottom)
        ((procedure? (car l)) (foldl-better op (mapp (car l) bottom) (cdr l)))
        ((or (number? (car l)) (symbol? (car l))) (foldl-better op (op (car l) bottom) (cdr l)))
        (else (foldl-better op (applyy (car (car l)) (cdr (car l)) bottom ) (cdr l)))
        ;(else (foldl-better op (op (car l) bottom) (cdr l)))
        )
)

(define (run-machine lst)(foldl-better cons '() lst))
;-------------------------------------------------------------------------------------------------------

;lambda ver
;(define (run-machine-lambda lst)(foldl
;                                 (lambda)(x l)( cond ( (or (number? (car l)) (symbol? (car l)) ) (cons) )
;                                                   ( (procedure? (car l) ) (map (car l) l)
;                                                   )
;                                 '()
;                                 lst)))

;(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6))                       → (6 5 4 3 a 2 x 1)
;(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5))) → (45 a 2 x 1)

;Задача 3. (10 т.) Казваме, че един списък е подсписък на друг, ако елементите на първия списък се
;срещат непосредствено последователно във втория. Например, '(2 4) не е подсписък на '(1 2 3 4 5), но '(2 3 4) е.
;Казваме, че един списък от числа a се мажорира от списъка b, ако двата списъка са с еднаква дължина n и ai ≤ bi
;за всяко i ∈ [0; n). Списък от списъци ll наричаме мажорен, ако е вярно, че li се мажорира от подсписък на li+1
;за всеки два съседни списъка li и li+1 в ll.

;Да се реализира функция is-major?, която проверява дали даден списък от списъци от числа е мажорен.
;(is-major? '((1 3) (4 2 7) (2 5 4 3 9 12))) → #t
;(is-major? '((1 3) (4 2 7) (2 5 3 3 9 12))) → #f
; (1 3) -> (2 7)
; (4 2 7) -> (4 3 9) ->#t

;(op nv (car l))

; (foldl op bottom list)
;    bottom = #t
;    op = (or bottom (foldl ) 
;    (op bottom (car l)) -> списък
;

;(define (foldl op bottom l)
;  (if (null? l) bottom
;          (foldl op (op (car l) bottom) (cdr l))))

;(define (map f l)
;  (if (null? l) '()
;         (cons (f (car l)) (map f (cdr l))))

;subtask1: is-major? два списъка (1 3) (2 3 2 6) дали се мажорират
;идея: правя списък от наредени двойки (1 2) (3 3) (1 2) (3 7) (1 7) (3 6)
; [ (1 <= 2) и (3 <= 3 )] или [(1 <= 3) и (3 <= 2)] или ...
; [ #t and #t] or [#t and #f]
;сравняване на списъци с една и съща дължина

(define (zip l1 l2) (if(or (null? l1) (null? l2) ) '()
                          (cons (cons (car l1) (cons l2)) (zip (cdr l1) (cdr l2)))))

;(foldl and #t (map λ (zip l1 l2)))
; λ = (lambda(x)(if(> (car x) (cdr x)) #f #t))
; l2 = функция, която връща подсписък с дължината на l1 (take* n)
; аз всички подсписъци от l2
; foldl 


