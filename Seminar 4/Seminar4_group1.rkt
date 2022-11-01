;Да се напише функция (zipWith f lst1 lst2), която връща списъка, получен от прилагането на f върху
;съответните елементи на двата списъка lst1 и lst2.
;(zipWith + '(1 2 3 4) '(7 10 12)) -> '(8 12 15)

(define (foldl op bottom l)
  (if(null? l) bottom
     (foldl op (op (car l) bottom) (cdr l))))

(define (zip l1 l2)
  (if(or (null? l1) (null? l2) ) '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define (zipWith f l1 l2) (foldl (lambda(p bottom)(cons (f (car p) (cdr p)) bottom) ) '() (zip l1 l2)))

;Да се напише функция (sorted? lst), която проверява дали списък е сортиран в ненамаляващ ред.

(define (sorted? l) ( if( number? ( foldl (lambda (x bottom) (if (and (number? bottom) (> x bottom)) x #f)) (car l) (cdr l))) #t #f))

;(sorted? '(1 3 4 5 10))
;(sorted? '(1 3 4 1 10))

;Да се напише функция (uniques lst), която оставя само уникалните стойности в даден списък.
;Можете да проверявате за еднаквост с equal? за най-сигурно.

(define (filter p? l)(cond ( (null? l) '() )
                            ( (p? (car l) ) (cons (car l) (filter p? (cdr l) )) )
                            ( else (filter p? (cdr l)) )
                      )
)

(define (map* f l)
  (if(null? l) '() (cons (f (car l)) (map* f (cdr l)) )))

;(uniques '(1 2 2 "iei" 1 3 "iei" 'oops)) -> '(1 2 "iei" 3 'oops) ; подредбата в резултата няма значение
;
(define (uniques lst)(foldl (lambda(x bottom)(cons x (filter (lambda(y)(not (equal? x y))) bottom))) '() lst))

;Да се напише функция (insert val lst), която вмъква стойността val на правилното място в сортирания
;в ненамаляващ ред списък lst:

;(insert 5 '(1 4 10)) ;-> '(1 4 5 10)
;(insert 12 '(1 4 10)) -> '(1 4 10 12))

(define (insert y lst)
  (cond ((> (car lst) y) (cons y lst))
        (( > y (car (reverse lst))) (append lst (list y)) )
        (else (reverse (foldl (lambda(x bottom)(if( < y x) (cons x (cons y bottom)) (cons x bottom)) ) '() lst) ))
        )
  )

;(equal? (insert 5 '(1 4 10)) '(1 4 5 10))
;(equal? (insert 12 '(1 4 10)) '(1 4 10 12))

;Да се напише функция (insertion-sort lst), която прави точно това, което подсказва името ѝ:

(define (insertion-sort l)(foldl (lambda(x bottom) (insert x bottom)) (list (car l)) (cdr l) ))

;Отворен числов интервал (a;b) се описва с наредената двойка (a . b). Да се напише функция longest-interval-subsets,
;която по даден списък от интервали il връща нов списък, който съдържа всички интервали от il, които са подинтервали
;на най-дългия интервал в списъка. Бонус: Функцията longest-interval-subsets да връща подинтервалите подредени в нарастващ
;ред по началната си точка.

;(longest-interval-subsets '((24 . 25) (90 . 110) (0 . 100) (10 . 109) (1 . 3) (-4 . 2)))
;-> ((0 . 100) (1 . 3) (24 . 25))

(define (longest-interval-subsets lst) (let ((max-int
                                             (foldl (lambda(x bottom)(if(> (- (cdr x) (car x)) (- (cdr bottom) (car bottom)) )
                                                                        x bottom))
                                                    (car lst) (cdr lst) ))) 
                                              (filter (lambda(intr)( if(and (>= (car intr) (car max-int))
                                                                            (<= (cdr intr) (cdr max-int)) ) #t #f)) lst)))

;(let ([x 5] [y 3]) x)