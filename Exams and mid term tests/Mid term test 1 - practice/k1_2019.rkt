;метриката
;(define (prod l) (apply * l))        (define (sum l) (apply + l)) 
;(max-metric (list sum prod) '((0 1 2) (3 4 5) (1337 0))) → <sum>
;(max-metric (list car sum)  '((1000 -1000) (29 1) (42))) → <car>


;(define (max-metric lf ln) )


; “Ниво на влагане” на атом в дълбок списък наричаме броя пъти,
;който трябва да се приложи операцията car за достигане до атома.
;Да се реализира функция deep-repeat, която в подаден дълбок списък
;заменя всеки атом на ниво на влагане n с n негови повторения.
;Пример:
;(deep-repeat '(1 (2 3) 4 (5 (6)))) → (1 (2 2 3 3) 4 (5 5 (6 6 6)))

;(define (gen n x)
;  (if(zero? n) '() (cons x (gen (- n 1) x) ) ))
;
;(define (level lv lst bottom)
;  (cond ((null? lst) bottom)
;        ( (list? (car lst)) (level lv (cdr lst) (cons (level (+ 1 lv) (car lst) '() ) bottom) ))
;        ((= lv 1) (level lv (cdr lst) (cons (car lst) bottom)))
;        (else (level lv (cdr lst) (append (gen lv (car lst)) bottom) ) )
;  )
;)

;(define (reverse* lst)
;  (cond ((null? lst) '())
;        ((list? (car lst)) (cons (reverse* (cdr lst)) (reverse* (car lst))) )
;        (else (cons (reverse* (cdr lst)) (car lst)))
;  )
;)

;(define (deep-repeat lst)
;  (reverse (level 1 lst '())))

(define (take* n ll)
  (if(zero? n) '()
     (cons (car ll) (take* (- n 1) (cdr ll)) )
   )
)


(define (foldl op nv l)
  (if (null? l) nv
          (foldl op (op nv (car l)) (cdr l))))

(define (is-major-e l1 l2)
  (foldl (lambda(bottom x)(and bottom x)) #t (map (lambda(x y)(if(<= x y) #t #f)) l1 l2) ))

(define (is-major-n l1 l2)
  (if( = (length l2) (length l1) ) (is-major-e l1 l2)
     (or (is-major-e l1 (take* (length l1) l2)) (is-major-n l1 (cdr l2))))
  )

(define (find-major ll)
  (if (or (null? ll) (null? (cdr ll))) #t
      (and (is-major-n (car ll) (car (cdr ll)) ) (find-major (cdr ll)) )
      )
 )

(define (is-major? ll)
  (find-major ll))

;(define (range n)
;  (if (zero? n) '() (cons n (range (sub1 n))))
;)

;(define (deep-repeat xs)
;  (define (dr ys n)
;   (cond
;     [(null? ys) ys]
;      [(list? (car ys)) (cons (dr (car ys) (add1 n)) (dr (cdr ys) n))]
;      [else (append (map (lambda (x) (car ys)) (range n)) (dr (cdr ys) n))]
;    )
;  )
;  (dr xs 1)
;)

;(equal? (deep-repeat '(1 (2 3) 4 (5 (6)))) '(1 (2 2 3 3) 4 (5 5 (6 6 6))))
