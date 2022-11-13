;Задача 1:
(define (accumulate-i op bottom from to term next)
  (if(> from to) bottom
     ( accumulate-i op (op bottom (term from)) (next from) to term next)
     )
  )

(define (isPrime? x)
  (accumulate-i (lambda(bottom y)(and bottom y))
                 #t
                 2
                 (- x 1)
                 (lambda(y)(if(zero? (remainder x y)) #f #t) )
                 (lambda(y)(+ y 1))
   )
 )

(define (id x) x)

(define(grow n)
  (accumulate-i
               (lambda(bottom x) (if(and (isPrime? x) (zero? (remainder n x))) (* bottom x) bottom))
               n
               2
               (- n 1)
               id
               (lambda(x)(+ x 1))
    )
)

;Задача 3:
(define (extr f a b) (if(f a b) #t #f))

(define (min* a b) (if(extr < a b) a b) )
(define (max* a b) (if(extr > a b) a b) )

(define (map* f l1 l2 rule)
  (cond ((null? l1) '())
        ((and (extr < (f (car l1)) (f (car l2)))
              (extr < (f (car l2)) (min* (car l1) (car l2))))
               (cons (f (car l1)) (map* f (cdr l1) (cdr l2) 1)))
        ((and (extr > (f (car l2)) (f (car l1)))
              (extr > (f (car l1)) (max* (car l1) (car l2))))
               (cons (f (car l2)) (map* f (cdr l1) (cdr l2) 2)))
        ((= rule 1) (cons (f (car l1)) (map* f (cdr l1) (cdr l2) 1)) )
        (else (cons (f (car l2)) (map* f (cdr l1) (cdr l2) 2)))
     )
 )

(define (selective-map f l1 l2)
 (cons (f (car l1)) (map* f (cdr l1) (cdr l2) 1))
  )

;example
;(selective-map (lambda(x)(- (* x x) 2)) '(2 -1 -2 -1 4 0 1 -4) '(10 2 -3 2 -1 1 3 5))
;expexted; '(2 -1 7 2 -1 -2 -1 23)

;Задача 2:
(define (no-common-div a b)
  (accumulate-i 
               (lambda(bottom x)
                 (cond ((not (isPrime? x)) (and bottom #t))
                       ((and (isPrime? x) (and (zero? (remainder a x)) (zero? (remainder b x)))) #f)
                       ( else (and bottom #t ) )
                    )
                )
               #t
                2
                a
                id
                (lambda(x)(+ x 1))
   )
)

(define (max-unitary n)
  (accumulate-i
               (lambda(bottom x)
                  (if (and (zero? (remainder n x)) (no-common-div x (quotient n x))) x bottom) )
                1
                1
                (- n 1)
                id
                (lambda(x)(+ x 1))
   )
)
;(max-unitary 60)

;Задача 4:

(define (foldl op bottom lst)
  (if(null? lst) bottom
      (foldl op (op bottom (car lst)) (cdr lst))
  )
)

(define (contains? x l2)
  (if(null? l2) #f
    (if(= (car l2) x) (or #t (contains? x (cdr l2))) (or #f (contains? x (cdr l2))) )
  )
)

(define (find-common l1 l2)
  (foldl
         (lambda(bottom x)(if(contains? x l2) (cons x bottom) bottom))
         '()
         l1)
 )

(define (len* l1)
  (if(null? l1) 0
     (+ 1 (len* (cdr l1)) )
   )
)

(define (preferred-device l1 l2)
  (define(coverage a b) (/ a b))
  (foldl
         (lambda(bottom x)
           (let* ((net (find-common x l1))) 
           (if(and (>= (len* net) 2) (> (coverage (len* net) (len* l1)) (coverage (len* bottom) (len* l1))))
              net
              bottom)
           ))
        '()
         l2
  )
)

;example (preferred-device '(2 4 5 17 30) '((1 3 5 7 9 20) (1 2 3 4 5 7 12 14 30) (2 4 17)) )
;(preferred-device '(2 4 5 17 30) '((1 3 5 7 9 20) (1 2 3 7 12 14 30) (2 4 17)) )
;(preferred-device '(2 4 5 17 30) '((1 3 5 7 9 20) (1 3 7 12 14 30) (2 19 9)) )
;(find-common '(2 4 5 17 30) '(1 2 3 4 5 7 12 14 30))

;Бонус задача: - не е тествана -
;няма примерче :(

;намира броя на мрежите, с които е съвместимо устройство
(define (comp l1 ll)
  (foldl
        (lambda(bottom x) (if(>= (len* (find-common x l1)) 2) (+ 1 bottom) bottom))
        0
        ll
   )
)

;sum покритие
(define (sum-coverage dev ll)
  foldl(
        (lambda(bottom x)
          (if(>= (comp dev x) 2)
             (+ bottom (coverage (len* (find-common x dev)) (len* dev)))
             bottom)
          )
        0
        ll)
)

;намирам устройството
;пазя усторйството
(define (get-device ll1 ll2)
  foldl(
        (lambda(bottom dev)
          (let((comp-dev (comp dev ll2)))
          (if(> (len* comp-dev (len* bottom)))
                comp-dev
                (if(and (= comp-dev (len* bottom)) (> (sum-coverage comp-dev ll2) (sum-coverage bottom ll2)))
                   comp-dev
                   bottom)
                )
            )
           )
        '()
        ll1
   )
)

(define (preferredDeviceForAll ll1 ll2) (preferred-device (get-device ll1 ll2) ll2) )

;още тестове
;(no-common-div 15 5)