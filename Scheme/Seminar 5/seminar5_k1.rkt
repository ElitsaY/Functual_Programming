
; 5. Намира броя на елементите в дълбокия списък lst.
; Тоест lst може да има произволни нива на вложеност.
;(define (count-atoms lst) 'undefined)
;()

; 6. Връща наредена двойка (fst . snd), където
; fst са елементите за които p? е истина
; и snd са тези за които p? е лъжа
;(define (partition* p? lst) (cons (filter p? lst) (filter (not p?) lst)))

(define (foldr op bottom l)
  (if (null? l) bottom
          (op (car l) (foldr op bottom (cdr l)))))

(define (foldl op bottom l)
  (if (null? l) bottom
          (foldl op (op (car l) bottom) (cdr l))))

; '(4 7 8 2 11 13) -> ((1 3 7 11 13)(4 8 2)) ->  (cons 1
(define (partition* p? lst) (foldl
                             (lambda (x bottom) (if(p? x) (cons (cons x (car bottom)) (cdr bottom)) (cons (car bottom) (cons x (cdr bottom)))))
                             (cons '() '())
                             lst))

; (partition* odd? '(1 3 4 7 8 2 11 13))


; 5. Намира броя на елементите в дълбокия списък lst.
; Тоест lst може да има произволни нива на вложеност.
;((1 2 3 (4))(5 ((6))) 7) -> 7

(define (count-atoms lst) (foldl (lambda(x bottom) (if(list? x) (+ bottom
                                                                   (count-atoms x)) (+ bottom 1)))
                                 0
                                 lst))

;(count-atoms '((1 2 3 (4))(5 ((6))) 7))

;(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6))                       → (6 5 4 3 a 2 x 1)
;(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5))) → (45 a 2 x 1)

;(define (foldl1 op l)
;  (foldl op (car l) (cdr l)))

(define (accumulate-i op bottom from to term next)
  (if(> from to) bottom
  (accumulate-i op (op bottom (term from)) (next from) to term next)))


(define (run-machine lst)
  (foldl (lambda(x bottom)
                (cond ((or (number? x) (symbol? x)) (cons x bottom))
                       ((procedure? x) (map
                                           (lambda(y)(if(number? y) (x y) y))
                                            bottom))
                       ((pair? x)(accumulate-i (lambda(spis y)
                                                 (if(and (number? y) (number? (car spis)) )
                                                    (cons ((car x) y)))
                                               bottom 1 (cdr x) - (lambda (x)(+ x 1)))) 
                       (else bottom)
                  )
          )
         '()
        lst)
 )
;
;(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6))
;(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5)))

;(is-major? '((1 3) (4 2 7) (2 5 4 3 9 12))) → #t
;(is-major? '((1 3) (4 2 7) (2 5 3 3 9 12))) → #f

(define (map* f l1 l2)
  (if(or (null? l1) (null? l2)) '()
     (cons (f (car l1) (car l2)) (map* f (cdr l1) (cdr l2))))
  )

(define (is-major-two? l1 l2)(foldl (lambda(x bottom)(and x bottom)) #t (map* <= l1 l2)))

(define (take* lst n)
  if(zero? n) '() (cons (car l) (take* (cdr lst) (- n 1))))

;(4 2 7) (2 5 4 3 9 12)
; (4 2 7 ) -> (2 5 4)
; (4 2 7) -> (5 4 3)

;(define (is-major? ll) (foldl )