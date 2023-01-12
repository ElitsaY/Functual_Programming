; Двоични дървета:
;-----------------
; Двоично дърво дефинираме по следния начин:
; 1) '() е двоично дърво.
; 2) (root left right) е двоично дърво, точно когато
;   left и right са двоични дървета,
;   а root е просто стойността в корена.

; Дефинираме си няколко функции за работа с дървета:

(define root car) ;car

(define left cadr) ; (car (cdr x))

(define right caddr); (car (cdr (cdr x)))

; Проверяваме по дефиницията
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (left t))
           (tree? (right t)))))

(define empty? null?)

; Едно дърво е листо ако има вида (root '() '()),
; тоест текущия връх няма наследници
(define (leaf? t)
  (and (empty? (left t))
       (empty? (right t))))

; Асоциативни списъци:
;---------------------
; асоциативен списък ще наричаме списък от двойки
; от вида (key . value)

; още познато като map или dictionary

; ето и някой основни функции:

; По функция и списък от ключове, правим
; асоциативен списък с елементи от вида (key . fn(key))
(define (make-alist fn keys)
  (map (lambda (key)
         (cons key (fn key)))
       keys))

; map f l

; Вече можем и да добавяме елементи в нашия списък
(define (add-assoc key value alist)
  (cons (cons key value)
        alist))

; Ще е хубаво да имаме и функции с които да вземем
; само ключовете или само стойностите на списъка
(define (alist-keys alist)
  (map car alist))

; ----------------------------Задачи-----------------------------

; За двоични дървета:
;--------------------

; Пример:
(define t
  '(1 (2 () ())
      (3 ()
         (4 () ()))))

; Изглежда така:
;
;   1
;  / \
; 2   3
;      \
;       4


; 1. Намира броя на листата в tree.
(define (count-leaves tree)
  (cond ((empty? tree) 0)
        ((leaf? tree)  1)
        (else(+ (count-leaves (left tree) ) (count-leaves (right tree))))
     )
)

; Конструктор
(define (make-tree root left right)
  (list root left right))

; 2. Връща ново дърво, в което f е приложена над
; всеки връх от tree.
(define (map-tree f tree)
 (if (empty? tree) '()
   (make-tree (f (root tree)) (map-tree f (left tree)) (map-tree f (right tree)))
   )
)

; 3. Връща списък от всички върхове на разстояние n от
; корена на tree.
(define (level n tree)
  (cond ((empty? tree) '())
        ((zero? n) (cons (root tree) '()))
        (else (append (level (- n 1) (left tree)) (level (- n 1) (right tree)) ))
     )
)

;(level 2 t)

; Обхождане на дърво, функциите да връщат списък
; от върховете в реда на обхождането им:
;---------------------------------------
; 4. корен-ляво-дясно
; (pre-order t) -> (1 2 3 4)

(define (pre-order tr)
    (if(empty? tr) '()
       (cons (root tr) (append (pre-order (left tr)) (pre-order (right tr))))
     )
)

; 5. ляво-корен-дясно
; (in-order t) -> (2 1 3 4)

(define (in-order tr)
  (if(empty? tr) '()
        (append (in-order (left tr)) (cons (root tr)(in-order (right tr)) ))
     )
)

; 6. ляво-дясно-корен
; (post-order t) -> (2 4 3 1)
(define (post-order tr)
   (if(empty? tr) '()
        (append (post-order (left tr)) (append (post-order (right tr)) (list (root tr))))
     )
 )

; 7. Обръща 2ката от наследници на всеки връх
; (root left right) -> (root right left)
(define (flip-tree tr)
  (if (empty? tr) '()
      (make-tree (root tr) (flip-tree (right tr)) (flip-tree (left tr)) )
  )
)

; За асоциативни списъци:
;------------------------
; 8.Връща списък от стойностите на асоциативен списък
(define (alist-values alist) (map cdr alist))
;(alist-values '((1 . 2) (3 . 4) (5 . 6)) 


(define (filter)

; 10. По даден ключ изтрива първата съответстваща двойка
; със същия ключ
(define (del-assoc key alist)
  (filter)
  )
; 11. Връща списък от списъци (result . args),
; където args са точно тези елементи x от lst,
; за които f(x) = result
(define (group-by* f lst) 'undefined)