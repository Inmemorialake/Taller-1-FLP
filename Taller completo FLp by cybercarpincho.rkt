#lang eopl


;;====================================================
;; Taller 1
;; Docentes: Prof. Robinson Duque 
;; Estudiante 1: Steven Fernando Aragon Alvarez - 2418804-3743
;; Estudiante 2: manuela bb
;; Estudiante 3: gerard
;;====================================================
;;            FUNCIONES AUXILIARES
;;====================================================

;; append-aux :
;; Proposito:
;; L1 x L2 -> L : Concatena dos listas L1 y L2,
;;                retornando una nueva lista con todos
;;                los elementos de L1 seguidos de L2.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
;;
(define append-aux
  (lambda (L1 L2)
    (if (null? L1)
        L2
        (cons (car L1) (append-aux (cdr L1) L2)))))

;; Pruebas
(append-aux '(1 2 3) '(4 5 6))   ; (1 2 3 4 5 6)
(append-aux '() '(1 2))           ; (1 2)
(append-aux '(a b) '())           ; (a b)
(append-aux '(a) '(b c))          ; (a b c)

;;----------------------------------------------------

;; multiplo5? :
;; Proposito:
;; N -> Boolean : Predicado que verifica si un número
;;               entero N es múltiplo de 5.
;;
;; <número> := (cualquier número entero)
;;
(define multiplo5?
  (lambda (n)
    (letrec ((resto
              (lambda (x)
                (cond
                  [(< x 0) (resto (+ x 5))]   
                  [(< x 5) x]                  
                  [else (resto (- x 5))]))))  
      (= (resto n) 0))))

;; Pruebas
(multiplo5? 10)    ; #t
(multiplo5? 90)    ; #t
(multiplo5? 7)     ; #f
(multiplo5? 0)     ; #t

;;----------------------------------------------------

;; mayor5? :
;; Proposito:
;; N -> Boolean : Predicado que verifica si un número N
;;               es estrictamente mayor que 5.
;;
;; <número> := (cualquier número)
;;
(define mayor5?
  (lambda (n)
    (> n 5)))

;; Pruebas
(mayor5? 6)     ; #t
(mayor5? 10)    ; #t
(mayor5? 5)     ; #f
(mayor5? 3)     ; #f

;;----------------------------------------------------

;; last-elem :
;; Proposito:
;; L -> valor : Retorna el último elemento de una lista
;;              no vacía L. Función auxiliar para palindrome?.
;;
;; <lista> := (<valor-de-scheme>)
;;         := (<valor-de-scheme> <lista>)
;;
(define last-elem
  (lambda (L)
    (if (null? (cdr L))
        (car L)
        (last-elem (cdr L)))))

;; Pruebas
(last-elem '(1 2 3))      ; 3
(last-elem '(a b c d))    ; d
(last-elem '(r a d a r))  ; r

;;----------------------------------------------------

;; remove-last :
;; Proposito:
;; L -> L' : Retorna la lista L sin su último elemento.
;;           Función auxiliar para palindrome?.
;;
;; <lista> := (<valor-de-scheme>)
;;         := (<valor-de-scheme> <lista>)
;;
(define remove-last
  (lambda (L)
    (if (null? (cdr L))
        '()
        (cons (car L) (remove-last (cdr L))))))

;; Pruebas
(remove-last '(1 2 3))   
(remove-last '(a b c d)) 
(remove-last '(5))       

;;----------------------------------------------------

;; pair-with-all :
;; Proposito:
;; elem x L -> L' : Crea una lista de pares (elem x)
;;                  para cada elemento x en L.
;;                  Función auxiliar para cartesian-product.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
;;
(define pair-with-all
  (lambda (elem L)
    (if (null? L)
        '()
        (cons (list elem (car L))
              (pair-with-all elem (cdr L))))))

;; Pruebas
(pair-with-all 'a '(x y z))  ; ((a x) (a y) (a z))
(pair-with-all 1  '(2 3))    ; ((1 2) (1 3))

;;----------------------------------------------------

;; count-greater :
;; Proposito:
;; x x L -> number : Cuenta cuántos elementos de la
;;   lista L son estrictamente menores que x 
;;   Función auxiliar para inversions.
;;
;; <lista-números> := ()
;;                 := (<número> <lista-números>)
;;
(define count-greater
  (lambda (x L)
    (if (null? L)
        0
        (if (> x (car L))
            (+ 1 (count-greater x (cdr L)))
            (count-greater x (cdr L))))))

;; Pruebas
(count-greater 5 '(3 1 6 2))  ; 3  
(count-greater 2 '(1 3 4))    ; 1  
(count-greater 1 '(1 2 3 4))  ; 0
;;----------------------------------------------------

;; zip-add :
;; Proposito:
;; L1 x L2 -> L : Suma elemento a elemento dos listas de
;;                números del mismo tamaño.
;;                Función auxiliar para pascal.
;;
;; <lista-números> := ()
;;                 := (<número> <lista-números>)
;;
(define zip-add
  (lambda (L1 L2)
    (if (null? L1)
        '()
        (cons (+ (car L1) (car L2))
              (zip-add (cdr L1) (cdr L2))))))

;; Pruebas
(zip-add '(1 2 3) '(4 5 6))   ; (5 7 9)
(zip-add '(0 1 3) '(1 3 1))   ; (1 4 4)
(zip-add '(0 0 0) '(1 1 1))   ; (1 1 1)

;;====================================================
;;  SECCIÓN IL 1.1.1 — 
;;====================================================

;;----------------------------------------------------
;; Ejercicio 1 
;;----------------------------------------------------

;; invert :
;; Proposito:
;; L x P -> L' : Recibe una lista L de pares (x y) y un
;;   predicado P. Retorna una lista con los pares invertidos
;;   (y x) únicamente cuando ambos elementos del par
;;   satisfacen el predicado P.
;;
;; <lista-pares> := ()
;;               := (<par> <lista-pares>)
;; <par>         := (<valor> <valor>)
;;
(define invert
  (lambda (L P)
    (if (null? L)
        '()
        (let ((x (car  (car L))) 
              (y (cadr (car L))))
          (if (and (P x) (P y))
              (cons (list y x) (invert (cdr L) P)) ; si es verdadero se agrega el par invertido y continua la recursion
              (invert (cdr L) P))))))               ; si es falso solo continua la recursion

;; Pruebas
(invert '((3 2) (4 2) (1 5) (2 8)) even?)    ; ((2 4) (8 2))
(invert '((5 9) (10 90) (82 7)) multiplo5?)   ; ((90 10))
(invert '((6 9) (10 90) (82 7)) odd?)         ; ()

;;----------------------------------------------------
;; Ejercicio 2  
;;----------------------------------------------------

;; down :
;; Proposito:
;; L -> L' : Retorna la lista L con cada elemento
;;           envuelto en un nivel adicional de paréntesis.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
;;
(define down
  (lambda (L)
    (if (null? L)
        '()
        (cons (list (car L)) (down (cdr L))))))

;; Pruebas
(down '(1 2 3))                         ; ((1) (2) (3))
(down '((una) (buena) (idea)))          ; (((una)) ((buena)) ((idea)))
(down '(un (objeto (mas)) complicado))  ; ((un) ((objeto (mas))) (complicado))

;;----------------------------------------------------
;; Ejercicio 3  
;;----------------------------------------------------

;; list-set :
;; Proposito:
;; L x N x X x P -> L' : Retorna la lista L con el elemento
;;   en la posición N (indexando desde cero) reemplazado por X,
;;   sólo si el elemento original cumple el predicado P. Si no
;;   cumple P, la lista se retorna sin cambios.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
;;
(define list-set
  (lambda (L n x P)
    (if (null? L)
        '()
        (if (= n 0)
            (if (P (car L))
                (cons x (cdr L))
                L)
            (cons (car L)
                  (list-set (cdr L) (- n 1) x P))))))

;; Pruebas
(list-set '(5 8 7 6) 2 '(1 2) odd?)         ; (5 8 (1 2) 6)
(list-set '(5 8 7 6) 2 '(1 2) even?)        ; (5 8 7 6)
(list-set '(5 8 7 6) 3 '(1 5 10) mayor5?)   ; (5 8 7 (1 5 10))
(list-set '(5 8 7 6) 0 '(1 5 10) mayor5?)   ; (5 8 7 6)

;;----------------------------------------------------
;; Ejercicio 4  
;;----------------------------------------------------

;; filter-in:
;; Proposito:
;; P x L -> L' : Retorna una lista con los elementos de L
;;               que satisfacen el predicado P.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
;;
(define filter-in
  (lambda (P L)
    (if (null? L)
        '()
        (if (P (car L))
            (cons (car L) (filter-in P (cdr L)))
            (filter-in P (cdr L))))))

;; Pruebas
(filter-in number? '(a 2 (1 3) b 7))
; (2 7)
(filter-in symbol? '(a (b c) 17 foo))
; (a foo)
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))
; ("univalle" "racket" "flp")

;;----------------------------------------------------
;; Ejercicio 5  
;;----------------------------------------------------

;; palindrome? :
;; Proposito:
;; palabra -> Boolean : Determin recursivamente si una
;;   lista de símbolos es palíndromo, es decir, si se lee
;;   igual de izquierda a derecha que de derecha a izquierda.
;;
;; <palabra> := ()
;;           := (<símbolo>)
;;           := (<simbolo> <palabra-interior> <simbolo>)
;;
(define palindrome?
  (lambda (palabra)
    (if (or (null? palabra) (null? (cdr palabra)))
        #t
        (if (eqv? (car palabra) (last-elem palabra))
            (palindrome? (remove-last (cdr palabra)))
            #f))))

;; Pruebas
(palindrome? '(r a d a r))      ; #t
(palindrome? '(n e u q u e n))  ; #t
(palindrome? '(h o l a))        ; #f
(palindrome? '())                ; #t
(palindrome? '(a))               ; #t

;;----------------------------------------------------
;; Ejercicio 6  
;;----------------------------------------------------

;; swapper :
;; Proposito:
;; E1 x E2 x L -> L' : Retorna la lista L con cada
;;   ocurrencia de E1 reemplazada por E2 y cada
;;   ocurrencia de E2 reemplazada por E1.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
;;
(define swapper
  (lambda (E1 E2 L)
    (if (null? L)
        '()
        (cond
          [(eqv? (car L) E1) (cons E2 (swapper E1 E2 (cdr L)))] ; si el primer elemento es igual a E1, pone E2 en la lista y sigue la recursion
          [(eqv? (car L) E2) (cons E1 (swapper E1 E2 (cdr L)))] ; si el primer elemento es igual a E2, pone E1 en la lista y sigue la recursion
          [else (cons (car L) (swapper E1 E2 (cdr L)))]))))     ; si ni lo uno ni lo otro, pues sigue y ya.

;; Pruebas
(swapper 'a 'd '(a b c d))             ; (d b c a)
(swapper 'a 'd '(a d () c d))          ; (d a () c a)
(swapper 'x 'y '(y y x y x y x x y))  ; (x x y x y x y y x)

;;----------------------------------------------------
;; Ejercicio 7 
;;----------------------------------------------------

;; cartesian-product :
;; Proposito:
;; L1 x L2 -> L : Retorna una lista de tuplas que
;;   representan el producto cartesiano entre L1 y L2.
;;   Cada par (a b) cumple a pertenece L1 y b pertenece L2.
;;
;; <lista-símbolos> := ()
;;                  := (<símbolo> <lista-símbolos>)
;;
(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        '()
        (append-aux
          (pair-with-all (car L1) L2)
          (cartesian-product (cdr L1) L2)))))

;; Pruebas
(cartesian-product '(a b c) '(x y))
; ((a x) (a y) (b x) (b y) (c x) (c y))
(cartesian-product '(p q r) '(5 6 7))
; ((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))

;;----------------------------------------------------
;; Ejercicio 8  
;;----------------------------------------------------

;; mapping :
;; Proposito:
;; F x L1 x L2 -> L : Retorna una lista de pares (a b)
;;   donde a ∈ L1, b ∈ L2, y se cumple que F(a) = b.
;;   L1 y L2 deben tener el mismo tamaño.



;; ALERTA ALERTA ALERTA: NO ESTOY SEGURO DE ESTA GRAMATICA !!!


;; <lista-números> := ()
;;                 := (<número> <lista-números>)
;;
(define mapping
  (lambda (F L1 L2)
    (if (null? L1)
        '()
        (if (= (F (car L1)) (car L2))
            (cons (list (car L1) (car L2))
                  (mapping F (cdr L1) (cdr L2)))
            (mapping F (cdr L1) (cdr L2))))))

;; Pruebas
(mapping (lambda (d) (* d 2)) '(1 2 3) '(2 4 6))   ; ((1 2) (2 4) (3 6))
(mapping (lambda (d) (* d 3)) '(1 2 2) '(2 4 6))   ; ((2 6))
(mapping (lambda (d) (* d 2)) '(1 2 3) '(3 9 12))  ; ()

;;----------------------------------------------------
;; Ejercicio 9  (4.5 pts)
;;----------------------------------------------------

;; inversions :
;; Proposito:
;; L -> number : Determina el número de inversiones de
;;   la lista L. Una inversión es un par de posiciones
;;   (i, j) tal que i < j pero L[i] > L[j].
;;
;; <lista-números> := ()
;;                 := (<número> <lista-números>)
;;
(define inversions
  (lambda (L)
    (if (null? L)
        0
        (+ (count-greater (car L) (cdr L))
           (inversions (cdr L))))))

;; Pruebas
(inversions '(2 3 8 6 1))  ; 5
(inversions '(1 2 3 4))    ; 0
(inversions '(3 2 1))      ; 3

;;----------------------------------------------------
;; Ejercicio 10  (4.5 pts)
;;----------------------------------------------------

;; balanced-parentheses? :
;; Proposito:
;; L -> Boolean : Determina si una lista de símbolos que
;;   representan paréntesis está correctamente balanceada.
;;   Los elementos de L son el símbolo 'O para paréntesis
;;   de apertura y 'C para paréntesis de cierre.
;;   Implementación recursiva con acumulador de conteo.
;;   Retorna #t si balanceado, #f en caso contrario el contador representa la siguiente logica, si ve un parentesis de apertura suma uno
;;   si ve uno de ceirre resta uno, si este contador llega a ser negativo en algun momento significa que la lista no esta balanceada.
;;
;; <lista-parens> := ()
;;               := ('|(| <lista-parens>)
;;               := ('|)| <lista-parens>)
;;
(define balanced-parentheses?
  (lambda (L)
    (letrec
      ((bal-aux
        (lambda (lst count)
          (cond
            [(< count 0) #f]
            [(null? lst) (= count 0)]
            [(eqv? (car lst) 'O)
             (bal-aux (cdr lst) (+ count 1))]
            [else
             (bal-aux (cdr lst) (- count 1))]))))
      (bal-aux L 0))))

;; Pruebas con 'O para abrir y 'C para cerrar
;; (()())  → O O C O C C
(balanced-parentheses?
  (list 'O 'O 'C 'O 'C 'C))   ; #t

;; ((())) → O O O C C C
(balanced-parentheses?
  (list 'O 'O 'O 'C 'C 'C))   ; #t

;; ()) → O C C
(balanced-parentheses?
  (list 'O 'C 'C))            ; #f

;; vacía
(balanced-parentheses? '())   ; #t

;;----------------------------------------------------
;; Ejercicio 11  
;;----------------------------------------------------

;; zip :
;; Proposito:
;; F x L1 x L2 -> L : Retorna una lista donde la
;;   posición n-ésima es el resultado de aplicar la función
;;   binaria F a los elementos n-ésimos de L1 y L2.
;;   L1 y L2 deben tener el mismo tamaño.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
;;
(define zip
  (lambda (F L1 L2)
    (if (null? L1)
        '()
        (cons (F (car L1) (car L2))
              (zip F (cdr L1) (cdr L2))))))

;; Pruebas
(zip + '(1 4)     '(6 2))      ; (7 6)
(zip * '(11 5 6)  '(10 9 8))   ; (110 45 48)

;;----------------------------------------------------
;; Ejercicio 12  
;;----------------------------------------------------

;; filter-acum :
;; Proposito:
;; a x b x F x acum x filter -> valor : Aplica la función
;;   binaria F a todos los enteros en el intervalo [a, b]
;;   que satisfacen el predicado filter, acumulando el
;;   resultado en acum. Retorna el valor final de acum.
;;
;; <número> := (cualquier número entero)
;;
(define filter-acum
  (lambda (a b F acum filter)
    (if (> a b)                                          ;esto hace que pare y retorne acum cuando ya se termino el rango [a,b]
        acum
        (if (filter a)                                   
            (filter-acum (+ a 1) b F (F acum a) filter) ; si "a" cumple la condicion de filter entonces se aumenta en 1 para "avanzar" y se le aplica F al acumulador
            (filter-acum (+ a 1) b F acum filter)))))   ; si no cumple, simplemente avanza
 
;; Pruebas
(filter-acum 1 10 + 0 odd?)   ; 25  (1+3+5+7+9)
(filter-acum 1 10 + 0 even?)  ; 30  (2+4+6+8+10)

;;----------------------------------------------------
;; Ejercicio 13  (5 pts)
;;----------------------------------------------------

;; operate :
;; Proposito:
;; lrators x lrands -> valor : Aplica sucesivamente las
;;   funciones binarias en lrators (lista de n funciones)
;;   a los valores en lrands (lista de n+1 números),
;;   de izquierda a derecha.
;;   Ejemplo: (operate (list f1 f2) '(a b c))
;;            => (f2 (f1 a b) c)
;;
;; <lista-funciones> := (<función-binaria>)
;;                   := (<función-binaria> <lista-funciones>)
;; <lista-números>   := (<número> <número>)
;;                   := (<número> <lista-números>)
;;
(define operate
  (lambda (lrators lrands)
    (if (null? lrators)
        (car lrands)
        (operate (cdr lrators)
                 (cons ((car lrators) (car lrands) (cadr lrands))
                       (cddr lrands))))))

;; Pruebas
(operate (list + * + - *) '(1 2 8 4 11 6))  ; 102
(operate (list *) '(4 5))                    ; 20


;;====================================================
;;  SECCIÓN IL 1.1.2
;;====================================================

;;----------------------------------------------------
;; Ejercicio 14  
;;----------------------------------------------------

;; path :
;; Proposito:
;; N x BST -> L : Retorna la lista con la ruta desde
;;   la raíz hasta el nodo que contiene N en el árbol
;;   binario de búsqueda BST, indicada con los símbolos
;;   'left y 'right. Si N es la raíz retorna lista vacía.
;;
;; <árbol-binario> := ()
;;                 := (<número> <árbol-binario> <árbol-binario>)
;;
(define path
  (lambda (n BST)
    (if (null? BST)
        '()
        (let ((raiz (car   BST))
              (izq  (cadr  BST))
              (der  (caddr BST)))
          (cond
            [(= n raiz) '()]
            [(< n raiz) (cons 'left  (path n izq))]
            [else        (cons 'right (path n der))])))))

;; Pruebas
(path 17 '(14 (7 () (12 () ()))
              (26 (20 (17 () ()) ())
                  (31 () ()))))
; (right left left)

(path 7 '(8 (3 (1 () ())
               (6 (4 () ()) (7 () ())))
             (10 () (14 (13 () ()) ()))))
; (left right right)

;;----------------------------------------------------
;; Ejercicio 15  
;;----------------------------------------------------

;; count-odd-and-even :
;; Proposito:
;; arbol -> L : Toma un árbol binario y retorna una lista
;;   (pares impares) con la cantidad de números pares e
;;   impares presentes en el árbol.
;;
;; <árbol-binario> := ()
;;                 := (<número> <árbol-binario> <árbol-binario>)
;;
(define count-odd-and-even
  (lambda (arbol)
    (if (null? arbol)
        (list 0 0)
        (let ((raiz       (car   arbol))
              (cnt-izq    (count-odd-and-even (cadr  arbol)))
              (cnt-der    (count-odd-and-even (caddr arbol))))
          (let ((pares   (+ (car  cnt-izq)
                            (car  cnt-der)
                            (if (even? raiz) 1 0)))
                (impares (+ (cadr cnt-izq)
                            (cadr cnt-der)
                            (if (odd?  raiz) 1 0))))
            (list pares impares))))))

;; Pruebas
(count-odd-and-even
  '(14 (7 () (12 () ()))
       (26 (20 (17 () ()) ())
           (31 () ()))))
; (4 3)   → pares: 14,12,26,20  | impares: 7,17,31

(count-odd-and-even
  '(8 (3 (1 () ()) (6 (4 () ()) (7 () ())))
      (10 () (14 (13 () ()) ()))))
; (5 4)   → pares: 8,6,4,10,14  | impares: 3,1,7,13

;;----------------------------------------------------
;; Ejercicio 16  
;;----------------------------------------------------

;; hanoi :
;; Proposito:
;; N x origen x auxiliar x destino -> L : Retorna la lista
;;   de movimientos para resolver las Torres de Hanoi con
;;   N discos. Cada movimiento es una lista (desde hacia).
;;
;; <N>       := (entero positivo)
;; <símbolo> := (nombre de torre: A, B, C, etc.)
;;
(define hanoi
  (lambda (n origen auxiliar destino)
    (if (= n 1)
        (list (list origen destino))
        (append-aux
          (hanoi (- n 1) origen destino auxiliar)
          (append-aux
            (list (list origen destino))
            (hanoi (- n 1) auxiliar origen destino))))))

;; Pruebas
(hanoi 1 'A 'B 'C)  ; ((A C))
(hanoi 2 'A 'B 'C)  ; ((A B) (A C) (B C))
(hanoi 3 'A 'B 'C)  ; ((A C) (A B) (C B) (A C) (B A) (B C) (A C))

;;----------------------------------------------------
;; Ejercicio 17  (8 pts)
;;----------------------------------------------------

;; coin-change :
;; Proposito:
;; monto x monedas -> number : Calcula el número total de
;;   formas distintas de obtener exactamente el monto dado
;;   usando las denominaciones de monedas disponibles.
;;   Implementación recursiva.
;;
;; <monto>   := (entero no negativo)
;; <monedas> := ()
;;            := (<número> <monedas>)
;;
(define coin-change
  (lambda (monto monedas)
    (cond
      [(= monto 0) 1]
      [(or (< monto 0) (null? monedas)) 0]
      [else
       (+ (coin-change (- monto (car monedas)) monedas)
          (coin-change monto (cdr monedas)))])))

;; Pruebas
(coin-change 5  '(1 5))      ; 2
(coin-change 5  '(1 2 5))    ; 4
(coin-change 10 '(2 5 3 6))  ; 5

;;----------------------------------------------------
;; Ejercicio 18  
;;----------------------------------------------------

;; pascal :
;; Proposito:
;; N -> L : Retorna la fila N del triángulo de Pascal.
;;   La fila 1 es (1). Cada fila N se calcula sumando
;;   pares adyacentes de la fila N-1, extendida con un
;;   cero en cada extremo.
;;   Ayuda: filaN = zip-add((0 . fila(N-1)) , (fila(N-1) 0))
;;
;; <N> := (entero positivo)
;;
(define pascal
  (lambda (N)
    (if (= N 1)
        '(1)
        (let ((fila-anterior (pascal (- N 1))))
          (zip-add
            (cons 0 fila-anterior)
            (append-aux fila-anterior (list 0)))))))

;; Pruebas
(pascal 1)  ; (1)
(pascal 2)  ; (1 1)
(pascal 3)  ; (1 2 1)
(pascal 4)  ; (1 3 3 1)
(pascal 5)  ; (1 4 6 4 1)