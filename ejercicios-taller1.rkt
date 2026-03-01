#lang eopl

;;; Steven Fernando Aragón Álvarez -2418804
;;; Manuela Martínez Moncada - 2375458
;;; Andrés Gerardo González Rosero - 2416541

;;--------------------------------------------------------------------------------------;;

;; cartesian-product : L1 L2 -> L
;; Propósito: Recibe dos listas de símbolos L1 y L2 sin repeticiones
;; y retorna una lista de tuplas que representan el producto cartesiano L1 x L2.
;;
;; <lista> := ()
;;          | (<valor-scheme> <lista>)
;;
;; Función auxiliar: combina un elemento x con cada elemento de L2
(define combinar-con
  (lambda (x L2)
    (if (null? L2)
        '()
        (cons (list x (car L2))
              (combinar-con x (cdr L2))))))

;; Función auxiliar: concatena dos listas
(define mi-append
  (lambda (L1 L2)
    (if (null? L1)
        L2
        (cons (car L1)
              (mi-append (cdr L1) L2)))))

(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        '()
        (mi-append (combinar-con (car L1) L2)
                   (cartesian-product (cdr L1) L2)))))

;; Pruebas
(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))

;;--------------------------------------------------------------------------------------;;

;; mapping : F L1 L2 -> L
;; Propósito: Recibe una función unaria F y dos listas de números
;; L1 y L2 de igual tamaño. Retorna los pares (a b) donde a ∈ L1,
;; b ∈ L2 y se cumple F(a) = b.
;;
;; <lista-num> := ()
;;             | (<número> <lista-num>)
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
(mapping (lambda (d) (* d 2)) '(1 2 3) '(2 4 6))
(mapping (lambda (d) (* d 3)) '(1 2 2) '(2 4 6))

;;--------------------------------------------------------------------------------------;;

;; inversions : L -> N
;; Propósito: Recibe una lista de números L y retorna la cantidad
;; de inversiones, es decir pares (i,j) donde i < j pero L[i] > L[j].
;;
;; <lista-num> := ()
;;             | (<número> <lista-num>)
;;
;; Auxiliar: cuenta cuántos elementos del resto son menores que x
(define contar-menores
  (lambda (x L)
    (if (null? L)
        0
        (if (> x (car L))
            (+ 1 (contar-menores x (cdr L)))
            (contar-menores x (cdr L))))))

(define inversions
  (lambda (L)
    (if (null? L)
        0
        (+ (contar-menores (car L) (cdr L))
           (inversions (cdr L))))))

;; Pruebas
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))

;;--------------------------------------------------------------------------------------;;