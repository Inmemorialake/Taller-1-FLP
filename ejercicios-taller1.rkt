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

