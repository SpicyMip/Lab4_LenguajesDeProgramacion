#lang scheme (current-namespace (make-base-namespace))

;; Retorna la transpuesta de lst, utilizando recursion de cola
;;
;; lst: lista desde la cual se leeran los datos para añadirlos a final
;; final: lista que se retornara con la transpuesta de lst
(define (reverse lst final)
    (if (null? lst)
        final
        (reverse(cdr lst) (cons (car lst)final))))

;; Remueve x de la lista ls utilizando recursion
;;
;; x: elemento que sera removido de ls
;; ls: lista que tiene el dato que se removera
(define remv
  (lambda (x ls)
    (cond ((null? ls) '())
          ((eqv? x (car ls)) (remv x (cdr ls)))
          (else (cons (car ls) (remv x (cdr ls)))))))

;; Crea una lista que contiene los numeros de 0 hasta x-1
;;
;; x: la cantidad de elementos que tendra la lista
(define lista_n
  (lambda (x)
    (let lista((i (- x 1))(final '()))
      (cond ((< i 0) final)
            (else (lista(- i 1)(cons i final)))))))

;; Retorna la cantidad de elementos de la lista ls utilizando recursividad
;;
;; ls: lista con elementos
(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ 1 (length (cdr ls))))))

;; Funcion auxiliar que es llamada por umbral_cola para realizar la recursividad de cola del umbral en el modo #\M, acronimo de umbral cola M
;;
;; lista: lista con todos los datos a filtrar
;; umbral: numero el cual se comparara con todos de la lista
(define (ucM lista umbral)
  (let ucMc((lista1 lista)(final '())(i 0))
    (cond ((null? lista1) final)
          ((< umbral (car lista1)) (ucMc(cdr lista1)(cons i final)(+ i 1)))
          ((> umbral (car lista1)) (ucMc(cdr lista1) final (+ i 1)))
          (else (ucMc(cdr lista1) final (+ i 1))))))

;; Funcion auxiliar que es llamada por umbral_cola para realizar la recursividad de cola del umbral en el modo #\m, acronimo de umbral cola modo m
;;
;; lista: lista con todos los datos a filtrar
;; umbral: numero el cual se comparara con todos de la lista
(define (ucmm lista umbral)
  (let ucmc((lista1 lista)(final '())(i 0))
    (cond ((null? lista1) final)
          ((> umbral (car lista1)) (ucmc(cdr lista1)(cons i final)(+ i 1)))
          ((< umbral (car lista1)) (ucmc(cdr lista1) final (+ i 1)))
          (else (ucmc(cdr lista1) final (+ i 1))))))

;; Funcion auxiliar que es llamada por umbral_simple para realizar la recursividad simple del umbral en el modo #\M, acronimo de umbral simple M
;;
;; lista: lista con todos los datos a filtrar
;; umbral: numero el cual se comparara con todos de la lista
;; n: contador para ir construyendo la lista
(define (usM lista umbral n)
  (cond ((null? lista)'())
        ((< umbral (car lista)) (cons n (usM(cdr lista)umbral (+ n 1))))
        (else (usM(cdr lista) umbral (+ n 1)))))

;; Funcion auxiliar que es llamada por umbral_simple para realizar la recursividad simple del umbral en el modo #\m, acronimo de umbral simple modo m
;;
;; lista: lista con todos los datos a filtrar
;; umbral: numero el cual se comparara con todos de la lista
;; n: contador para ir construyendo la lista
(define (usmm lista umbral n)
  (cond ((null? lista)'())
        ((> umbral (car lista)) (cons n (usmm(cdr lista)umbral (+ n 1))))
        (else (usmm(cdr lista) umbral (+ n 1)))))

;; Funcion auxiliar que es llamada por query para realizar la operacion op
;;
;; lista: lista que contienen numeros
;; op: operacion que se le realizara a la lista ubicada en pos
;; params: parametros necesarios para aplicar la operacion op
(define (query2 lista op params)
  (cond ((= 1 op) (umbral_cola lista (car params) (car(cdr params))))
        ((= 2 op) (modsel_cola lista (car params) (car(cdr params))))
        ((= 3 op) (estables lista (car params)(car(cdr params))(car(cdr(cdr params)))))))


;; Funciones Especificadas en la tarea

;; Retorna una lista con los elementos desde 0 hasta n-1 sin los elementos de la lista dada
;;
;; lista: lista de elementos que no estaran en la lista final
;; n: numero dado para que la funcion vea hasta que numero añadir
(define(inverso lista n)
  (let inv((lista1 lista)(final (lista_n n)))
    (cond ((null? lista1)final)
          (else (inv(cdr lista1)(remv(car lista1)final))))))

;; Funcion que llama a las funciones auxiliares usM y usmm dependiendo el modo del umbral y retorna una lista de posiciones donde se encuentran los numeros
;;
;; lista: lista de numero a ser evaluado
;; umbral: numero el cual se comparara con todos de la lista
;; tipo: puede ser #\M que determinara todos los mayores de la lista o #\m todos los menores
(define (umbral_simple lista umbral tipo)
  (cond ((eqv? tipo #\M) (usM lista umbral 0))
        ((eqv? tipo #\m) (usmm lista umbral 0))))

;; Funcion que llama a las funciones auxiliares ucM y ucmm dependiendo el modo del umbral y retorna una lista de posiciones donde se encuentran los numeros
;;
;; lista: lista de numero a ser evaluado
;; umbral: numero el cual se comparara con todos de la lista
;; tipo: puede ser #\M que determinara todos los mayores de la lista o #\m todos los menores
(define(umbral_cola lista umbral tipo)
  (cond ((eqv? tipo #\M) (reverse (ucM lista umbral)'()))
        ((eqv? tipo #\m) (reverse(ucmm lista umbral)'()))))

;; funcion que aplica una funcion lambda a los elementos seleccionados de la lista utilizando recursion de cola
;;
;; lista: lista con todos los elementos
;; seleccion: lista con la posicion de los elementos a aplicarle la funcion lambda
;; f: funcion lambda
(define (modsel_simple lista seleccion f)
  (let modsel((lista1 lista)(seleccion1 (sort seleccion <))(i 0)(funcion (eval f)))
    (cond ((null? lista1)'())
          ((null? seleccion1)(append (list (car lista1))(modsel (cdr lista1)seleccion1(+ i 1)funcion)))
          ((= i (car seleccion1)) (append (list (funcion (car lista1)))(modsel (cdr lista1)(cdr seleccion1)(+ i 1)funcion)))
          (else (append (list (car lista1))(modsel (cdr lista1)seleccion1(+ i 1)funcion))))))

;; funcion que aplica una funcion lambda a los elementos seleccionados de la lista utilizando recursion simple
;;
;; lista: lista con todos los elementos
;; seleccion: lista con la posicion de los elementos a aplicarle la funcion lambda
;; f: funcion lambda
(define (modsel_cola lista seleccion f)
  (let modsel((lista1 lista)(seleccion1 (sort seleccion <))(final '())(i 0)(funcion (eval f)))
    (cond ((null? lista1)final)
          ((null? seleccion1)(modsel (cdr lista1)seleccion1(append final (list(car lista1)))(+ i 1)funcion))
          ((= i (car seleccion1))(modsel (cdr lista1)(cdr seleccion1)(append final (list(funcion(car lista1))))(+ i 1)funcion))
          (else (modsel (cdr lista1) seleccion1 (append final (list(car lista1)))(+ i 1) funcion)))))

;; Retorna una lista con la cantidad de elementos que son mayores al umbral al aplicarles fM y menores al umbral al aplicarles fm
;;
;; lista: lista con los numeros a comparar 
;; umbral: numero con el cual se compararan todos los elementos de la lista luego de aplicar la funcion lambda respectiva
;; fM: funcion lambda a aplicar a los mayores que el umbral
;; fm: funcion lambda a aplicar a los menores que el umbral
(define (estables lista umbral fM fm)
  (let((x (length (umbral_cola (modsel_cola lista (lista_n (length lista)) fM) umbral #\M)))
       (y (length (umbral_cola (modsel_cola lista (lista_n (length lista)) fm) umbral #\m))))
    (list x y)))

;; Funcion multifuncional que aplica el umbral, modsel o estables dependiendo cual se pida a una lista en concreto desde la lista de listas
;;
;; lista: lista de listas que contienen numeros
;; pos: posicion de la lista de numeros que se utilizara
;; op: operacion que se le realizara a la lista ubicada en pos
;; params: parametros necesarios para aplicar la operacion op
(define (query lista pos op params)
  (let query1((lista1 lista)(i 0))
    (cond((= pos i) (query2 (car lista1) op params))
         (else (query1 (cdr lista1) (+ i 1))))))