(defun area_rectangulo (base altura)
  (* base altura))

(defun area_cuandrado (lado)
  (* lado lado))

(defun sumar_listas (l1 l2)
  (mapcar '+ l1 l2))

(defun sumar_matrices (matriz1 matriz2)
  (mapcar 'sumar_listas matriz1 matriz2))

(defun cociente_resto (numero1 numero2)
  (list (floor numero1 numero2)
	(- numero1 (* numero2 (floor numero1 numero2)))))

(defun sumar_matrices_version_lambda (matriz1 matriz2)
  (mapcar (lambda (m1 m2)
            (mapcar '+ m1 m2))   
	  matriz1 matriz2))

(defun num_posicion (lista pos)
  (append
   (list (car (append
               (butlast lista (- (length lista) pos))
               (cdr (nthcdr pos lista)))))
   (list (cadr (append
		(butlast lista (- (length lista) pos))
		(cdr (nthcdr pos lista)))))
   (append (list (nth pos lista)) (nthcdr 2 (append
					     (butlast lista (- (length lista) pos))
					     (cdr (nthcdr pos lista)))))))

(defun pasa-1-a-3 (lista)
  (append (list (nth 1 lista) (nth 2 lista) (nth 0 lista)) (cdddr lista)))

; revisar - no funciona - pasar el elemento 1 al la posicion n
(defun pasa1-a-n (lista n)
  (append
   (reverse (nthcdr (- (length lista) n) (reverse (cdr lista))))
   (list (nth 0 lista))
   (nthcdr n lista)))

; dame_elementos_entre_n_m - dar los elementos que se encuentran entre dos posiciones de una lista
(defun elementos_entre_n_m (n m lista)
  (reverse (cdr (reverse (cdr (reverse (member m (reverse (member n lista)))))))))

(defun elementos_entre_n_m_2 (n m lista)
  (reverse (member m (reverse (member n lista)))))

(defun cuadrado_numero (numero)
  (* numero numero))

(defun num_posicion_2 (pos lista)
  (setf (nth pos lista) (nth 0 lista))
  lista)

(defun traspuesta_matriz (m)
  (eval 
   (append '(mapcar 'list)
	   (mapcar (lambda (v) (list 'quote v))
		   m))))

(defun multiplicar_listas (l1 l2)
  (mapcar '* l1 l2))

(defun multiplicar_matriz (m1 m2)
  (mapcar 'multiplicar_listas m1 (traspuesta_matriz m2)))

; una lista si tiene mas de 10 elementos decir 'esta lista es muy larga'
(defun comprobar_lista (lista)
  (if (> (length lista) 10)
      '(esta lista es muy larga)
      '(ok)))


; tenemos una lista '(A B C D E F) y queremos devolverla así ((a b c)(b c d)(c d e)(d e f))
(defun dividir_lista (lista)
  (mapcar (lambda (x y z) (list x y z)) lista (cdr lista) (cdr (cdr lista))))

(defun dividir_lista_2 (lista)
  (mapcar 'list lista (cdr lista) (cdr (cdr lista))))


(defun producto_escalar (matriz k)
  (mapcar (lambda (fila) 
	    (mapcar (lambda (x) (* k x)) fila)) 
	  matriz))

(defun suma_fila_columna (matriz)
  (list (mapcar (lambda (x) (apply '+ x)) matriz) 
	(apply 'mapcar '+ matriz)))

(defun suma_fila (matriz)
  (mapcar (lambda (x) (apply '+ x)) 
	  matriz))

(defun suma_columna (matriz)
  (apply 'mapcar '+ matriz))

(defun producto_vector_matriz (vector matriz)
  (mapcar (lambda (fila) (mapcar '* vector fila)) 
	  matriz))

(defun producto_punto_por_fila_con_vector (vector matriz)
  (mapcar (lambda (x) (apply '+ x)) (mapcar (lambda (fila) (mapcar '* fila vector)) matriz)))

;(mapcar 'list lista (cdr lista) (cdr (cdr lista)))
; tenemos una lista '(A B C D E F) y queremos devolver los elementos de n en n

; -----------------------------------------------------------------------------------------------------------
; () es igual a NIL, ya que se trata de una lista vacía, así que, si queremos saber si un lista tiene elementos o no
; podemos preguntas si tiene elementos o no, es decir, if ()
; numberp - (if numberp v)
; symbolp - (symbolp '()) -> T | (symbolp 'A) -> T
; listp - (lisp lista) -> T
; NULL - (null '()) -> T | (null 0) -> NIL
; (and (lisp lista) (null lista))
; (and (> a 3) (< a 10))
; (or (eq (color 'blanco) (eq color 'negro)))
; (equal '(a b) '(a b) -> T | (equal '(a b) '(a b c)) -> NIL)
; (not (equal '(a b) '(a b))) -> NIL
; cuando una funcion me va a devolver un predicado debemos nombrardo asi: funcionp

(defun tipo-simple (x)
  (if (null x)
      'nil
      (if (numberp x)
	  'NUM
          (if (symbolp x)
              'SYM
              (if (listp x)
		  'list
		  'otro)))))

(defun inc-si-num (x)
  (if (numberp x)
      (1+ x)
      x))

(defun nil-a (x defecto)
  (if (null x)
      x
      defecto))

; función que incrementa 1+ si el valor contenido dentro de la lista se trata de un número
(defun inc-numeros (lista)
  (mapcar (lambda (x) (if (numberp x) (1+ x) x)) lista))

(defun etiqueta (lista)
  (mapcar (lambda (x) 
      (if (numberp x)
        (list 'NUM x)
          (if (listp x)
            (list 'LIST x)
              (if (symbolp x)
                (list 'SYM x)
                  (if (null x)
                    (list 'nil x)
                    'OTRO))))) lista))

; convertir números negativos a 0 dentro de una matriz
(defun negativos-a-cero (matriz)
  (mapcar (lambda (fila) 
    (mapcar (lambda (x) 
      (if (< x 0)
        '0
        x)) fila)) matriz))

; suma cada numero de cada fila ignorando aquellos valores que no son números
(defun suma-filas-ingnorando-no-num (matriz)
  (mapcar (lambda (fila)
    (if (null fila)
      0
      (apply '+ 
        (mapcar (lambda (x)
          (if (numberp x) x 0)) 
        fila)))) 
  matriz))

(defun centrar-filas (matriz)
  (mapcar (lambda (fila) 
    (if (not (numberp (car fila)))
      fila
      (if (numberp (car fila))
          (cons '0 (nthcdr 1 fila)) 
          x)))
  matriz))

; min -> min 1 0 2 7
; max -> max 1 0 2 7
; (apply 'min '(1 2 0 4))

(defun comparar-min-max-lista (lista)
   (if (null lista)
      nil
      (nth (- (length lista)
              (length
               (member nil
                       (mapcar (lambda (yy)
                                 (member t yy))
                               (mapcar (lambda (x)
                                         (mapcar (lambda (y)
                                                   (> x y))
                                                 lista))
                                       lista)))))
           lista)))

; cuenta cuantos numeros menores hay comparadose entre si dentro de la lista
(defun cuantos-menores-que-cada (lista)
(mapcar (lambda (fila) (apply '+ (mapcar (lambda (valor) (if valor 1 0)) fila)))
  (mapcar (lambda (y)
        (mapcar (lambda (x) (< x y)) 
        lista)) 
      lista)))

(defun frecuencia-por-elemento (lista)
  (mapcar (lambda (fila) (apply '+ 
    (mapcar (lambda (valor) (if valor 1 0)) fila)))
  (mapcar (lambda (x) (mapcar (lambda (y) (eq x y)) lista))
    lista))
  
  
  
  )

  
(defun fibonacci (n)
  (if (< n 2)
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))
    ))

; recursividad de cola
; trace funcion
; hacerlo recursivo - calcular la longitud de una lista sin utilizar length
(defun longitud-lista-recursivo (lista)
  (if (null lista)
    0
    (1+ (longitud-lista-recursivo (cdr lista)))))

(defun longitud-lista-mapcar (lista)
  (apply '+
    (mapcar (lambda (x) 1)
    lista)))

(defun sumar-recursivo (lista)
(if (null lista)
  0
  (+ (car lista) (sumar-recursivo (cdr lista)))))

(defun mi-last (lista)
(if (null lista)
  nil
  (if (null (cdr lista))
    (car lista)
    (mi-last (cdr lista)))))

(defun penultimo-elemento (lista)
(if (null lista)
  nil
  (if (null (cdr (cdr lista)))
    (car lista)
    (penultimo-elemento (cdr lista)))))

(defun un-elemento? (lista)
(if (null lista)
  nil
  (if (null (cdr lista))
    t
    nil)))

(defun contiene-elemento? (n lista)
(if (null lista)
  nil
  (if (eq n (car lista))
    t
    (contiene-elemento? n (cdr lista)))))

(defun inc-todos (lista)
(if (null lista)
  nil
  (if (numberp (car lista))
    (cons (1+ (car lista)) (inc-todos (cdr lista)))
    (cons (car lista) (inc-todos (cdr lista))))))

(defun mi-reverse (lista)
(if (null lista)
  nil
  (if (null lista)
    nil
    (append (mi-reverse (cdr lista)) (list (car lista))))))

(defun aplanar-lista (lista)
(if (null lista)
  nil
  (if (listp lista)
    (append (car lista) (aplanar-lista (cdr lista)))
    (cons (car lista) (aplanar-lista (cdr lista))))))

(defun potencia-recursiva (base exponente)
  (if (equal exponente 0)
    1
    (* base (potencia-recursiva base (1- exponente)))))

(defun sumar-hasta (n)
  (if (equal n 0)
    0
    (+ n (sumar-hasta (1- n)))))

; dado un numero y una lista, una funcion que me diga cuantas veces esta el numero en la lista

(defun numero-aparicion (n lista)
(if (null lista)
  0
  (if (equal n (car lista))
    (1+ (numero-aparicion n (cdr lista))) 
    (numero-aparicion n (cdr lista)))))

; automata celular unidimensional

(defun shift-left (lista)
  (if (null lista)
    nil
    (if (null (cdr lista))
    '(0)
    (cons (car (cdr lista)) (shift-left (cdr lista))))))

(defun shift-right (lista)
  (if (null lista)
    nil
    (if (null (cdr lista))
      '(0)
      (append (shift-right (cdr lista)) (list (car lista))))))

(defun shift-left-wrap (lista)
  (if (null lista)
    nil
    (shift-left-wrap-aux (cdr lista) (car lista))))

(defun shift-left-wrap-aux (lista primero)
  (if (null lista)
    (list primero)
    (cons (car lista) (shift-left-wrap-aux (cdr lista) primero))))



(defun shift-right-wrap (lista)
  (if (null lista)
    lista
    (if (null (cdr lista))
    lista
    (shift-right-wrap-aux lista))))

(defun shift-right-wrap-aux (lista)
  (if (null (cdr lista))
    (list (car lista))
    (append (shift-right-wrap-aux (cdr lista)) (list (car lista)))))

(defun contar-elementos (lista)
  (if (null lista)
    0
    (if (symbolp lista)
      1
      (+ (contar-elementos (car lista)) (contar-elementos (cdr lista))))))

; atom reconoce numeros y letras
; subst '0 '_
; subs 1 '@
; funcion para aplanar de forma recursiva

(defun aplanar-lista-recursivo (lista)
(if (null lista)
  nil
  (if (atom (car lista))
    (cons (car lista) (aplanar-lista-recursivo (cdr lista)))
    (append (aplanar-lista-recursivo (car lista)) (aplanar-lista-recursivo (cdr lista))))))


; funcion que te diga el numero maximo de veces maximo que se repite un simbolo

(defun numero-maximo-apariciones (n lista)
  (if (null lista)
    0
    (if (equal n (car lista))
      (numero-maximo-apariciones-aux n lista 0 0)
      (numero-maximo-apariciones n (cdr lista)))))

(defun numero-maximo-apariciones-aux (n lista actual maximo)
  (if (null lista)
    (max maximo actual)
    (if (equal n (car lista))
    (numero-maximo-apariciones-aux n (cdr lista) (1+ actual) maximo)
      (if (> actual maximo)
        (numero-maximo-apariciones-aux n (cdr lista) 0 actual)
        (numero-maximo-apariciones-aux n (cdr lista) actual maximo)))))
;(defun ev_auto pos_init regla iter)
; le debemos dar una regla de las 256 posible, y un numero de pasos, y partir de la regla calcular la evolucion hasta
; el instante dado

(defun recorrer-lista (lista)
  (if (null lista)
    nil
    (cons (car lista) (recorrer-lista (cdr lista)))))

;----------------------------------------------------------------------------------------------------------
; AUTOMATA CELULAR UNIDIMENSIONAL
; ---------------------------------------------------------------------------------------------------------- 

; función utilizada para convertir un número decimal a binario

(defun convertir-num-binario (num)
  (if (equal num 0)
    nil
    (append (convertir-num-binario (car (list (floor num 2)))) (list (- num (* 2 (floor num 2)))))))

; función utilizada para convertir el número binario que le pasamos a un número binario de 8 bits

(defun binario-8bits (lista)
  (if (< (length lista) 8)
      (binario-8bits (cons 0 lista))
      lista))

; función utilizada para calcular el estado de un vecindario, es decir, le pasamos un vecindario de 3 bits y nos da el valor decimal

(defun valor-vecindario (vecindario)
  (if (null vecindario)
    0
    (+ (* (car vecindario) (potencia 2 (1- (length vecindario)))) (valor-vecindario (cdr vecindario)))))

; función utilizada para calcular la potencia, le pasamos la base y el exponente para calcular la operación

(defun potencia (base exponente)
  (if (equal exponente 0)
    1
    (* base (potencia base (1- exponente)))))

; función utilizada para obtener el estado de un vecindario, le pasamos un regla binaria y el vecindario y obtendremos el valor
; de ese vecindario según la regla

(defun obtener-estado-vecindario (regla vecindario)
  (if (null vecindario)
    0
    (nth (valor-vecindario vecindario) (reverse regla))))

; función utilizada para construir los vecindarios de 3 bits que serán utilizados para comprobarlos con la regla binaria.
; le pasamos una lista inicial para situarnos y construir la lista de vecindarios. 
; debemos pasarle un indice de 0 para comience a construir desde el comienzo

(defun construir-vecindarios (lista indice)
  (if (< indice (length lista))
      (cons (construir-celdas lista indice) (construir-vecindarios lista (1+ indice)))
      nil))

; función complementaria con la anterior, mediante el indice que se le pasa a la funcion anterior calculamos el indice anterior, central y posterior
; según la posición concreta de este para obtener cada vecindario.
; el calculo de los vecindarios es:
  ; izquierda -> indice - 1 mod 8 (length lista)
  ; cental -> indice mod 8
  ; derecha -> indice + 1 mod 8
; el valor obtenido de los modulos se les realiza un nth para obtener el valor de esa celda dentro de la lista
; el módulo es 8 ya que es la longitud de la lista
; Un ejemplo de la primera iteración es:
  ; -1 mod 8 -> 7
  ; 0 mod 8 -> 0
  ; 1 mod 8 -> 1
; los numeros obtenidos son a aquellas celdas a las que debemos realizar el nth para obtener su valor 

(defun construir-celdas (lista indice)
  (list 
  (nth (- (1- indice) (* (length lista) (floor (1- indice) (length lista)))) lista)
  (nth (- indice (* (length lista) (floor indice (length lista)))) lista)
  (nth (- (1+ indice) (* (length lista) (floor (1+ indice) (length lista)))) lista)))

; función utilizada para construir una sola generacion, utilizando la función construir-vecindarios donde le pasamos una lista inicial y un indice,
; a partir de la siguiente, a la cual le pasamos el listado de vecindarios de 3 bits y una regla binaria construimos una sola generación

(defun construir-generacion (regla lista)
  (if (null lista)
    nil
    (cons (obtener-estado-vecindario regla (car lista)) (construir-generacion regla (cdr lista)))))

; función utilizada para construir el automata celular unicelular, a partir de la lista inicial, la regla binaria y el número de pasos
; realizamos la ejecución pero solo se mostrarán secuencias de 0s y 1s.

(defun ev_auto (lista regla pasos)
  (if (> pasos 0)
    (cons (construir-generacion regla (construir-vecindarios lista 0)) (ev_auto (construir-generacion regla (construir-vecindarios lista 0)) regla (1- pasos)))
    nil))

; subst '0 '_
; subs 1 '@

; fución utilizada para mostrar el autómata celular unicelular pero intercambiando los 0s y 1s por _ y @.

(defun mostrar-automata (matriz)
  (mapcar (lambda (fila)
    (mapcar (lambda (x) 
      (if (equal x 1)
        '@ '_)) 
    fila))
  matriz))

;----------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------

; (cons (ev_auto (construir-generacion regla (construir-vecindarios lista 0)) regla (1- pasos)))
; nth (- (1- indice) (* (length lista) (floor (1- indice) (length lista)))) lista
(defun inc-indice (num)
  (if (numberp num)
    (1+ num)
    0))

(defun negativos-a-cero (matriz)
  (mapcar (lambda (fila) 
    (mapcar (lambda (x) 
      (if (< x 0)
        '0
        x)) fila)) matriz))


(defun convertir-num-binario-acb (num)
  (if (equal num 0)
    nil
    (append (convertir-num-binario-acb (car (list (floor num 2)))) (list (- num (* 2 (floor num 2)))))))

(defun binario-10bits (lista)
  (if (< (length lista) 9)
      (binario-10bits (cons 0 lista))
      lista))

(defun valor-vecindario-2d (vecindario)
  (if (null vecindario)
    0
    (valor-vecindario (aplanar-lista-recursivo vecindario))))

(defun obtener-valor-vecindario-2d (vecindario)
  (if (equal vecindario 0)
  0
  (1+ (obtener-valor-vecindario-2d (floor vecindario 2)))))

(defun obtener-estado-vecindario-2d (regla vecindario)
  (if (null vecindario)
    0
    (nth (1- (obtener-valor-vecindario-2d (valor-vecindario-2d vecindario))) (reverse regla))))

(defun construir-vecindarios-2d (lista x y)
  (if (and (< x (length lista)) (< y (length lista)))
    ))

(defun construir-celdas-acb (lista y)
  (list
    (list 
      (nth (modulo (1- (nth (modulo (1- y) (length lista))))) (length lista))
      (nth (modulo (nth (modulo (1- y) (length lista)))) (length lista))
      (nth (modulo (1+ (nth (modulo (1- y) (length lista))))) (length lista)))
    (list
      (nth (modulo (1- (nth (modulo y (length lista))))) (length lista))
      (nth (modulo (nth (modulo y (length lista)))) (length lista))
      (nth (modulo (1+ (nth (modulo y (length lista))))) (length lista)))
    (list
      (nth (modulo (1- (nth (modulo (1+ y) (length lista))))) (length lista))
      (nth (modulo (nth (modulo (1+ y) (length lista)))) (length lista))
      (nth (modulo (1+ (nth (modulo (1+ y) (length lista))))) (length lista)))
  ))

(defun modulo (n1 n2)
  (- n1 (* n2 (floor n1 n2))))

(defun construir-celdas (lista indice)
  (list 
  (nth (- (1- indice) (* (length lista) (floor (1- indice) (length lista)))) lista)
  (nth (- indice (* (length lista) (floor indice (length lista)))) lista)
  (nth (- (1+ indice) (* (length lista) (floor (1+ indice) (length lista)))) lista)))
; pasar a decimal el vecindario y a partir de ahi saber que valor le pertenece dentro de la regla

; (print (make-sequence 'list 10 :initial-element 0))
;(append (convertir-num-binario (car (list (floor num 2)))) (list (- num (* 2 (floor num 2)))))
;(defun cociente_resto (numero1 numero2)
; utilizar para cada vecindario de 3 bits pasarlo a decimal y hacerle el nth con la lista (conversada - reverse) 
; de la regla 
;  (list (floor numero1 numero2)
;	(- numero1 (* numero2 (floor numero1 numero2)))))
; automata celular bidimensional - el juego de la vida - matrices, necesito 2^9  512 reglas para construir un
; automata celular bidimensional, los vecinos son areas de 3*3, y en caso de que los vecinos queden fuera de la
; matriz se debe completar con el resto de vecinos de otras puntas 



