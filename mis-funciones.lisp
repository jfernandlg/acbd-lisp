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

(defun construir-vecindarios-sin-indice (lista)
  (dividir_lista (construir-celdas-sin-indice lista)))

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

(defun construir-celdas-sin-indice (lista)
  (mapcar (lambda (x) x) 
    (append (last lista) lista (list (car lista)))))

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


(defun ev_auto-sin-indice (lista regla pasos)
  (if (> pasos 0)
    (cons (construir-generacion regla (construir-vecindarios-sin-indice lista)) (ev_auto (construir-generacion regla (construir-vecindarios-sin-indice lista)) regla (1- pasos)))
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

; append (last (list car))

;----------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------


; función para construir los vecindario 3 x 3 de cada uno de los 9 bits de la lista que le pasamos, debemos recorrer la matriz para construir
; cada fila de 3 bits, es por ello que utilizamos la función construir-celdas-acb para obtener el vecindario completo junto a la función
; dividir-lista-recurisivo para dividir el vecindario completo en una matriz 3 x 3.
(defun construir-vecindarios-2d-pers (lista x y)
(let* ((dimensiones (tam-tablero (obtener-tams-tablero lista 1)))
      (ancho (car dimensiones))
      (alto (cadr dimensiones))
      (matriz (dividir-lista-recurisivo lista ancho)))

      (if (eq y alto)
        nil
        (if (< x (1- ancho))
          (cons (construir-celdas-acb (dividir-lista-recursivo lista ancho) x y) (construir-vecindarios-2d lista (1+ x) y))
          (cons (construir-celdas-acb (dividir-lista-recursivo lista ancho) x y) (construir-vecindarios-2d lista 0 (1+ y)))))))



(defun construir-celdas-acb (matriz x y ancho alto)
  (list
    (list 
      (nth (modulo (1- x) ancho) (nth (modulo (1- y) alto) matriz))
      (nth (modulo x ancho) (nth (modulo (1- y) alto) matriz))
      (nth (modulo (1+ x) ancho) (nth (modulo (1- y) alto) matriz)))
    (list
      (nth (modulo (1- x) ancho) (nth (modulo y alto) matriz))
      (nth (modulo x ancho) (nth (modulo y alto) matriz))
      (nth (modulo (1+ x) ancho) (nth (modulo y alto) matriz)))
    (list
      (nth (modulo (1- x) ancho) (nth (modulo (1+ y) alto) matriz))
      (nth (modulo x ancho) (nth (modulo (1+ y) alto) matriz))
      (nth (modulo (1+ x) ancho) (nth (modulo (1+ y) alto) matriz)))
  ))


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

; función parfa convertir un número decimal en binario
(defun convertir-num-binario-acb (num)
  (if (equal num 0)
    nil
    (append (convertir-num-binario-acb (car (list (floor num 2)))) (list (- num (* 2 (floor num 2)))))))

; función para convertir en número binario en un binario de 9 bits
(defun binario-9bits (lista)
  (if (< (length lista) 9)
      (binario-9bits (cons 0 lista))
      lista))

; función que al pasarle un vecindario, por ejemplo, ((1 1 1)(1 1 1)(1 1 1)) nos devuelve el valor en decimal, además, utilizamos la función aplanar
; para que cuando le pasemos el vecindario sea más sencillo al calcular su valor
(defun valor-vecindario-2d (vecindario)
  (if (null vecindario)
    0
    (valor-vecindario (aplanar-lista-recursivo vecindario))))

; función que al pasarle un vecindario hasta el 511, nos simplifica su valor para que sea más sencillo su cálculo en las funciones posteriores
; por ejemplo, si le pasamos un vecindario '((0 0 0)(0 0 1)(1 1 1)) cuyo valor es 15 en decimal, cuando comprobemos este valor dentro de la regla
; será más complejo identificar a que valor se refiere, es por ello que he realizado una conversión del vecindario que según el rango en el que se encuentre
; devolverá su posición para identificar su valor, como el valor en decimal es 15, el valor convertido sería 4. 
(defun obtener-valor-vecindario-2d (vecindario)
  (if (equal vecindario 0)
    0
    (if (equal vecindario 1)
      1
      (1+ (obtener-valor-vecindario-2d (floor vecindario 2))))))

; función para obtener el estado del vecindario según su número decimal, como en la función anterior obtuvimos el valor decimal convertido a su índice correspondiente, mediante la
; siguiente fución y teniendo en cuenta la regla en binario. Es de suma importancia que una vez obtenemos el indice realicemos la resta de una unidad al indice, debido que cuando realizamos
; el nth sobre la lista que es de longitud 9, pero sus posiciones son desde la 0 a la 8, es por ello, que al resultar un valor de indice 9, cuando realicemos el nth con ese valor de indice
; sobre la lista revertida no nos dará ningún valor, por tanto, restamos uno menos y nos resulta el valor según el vecindario que pasamos por parámetro. 
(defun obtener-estado-vecindario-2d (regla vecindario)
  (if (null vecindario)
    0
    (let ((indice (obtener-valor-vecindario-2d (valor-vecindario-2d vecindario))))
      (if (>= (1- indice) (length regla))
        0
        (if (< (1- indice) 0)
          (nth 0 (reverse regla))
          (nth (1- indice) (reverse regla)))))))

; función para construir los vecindarios según la lista que le pasamos, para construir los vecindarios necesitamos dos indice x e y, ambos a 0, y por otro lado, en caso de utilizar
; longitud de listas superior a 9, debemos calcular las dimensiones de lo que será nuestro nuevo vecindario
(defun construir-vecindarios-2d (lista x y)
  (let* ((dimensiones (tam-tablero (obtener-tams-tablero lista 1)))
          (ancho (car dimensiones))
          (alto (cadr dimensiones))
          (matriz (dividir-lista-recursivo lista ancho)))
          (construir-vecindarios-2d-aux matriz x y ancho alto)))

; función auxiliar utilizada para construir los vecindarios a partir de la lista, en este caso, a partir de las dimensiones obtenidas (ancho y alto) y los indices obtenemos
; los vecindarios construidos
(defun construir-vecindarios-2d-aux (matriz x y ancho alto)
  (if (equal y alto)
    nil
    (if (< x (1- ancho))
      (cons (construir-celdas-acb matriz x y ancho alto) (construir-vecindarios-2d-aux matriz (1+ x) y ancho alto))
      (cons (construir-celdas-acb matriz x y ancho alto) (construir-vecindarios-2d-aux matriz 0 (1+ y) ancho alto)))))

; función utilizada para obtener los divisores según la longitud de la lista, debido a que cuando le pasamos una lista de una longitud cualquiera nuestro algoritmo debe calcular
; el tamaño de la matriz correspondiente para construir los vecindarios, por ejemplo si nuestra longitud de la lista es de 36, esta función mostrará todas aquellas conbinaciones de
; de pares se pueden dar calculando sus divisores. La condición de parada se dará cuando el posible numero divisible se superior a la longitud de la lista. Como paso importante
; calculamos si el tamaño de la lista es divisible por el número actual, en caso de que sea divisible es que encontró un divisor, por ejemplo, si la lista mide 9 y el número es 3
; el par generado será de (3 3). Finalmente, en caso de que el número actual no sea divisible, aumentará en una unidad el número de manera recursiva para seguir buscando divisores.
(defun obtener-tams-tablero (lista numero)
  (if (> numero (length lista))
    nil
    (if (eq (modulo (length lista) numero) 0)
      (cons (list numero (/ (length lista) numero)) (obtener-tams-tablero lista (1+ numero)))
      (obtener-tams-tablero lista (1+ numero)))))

; esta función es utilizada para calcular el valor absolto entre las parejas
(defun diferencia (par)
  (if (eq (max (car par) (cadr par)) (car par))
      (- (car par) (cadr par))
      (- (cadr par) (car par))))
  
; esta función reciba todas la combinaciones y decide cuál de todas ellas es la mejor, la función encontrará la combinación lo más cuadrada posible
; mediante el let comparamos el par actual con el que ya haya resultado de todo el resto de la lista. Posteriormente, si la diferencia entre el ancho y el alto del par actual
; es menor que la del "mejor" hasta ahora, el par actual gana, por ejemplo, la función preferirá un 6 x 5 que un 15 x 2. Para el desempate, si las diferencias son iguales, 
; por ejemplo, 3 x 4 y 4 x 3, elige el que tenga un ancho más grande.
(defun tam-tablero (lista)
  (if (null (cdr lista))
      (car lista)
      (let ((mejor-par (tam-tablero (cdr lista)))
            (par (car lista)))
        (if (or (< (diferencia par) (diferencia mejor-par))
                (and (= (diferencia par) (diferencia mejor-par))
                     (> (car par) (car mejor-par))))
            par
            mejor-par))))


; función para obtener el nuevo valor de la celda mediante el cálculo del módulo, para ello le debemos pasar la matriz obtenida, el alto, el ancho y sus respectivos indices
(defun obtener-celda (matriz ancho alto x y)
  (let ((col (modulo x ancho))
          (fila (modulo y alto)))
      (nth col (nth fila matriz))))

; utilizamos esta función para obtener cada posición de los vecindario 3 x 3 de la lista, el cálculo de los vecindarios es:
; c(0, 0) -> x - 1, y - 1   c(0, 1) -> x, y - 1   c(0, 2) -> x + 1, y - 1
; c(1, 0) -> x - 1, y       c(1, 1) -> x, y       c(1, 2) -> x + 1, y
; c(2, 0) -> x - 1, y + 1   c(2, 1) -> x, y + 1   c(2, 2) -> x + 1, y + 1

; a cada variable se le pasa el valor posicional y se realiza el módulo, posteriormente con el valor obtenido
; se ejecuta con la función nth para obtener su fila y columna respectiva y obtener el valor de la celda 

(defun construir-celdas-acb (matriz x y ancho alto)
  (list
    (list 
      (obtener-celda matriz ancho alto (1- x) (1- y))
      (obtener-celda matriz ancho alto x (1- y))
      (obtener-celda matriz ancho alto (1+ x) (1- y)))
    (list
      (obtener-celda matriz ancho alto (1- x) y)
      (obtener-celda matriz ancho alto x y)
      (obtener-celda matriz ancho alto (1+ x) y))
    (list
      (obtener-celda matriz ancho alto (1- x) (1+ y))
      (obtener-celda matriz ancho alto x (1+ y))
      (obtener-celda matriz ancho alto (1+ x) (1+ y)))
  ))

; función utilizada para calcular el módulo
(defun modulo (n1 n2)
  (let ((resultado (- n1 (* n2 (floor n1 n2)))))
    (if (< resultado 0)
      (+ resultado n2)
        resultado)))

; función encargada de seleccionar n elementos, debido a que el algoritmo está diseñados para calcular el tamaño necesario para los vecindarios
; esta función ayuda a seleccionar el número de elementos necesarios para pasarlos a una nueva lista
(defun seleccionar-n (n lista)
  (if (or (null lista) (<= n 0))
    nil
    (cons (car lista) (seleccionar-n (1- n) (cdr lista)))))

; esta función se compagina con la anterior, pero hace lo contrario, salta n elementos para continuar seleccionando los elementos y poder realizar la división
(defun saltar-n (n lista)
  (if (or (null lista) (<= n 0))
    lista
    (saltar-n (1- n) (cdr lista))))

; función utilizada para dividir una función en n sublistas, para ello, utilizando las dos funciones anteriores, primero selecciona los n elementos que especifique el ancho de la matriz
; y posteriormente salta los n elementos para econtrar donde empieza la siguiente información de la lista para poder extraer la información de los siguientes elementos
(defun dividir-lista-recursivo (lista ancho)
  (if (null lista)
    nil
    (cons (seleccionar-n ancho lista)
      (dividir-lista-recursivo (saltar-n ancho lista) ancho))))

; función utilizada para construir una sola generacion, utilizando la función construir-vecindarios-2d donde le pasamos una lista inicial y el
; indice x e y, y a partir de la siguiente, a la cual le pasamos el listado de vecindarios de 3 bits y una regla binaria construimos una sola generación
(defun construir-generacion-2d (regla lista)
  (if (null lista)
    nil
    (cons (obtener-estado-vecindario-2d regla (car lista)) (construir-generacion-2d regla (cdr lista)))))

; función utilizada para construir el automata celular bidimensional, a partir de la lista inicial, la regla binaria y el número de pasos
; realizamos la ejecución pero solo se mostrarán secuencias de 0s y 1s en matrices de 3 x 3.
(defun ev_auto-2d (lista regla pasos)
  (if (> pasos 0)
      (let* ((dimensiones (tam-tablero (obtener-tams-tablero lista 1)))
              (ancho (car dimensiones))
              (nueva-generacion (construir-generacion-2d regla (construir-vecindarios-2d lista 0 0))))      
        (cons (dividir-lista-recursivo nueva-generacion ancho) 
        (ev_auto-2d nueva-generacion regla (1- pasos))))
    nil))

; fución utilizada para mostrar el autómata celular bidimensional pero intercambiando los 0s y 1s por _ y @.
(defun mostrar-automata-2d (matriz)
  (mapcar (lambda (fila)
    (mapcar (lambda (columna)
      (mapcar (lambda (x)
        (if (equal x 1)
          '@ '_))
      columna)) 
    fila)) 
  matriz))
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
; let para crear variables locales
; let lleva una lista de variables -> (let* ((var_1 (+ 1 (* 2 (nth 1 l))))
;                                           (var_2 (+ (car l) (cadr l))
;                                           (var_3 (+ var_1 var_2)))
;                                    ) <- parentesis de asignaciones
;                                   (+ (otra-funcion (var_1 var_2 var_3) 2)))
; let se ejecuta en paralelo, para eso se introduce el *


; esta función cuenta las células vivas que hay en una lista, es decir, cuenta el número de 1 que se encuentra, ya que, una celula viva representa un 1 y una muerta un 0.
(defun obtener-valores-vecindario (vecindario)
  (if (null vecindario)
    0
    (let ((siguiente (obtener-valores-vecindario (cdr vecindario))))
    (if (equal (car vecindario) 1)
      (1+ siguiente)
      siguiente))))

(defun aplanar-lista-recursivo (lista)
(if (null lista)
  nil
  (if (atom (car lista))
    (cons (car lista) (aplanar-lista-recursivo (cdr lista)))
    (append (aplanar-lista-recursivo (car lista)) (aplanar-lista-recursivo (cdr lista))))))

; esta función llama a la función cuenta el número de células vivas y como lista se pasa la lista pero ya aplanada, debido a que trabajamos con matrices y en mi caso, es más sencillo
; aplanar la lista para luego realizar el conteo de las células.
(defun obtener-valores-vecindario-2d (vecindario)
  (obtener-valores-vecindario (aplanar-lista-recursivo vecindario)))

; esta función ayuda a calcular el siguiente estado de una célula según las reglas, las cuales son:
  ; - si una célula muerta tiene exactamente 3 células vivas "nace"
  ; - una célula viva se mantiene si tiene 2 o 3 vecinos a su alrededor
; por lo que dado un vecindario de 3x3 la función calculará el siguiente estado de esa célula.
; para comprobar cuantas células vivas tiene ese vecindario utilizamos la función obtener-valores-vecindario-2d para obtener cuantas células vivas tiene alrededor menos ella misma
; según cada caso esta función decidirá si esa célula segurirá viviendo o morirá.
(defun obtener-estado (pos vecindario)
  (let ((comprobar-vivos (- (obtener-valores-vecindario-2d vecindario) pos)))
    (if (equal pos 1)
      (if (or (equal comprobar-vivos 2) (equal comprobar-vivos 3))
        1
        0)
      (if (equal comprobar-vivos 3) 
        1
        0))))

; esta función es utilizada para construir todas las filas de la nueva generación. Para la condición de parada, si no hay más filas, termina.
; construye una fila completa y para las siguientes filas las construye de forma recursiva
(defun construir-fila (matriz y)
  (if (>= y (length matriz))
    nil
    (cons (construir-columna matriz y 0) (construir-fila matriz (1+ y)))))

; esta función construye una fila céluca a célula, en primer lugar, para cada posición (x, y) construye una vecindario 3x3 mediante la función construir-celdas-acb,
; de esta forma el algoritmo realiza un comportamiento toroidal. Es por ello, que obtiene el nuevo estado de la célula calculada y lo añade a la fila
(defun construir-columna (matriz y x)
  (if (>= x (length (car matriz)))
    nil
    (cons (obtener-estado (nth x (nth y matriz)) (construir-celdas-acb matriz x y)) (construir-columna matriz y (1+ x)))))


; función utilizada para calcular el módulo
(defun modulo (n1 n2)
  (let ((resultado (- n1 (* n2 (floor n1 n2)))))
    (if (< resultado 0)
      (+ resultado n2)
        resultado)))

; utilizamos esta función para obtener cada posición de los vecindario 3 x 3 de la lista, el cálculo de los vecindarios es:
; c(0, 0) -> x - 1, y - 1   c(0, 1) -> x, y - 1   c(0, 2) -> x + 1, y - 1
; c(1, 0) -> x - 1, y       c(1, 1) -> x, y       c(1, 2) -> x + 1, y
; c(2, 0) -> x - 1, y + 1   c(2, 1) -> x, y + 1   c(2, 2) -> x + 1, y + 1

; a cada variable se le pasa el valor posicional y se realiza el módulo, posteriormente con el valor obtenido
; se ejecuta con la función nth para obtener su fila y columna respectiva y obtener el valor de la celda


(defun construir-celdas-acb (lista x y)
  (list
    (list 
      (nth (modulo (1- x) (length lista)) (nth (modulo (1- y) (length lista)) lista))
      (nth (modulo x (length lista)) (nth (modulo (1- y) (length lista)) lista))
      (nth (modulo (1+ x) (length lista)) (nth (modulo (1- y) (length lista)) lista)))
    (list
      (nth (modulo (1- x) (length lista)) (nth (modulo y (length lista)) lista))
      (nth (modulo x (length lista)) (nth (modulo y (length lista)) lista))
      (nth (modulo (1+ x) (length lista)) (nth (modulo y (length lista)) lista)))
    (list
      (nth (modulo (1- x) (length lista)) (nth (modulo (1+ y) (length lista)) lista))
      (nth (modulo x (length lista)) (nth (modulo (1+ y) (length lista)) lista))
      (nth (modulo (1+ x) (length lista)) (nth (modulo (1+ y) (length lista)) lista)))
  ))


; esta función es la utilizada para generar una nueva matriz completa desde la posición inicial.
(defun construir-nueva-generacion (matriz)
  (construir-fila matriz 0))

; esta función es la utilizad para realizar la conversión de los 1 y 0 dentro de la matriz
(defun construir-juego-vida (matriz)
  (mapcar (lambda (fila)
    (print (mapcar (lambda (x)
        (if (equal x 1)
          '@ '_))
      fila)))
  matriz))

; esta función es la utilizada para mostrar el video durante el número de pasos que se haya introducido, en cada iteración construye la matriz, espera, genera la nueva generación y repite
; así hasta completa el número de pasos introducido
(defun mostrar-video (matriz pasos)
  (if (> pasos 0)
    (let ((construir (construir-juego-vida matriz))
          (esperar (sleep 0.5))
          (saltar (terpri)))
      (mostrar-video (construir-nueva-generacion matriz) (1- pasos)))))

(defun error-fitness (arbol punto)
  (if (null arbol)
    nil
    (if (equal (car arbol) 'x)
      (error-fitness-2 arbol)
      (+ (eval arbol) (error-fitness (cdr arbol) punto))))) 

(defun sustituir-punto (arbol punto)
  (mapcar (lambda (x)
    (if (equal x 'x)
      (subst 'x punto arbol)
      x))
  arbol))

  (defun sustituir-x (arbol punto)
    (eval (subst punto 'x arbol)))

  (defun evaluar-operacion (arbol punto)
    (let ((x0 (sustituir-x arbol punto)))
      (eval x0)))

