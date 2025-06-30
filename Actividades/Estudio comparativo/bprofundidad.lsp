;;; Búsqueda en profundidad

;;; Funciones y variables dependientes del problema

;;; Se supone que se han definido
;;; 1. La variable *ESTADO-INICIAL* que contiene el estado inicial.
;;; 2. La función (ES-ESTADO-FINAL ESTADO) que determina si el ESTADO es un
;;;    estado final.
;;; 3. La variable *OPERADORES* que contiene la lista de operadores.
;;; 4. Para cada OPERADOR la función (OPERADOR ESTADO-ACTUAL) que devuelve
;;;    el estado obtenido aplicando el OPERADOR al ESTADO-ACTUAL, si el
;;;    OPERADOR es aplicable y NIL, en caso contrario.

(proclaim '(special *estado-inicial* *operadores*))

;;; Representación de los nodos

;;; Los nodos se componen de un estado y un camino (que es la lista de
;;; operadores que transforman el estado inicial en el actual, escrita en
;;; orden inverso)

;;; NODO
;;; Es una estructura con constructor CREA-NODO y campos ESTADO y CAMINO.
(defstruct (nodo (:constructor crea-nodo)
		 (:conc-name nil))
  estado
  camino)

;;; Procedimiento de búsqueda en profundidad

;;; (BUSQUEDA-EN-PROFUNDIDAD)
;;; Valor: El primer nodo final encontrado de acuerdo al procedimiento de
;;;   búsqueda en profundidad.
;;; Procedimiento:
;;; 1. Crear las siguientes variables locales
;;;    1.1. ABIERTOS (para almacenar los nodos generados aún no analizados)
;;;         con valor la lista formado por el nodo inicial (es decir, el
;;;	    nodo cuyo estado es el estado inicial y cuyo camino es la lista
;;;	    vacía);
;;;    1.2. CERRADOS (para almacenar los nodos analizados) con valor la
;;;         lista vacía;
;;;    1.3. ACTUAL (para almacenar el nodo actual) con valor la lista vacía.
;;;    1.4. NUEVOS-SUCESORES (para almacenar la lista de los sucesores del
;;;         nodo actual que aún no se han generado) con valor la lista
;;;         vacía.
;;; 2. Mientras que ABIERTOS no está vacía,
;;;    2.1 Hacer ACTUAL el primer nodo de ABIERTOS
;;;    2.2 Hacer ABIERTOS el resto de ABIERTOS
;;;    2.3 Poner el nodo ACTUAL en CERRADOS.
;;;    2.4 Si el nodo ACTUAL es un final,
;;;        2.4.1 devolver el nodo ACTUAL y terminar.
;;;        2.4.2 en caso contrario, hacer
;;;              2.4.2.1 NUEVOS-SUCESORES la lista de sucesores del nodo
;;;                      ACTUAL que no están en ABIERTOS ni en CERRADOS y
;;;              2.4.2.2 ABIERTOS la lista obtenida añadiendo los
;;;                      NUEVOS-SUCESORES al principio de ABIERTOS.
;;; 3. Si ABIERTOS está vacía, devolver NIL.
(defun busqueda-en-profundidad ()
  (let ((abiertos (list (crea-nodo :estado *estado-inicial*           ;1.1
                                   :camino nil)))
        (cerrados nil)                                                ;1.2
        (actual nil)                                                  ;1.3
        (nuevos-sucesores nil))                                       ;1.4
    (loop until (null abiertos) do                                    ;2
          (setf actual (first abiertos))                              ;2.1
          (setf abiertos (rest abiertos))                             ;2.2
          (setf cerrados (cons actual cerrados))                      ;2.3
          (cond ((es-estado-final (estado actual))                    ;2.4
                 (return actual))                                     ;2.4.1
                (t (setf nuevos-sucesores                             ;2.4.2.1
                         (nuevos-sucesores actual abiertos cerrados))
                   (setf abiertos                                     ;2.4.2.2
                         (APPEND NUEVOS-SUCESORES ABIERTOS)))))))

;;; Funciones auxiliares

;;; (NUEVOS-SUCESORES NODO ABIERTOS CERRADOS)
;;; Valor: La lista de los nodos obtenidos aplicando al estado del NODO
;;;   todos los *OPERADORES* aplicables al NODO, pero que no están en
;;;   ABIERTOS ni en CERRADOS.
(defun nuevos-sucesores (nodo abiertos cerrados)
  (elimina-duplicados (sucesores nodo) abiertos cerrados))

;;; (SUCESORES NODO)
;;; Valor: La lista de los nodos obtenidos aplicando al estado del NODO
;;;   todos los *OPERADORES* aplicables al NODO.
(defun sucesores (nodo)
  (let ((resultado ()))
    (loop for operador in *operadores* do
      (let ((siguiente (sucesor nodo operador)))
        (when siguiente (push siguiente resultado))))
    (nreverse resultado)))

;;; (SUCESOR NODO OPERADOR)
;;; Valor:
;;;   - Si el OPERADOR puede aplicarse al estado del NODO, devuelve el nodo
;;;     cuyo estado es el obtenido aplicando al estado del NODO el OPERADOR
;;;     y cuyo camino es el obtenido añadiendo el OPERADOR al camino del
;;;     NODO.
;;;   - En caso contrario, devuelve NIL.
;;; Procedimiento:
;;; 1. Sea SIGUIENTE-ESTADO el estado obtenido aplicando el OPERADOR al
;;;    estado del NODO, si es aplicable y NIL cuando no lo sea.
;;; 2. Cuando sea aplicable, devolver el nodo
;;;    2.1 cuyo estado es el ESTADO-SIGUIENTE y
;;;    2.2 cuyo camino es el obtenido añadiendo el OPERADOR al camino del
;;;    NODO.  En caso contrario, devolver NIL.
(defun sucesor (nodo operador)
  (let ((siguiente-estado (aplica operador (estado nodo))))             ;1
    (when siguiente-estado                                              ;2
          (crea-nodo :estado siguiente-estado                           ;2.1
                     :camino (cons operador                             ;2.2
                                   (camino nodo))))))

;;; (ELIMINA-DUPLICADOS NODOS ABIERTOS CERRADOS)
;;; Valor: La lista de los NODOS que no están en ABIERTOS ni en CERRADOS.
(defun elimina-duplicados (nodos abiertos cerrados)
  (loop for nodo in nodos
        when (and (not (esta nodo abiertos))
                  (not (esta nodo cerrados)))
        collect nodo))

;;; (ESTA NODO LISTA-DE-NODOS)
;;; Valor:
;;;   - T, si el estado del NODO coincide con el estado de alguno de los
;;;     nodos de la LISTA-DE-NODOS;
;;;   - NIL, en caso contrario.
(defun esta (nodo lista-de-nodos)
  (let ((estado (estado nodo)))
    (loop for n in lista-de-nodos
          thereis (equalp estado (estado n)))))

;;;============================================================================

(defun busqueda-en-profundidad-con-tablas ()
  (let ((abiertos (list (crea-nodo :estado *estado-inicial*           ;1.1
                                   :camino nil)))
        (cerrados nil)                                                ;1.2
        (actual nil)                                                  ;1.3
        (nuevos-sucesores nil)                                        ;1.4
	(n-nodo 0))
    (format t "----------------------------------------------------------------------~%")
    (format t "Abiertos: ~a~%" (mapcar #'estado abiertos))
    (format t "----------------------------------------------------------------------~%")
    (loop until (null abiertos) do                                    ;2
          (setf actual (first abiertos))                              ;2.1
	  (setf n-nodo (+ n-nodo 1))
          (setf abiertos (rest abiertos))                             ;2.2
          (setf cerrados (cons actual cerrados))                      ;2.3
	  (format t "Nodo: ~a~%" n-nodo)
	  (format t "Actual: ~a~%" (estado actual))
          (cond ((es-estado-final (estado actual))                    ;2.4
		 (format t
			 "----------------------------------------------------------------------~%")
                 (return actual))                                     ;2.4.1
                (t (setf nuevos-sucesores                             ;2.4.2.1
                         (nuevos-sucesores actual abiertos cerrados))
                   (setf abiertos                                     ;2.4.2.2
                         (APPEND NUEVOS-SUCESORES ABIERTOS))))
	  (format t "Sucesores: ~a~%" (mapcar #'estado nuevos-sucesores))
	  (format t "Abiertos: ~a~%" (mapcar #'estado abiertos))
	  (format t "----------------------------------------------------------------------~%"))))

;;;**************************************************************************

(defun busqueda-en-profundidad-con-estadisticas ()
  (let ((abiertos (list (crea-nodo :estado *estado-inicial*           ;1.1
                                   :camino nil)))
	(max-abiertos 1)
	(prof-max 0)
        (cerrados nil)                                                ;1.2
        (actual nil)                                                  ;1.3
        (nuevos-sucesores nil)                                        ;1.4
	(n-nodo 0))
    (format t "----------------------------------------------------------------------~%")
    (loop until (null abiertos) do                                    ;2
          (setf actual (first abiertos))                              ;2.1
	  (setf n-nodo (+ n-nodo 1))
	  (setf prof-max (max (length (camino actual)) prof-max))
          (setf abiertos (rest abiertos))                             ;2.2
          (setf cerrados (cons actual cerrados))                      ;2.3
          (cond ((es-estado-final (estado actual))                    ;2.4
		 (format t "Nodos analizados: ~a~%" n-nodo)
		 (format t "Máximo en abiertos: ~a~%" max-abiertos)
		 (format t "Profundidad máxima: ~a~%" prof-max)
		 (format t "----------------------------------------------------------------------~%")
                 (return actual))                                     ;2.4.1
                (t (setf nuevos-sucesores                             ;2.4.2.1
                         (nuevos-sucesores actual abiertos cerrados))
                   (setf abiertos                                     ;2.4.2.2
                         (APPEND NUEVOS-SUCESORES ABIERTOS))
		   (setf max-abiertos (max (length abiertos) max-abiertos)))))))

;;;**************************************************************************
