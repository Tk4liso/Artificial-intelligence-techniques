;;; B�squeda primero el mejor

;;; Funciones y variables dependientes del problema

;;; Se supone que se han definido
;;; 1. La variable *ESTADO-INICIAL* que contiene el estado inicial.
;;; 2. La funci�n (ES-ESTADO-FINAL ESTADO) que determina si el ESTADO es un
;;;    estado final.
;;; 3. La variable *OPERADORES* que contiene la lista de operadores.
;;; 4. Para cada OPERADOR la funci�n (OPERADOR ESTADO-ACTUAL) que devuelve
;;;    el estado obtenido aplicando el OPERADOR al ESTADO-ACTUAL, si el
;;;    OPERADOR es aplicable y NIL, en caso contrario.
;;; 5. La funci�n (HEURISTICA ESTADO) que devuelve el valor de la funci�n de
;;;    evaluaci�n heur�stica aplicada al ESTADO (es decir, el costo estimado
;;;    para alcanzar una soluci�n a partir del ESTADO).

(proclaim '(special *estado-inicial* *operadores*))

;;;**************************************************************************
;;; Representaci�n de los nodos heur�sticos
;;;**************************************************************************

;;; Los nodos heur�sticos se componen de:
;;; - un estado,
;;; - un camino (que es la lista de operadores que para transformar el
;;;   estado inicial en el actual, escrita en orden inverso) y
;;; - el valor de la funci�n de evaluaci�n heur�stica del estado actual.

;;; NODO-H
;;; Es una estructura con constructor CREA-NODO-H y campos ESTADO, CAMINO,
;;; y HEURISTICA-DEL-NODO.
(defstruct (nodo-h (:constructor crea-nodo-h)
		   (:conc-name nil))
  estado
  camino
  heuristica-del-nodo)

;;;**************************************************************************
;;; Procedimiento de b�squeda por primero el mejor
;;;**************************************************************************

;;; (BUSQUEDA-POR-PRIMERO-EL-MEJOR)
;;; Valor: El primer nodo final encontrado de acuerdo al procedimiento de
;;;   b�squeda por primero el mejor.
;;; Procedimiento:
;;; 1. Crear las siguientes variables locales
;;;    1.1. ABIERTOS (para almacenar los nodos generados a�n no analizados)
;;;         con valor la lista formado por el nodo inicial (es decir, el nodo
;;;	    cuyo estado es el estado inicial, cuyo camino es la lista vac�a y
;;;         cuya heur�stica es la del estado inicial);
;;;    1.2. CERRADOS (para almacenar los nodos analizados) con valor la
;;;         lista vac�a;
;;;    1.3. ACTUAL (para almacenar el nodo actual) con valor la lista vac�a.
;;;    1.4. NUEVOS-SUCESORES (para almacenar la lista de los sucesores del
;;;         nodo actual) con valor la lista vac�a.
;;; 2. Mientras que ABIERTOS no est� vac�a,
;;;    2.1 Hacer ACTUAL el primer nodo de ABIERTOS
;;;    2.2 Hacer ABIERTOS el resto de ABIERTOS
;;;    2.3 Poner el nodo ACTUAL en CERRADOS.
;;;    2.4 Si el nodo ACTUAL es un final,
;;;        2.4.1 devolver el nodo ACTUAL y terminar.
;;;        2.4.2 en caso contrario, hacer
;;;              2.4.2.1 NUEVOS-SUCESORES la lista de sucesores del nodo
;;;                      ACTUAL que no est�n en ABIERTOS ni en CERRADOS y
;;;              2.4.2.2 ABIERTOS la lista obtenida a�adiendo los
;;;                      NUEVOS-SUCESORES al final de ABIERTOS y ordenando sus
;;;                      nodos por orden creciente de sus heur�sticas.
;;; 3. Si ABIERTOS est� vac�a, devolver NIL.
(defun busqueda-por-primero-el-mejor ()
  (let ((abiertos (list (crea-nodo-h :estado *estado-inicial*
				     :camino nil
				     :heuristica-del-nodo
				     (heuristica *estado-inicial*)))) ;1.1
	(cerrados nil)                                                ;1.2
	(actual nil)                                                  ;1.3
	(nuevos-sucesores nil))                                       ;1.4
    (loop until (null abiertos) do                                    ;2
      (setf actual (first abiertos))                                  ;2.1
      (setf abiertos (rest abiertos))                                 ;2.2
      (setf cerrados (cons actual cerrados))                          ;2.3
      (cond ((es-estado-final (estado actual))                        ;2.4
             (return actual))                                         ;2.4.1
            (t (setf nuevos-sucesores                                 ;2.4.2.1
                     (nuevos-sucesores actual abiertos cerrados))
               (setf abiertos                                         ;2.4.2.2
                     (ordena-por-heuristica
                      (append abiertos nuevos-sucesores))))))))

;;;**************************************************************************
;;; Funciones auxiliares
;;;**************************************************************************

;;; (NUEVOS-SUCESORES NODO ABIERTOS CERRADOS)
;;; Valor: La lista de los nodos obtenidos aplicando al estado del NODO todos
;;;   los *OPERADORES* aplicables al NODO, pero que no est�n en ABIERTOS ni en
;;;   CERRADOS.
(defun nuevos-sucesores (nodo abiertos cerrados)
  (elimina-duplicados (sucesores nodo) abiertos cerrados))

;;;; (SUCESORES NODO)
;;; Valor: La lista de los nodos obtenidos aplicando al estado del NODO todos
;;;   los *OPERADORES* aplicables al NODO.
(defun sucesores (nodo)
  (let ((resultado ()))
    (loop for operador in *operadores* do
      (let ((siguiente (sucesor nodo operador)))
	(when siguiente (push siguiente resultado))))
    (nreverse resultado)))

;;;; (SUCESOR NODO OPERADOR)
;; Valor:
;;;   - Si el OPERADOR puede aplicarse al estado del NODO, devuelve el nodo
;;;     cuyo estado es el obtenido aplicando al estado del NODO el OPERADOR
;;;     y cuyo camino es el obtenido a�adiendo el OPERADOR al camino del NODO.
;;;   - En caso contrario, devuelve NIL.
(defun sucesor (nodo operador)
  (let ((siguiente-estado (aplica operador (estado nodo))))          
    (when siguiente-estado						
	  (crea-nodo-h :estado siguiente-estado
		       :camino (cons operador (camino nodo))
		       :heuristica-del-nodo (heuristica siguiente-estado)))))

;;;; (ELIMINA-DUPLICADOS NODOS ABIERTOS CERRADOS)
;;; Valor: La lista de los NODOS que no est�n en ABIERTOS ni en CERRADOS.
(defun elimina-duplicados (nodos abiertos cerrados)
  (loop for nodo in nodos
	when (and (not (esta nodo abiertos))
		  (not (esta nodo cerrados)))
	collect nodo))

;;;; (ESTA NODO LISTA-DE-NODOS)
;;; Valor:
;;;   - T, si el estado del NODO coincide con el estado de alguno de los nodos
;;;     de la LISTA-DE-NODOS;
;;;   - NIL, en caso contrario.
(defun esta (nodo lista-de-nodos)
  (let ((estado (estado nodo)))
    (loop for n in lista-de-nodos
	  thereis (equalp estado (estado n)))))

;;; (ORDENA-POR-HEURISTICA LISTA-DE-NODOS)
;;; Valor: Una lista cuyos elementos son los de la LISTA-DE-NODOS pero
;;;   ordenados de menor a mayor heur�stica.
(defun ordena-por-heuristica (lista-de-nodos)
  (sort lista-de-nodos #'< :key #'heuristica-del-nodo))