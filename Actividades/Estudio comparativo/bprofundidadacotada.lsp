;;; Búsqueda en profundidad acotada

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

;;;  Representación de los nodos

;;; Los nodos se componen de un estado y un camino (que es la lista de
;;; operadores que para transformar el estado inicial en el actual, escrita en
;;; orden inverso)

;;; NODO
;;; Es una estructura con constructor CREA-NODO y campos ESTADO y CAMINO.

(defstruct (nodo (:conc-name nil)
		 (:constructor crea-nodo))
  estado
  camino)

;;; Procedimiento de búsqueda en profundidad acotada

;;; (BUSQUEDA-EN-PROFUNDIDAD-ACOTADA :COTA N)
;;; Argumento clave: N es la cota de la profundidad (por defecto, 5)
;;; Valor: El primer nodo final encontrado de acuerdo al procedimiento de
;;;   búsqueda en profundidad acotada.
;;; Procedimiento:
;;; 1. Crear las siguientes variables locales
;;;    1.1. ABIERTOS (para almacenar los nodos generados aún no analizados)
;;;         con valor la lista formado por el nodo inicial (es decir, el nodo
;;;	    cuyo estado es el estado inicial y cuyo camino es la lista vacía);
;;;    1.2. CERRADOS (para almacenar los nodos analizados) con valor la
;;;         lista vacía;
;;;    1.3. ACTUAL (para almacenar el nodo actual) con valor la lista vacía.
;;;    1.4. NUEVOS-SUCESORES (para almacenar el la lista de los sucesores del
;;;         nodo actual que aún no se han generado) con valor la lista vacía.
;;; 2. Mientras que ABIERTOS no está vacía,
;;;    2.1 Hacer ACTUAL el primer nodo de ABIERTOS
;;;    2.2 Hacer ABIERTOS el resto de ABIERTOS
;;;    2.3 Poner el nodo ACTUAL en CERRADOS.
;;;    2.4 Si el nodo ACTUAL es un final,
;;;        2.4.1 devolver el nodo ACTUAL y terminar.
;;;        2.4.2 si la profundidad del ACTUAL es menor que la cota, hacer
;;;              2.4.2.1 NUEVOS-SUCESORES la lista de sucesores del nodo
;;;                      ACTUAL que no están en ABIERTOS ni en CERRADOS y
;;;              2.4.2.2 ABIERTOS la lista obtenida añadiendo los
;;;                      NUEVOS-SUCESORES al priincipio de ABIERTOS.
;;; 3. Si ABIERTOS está vacía, devolver NIL.

(defun busqueda-en-profundidad-acotada (&key (COTA 5))
  (let ((abiertos (list (crea-nodo :estado *estado-inicial*   	     ;1.1
				   :camino nil)))
	(cerrados nil)						     ;1.2
	(actual nil)						     ;1.3
	(nuevos-sucesores nil))					     ;1.4
    (loop until (null abiertos) do			 	     ;2
	  (setf actual (first abiertos))			     ;2.1
	  (setf abiertos (rest abiertos))			     ;2.2
	  (setf cerrados (cons actual cerrados))		     ;2.3
	  (cond ((es-estado-final (estado actual))	             ;2.4
		 (return actual))	             		     ;2.4.1
		((< (LENGTH (CAMINO ACTUAL)) COTA)
		 (setf nuevos-sucesores			             ;2.4.2.1
		       (nuevos-sucesores actual	abiertos cerrados))
		 (setf abiertos				             ;2.4.2.2
		       (append nuevos-sucesores abiertos)))))))

;;;****************************************************************************
;;; § Funciones auxiliares
;;;****************************************************************************

;;; (NUEVOS-SUCESORES NODO ABIERTOS CERRADOS)
;;; Valor: La lista de los nodos obtenidos aplicando al estado del NODO todos
;;;   los *OPERADORES* aplicables al NODO, pero que no están en ABIERTOS ni en
;;;   CERRADOS.

(defun nuevos-sucesores (nodo abiertos cerrados)
  (elimina-duplicados (sucesores nodo) abiertos cerrados))

;;; (SUCESORES NODO)
;;; Valor: La lista de los nodos obtenidos aplicando al estado del NODO todos
;;;   los *OPERADORES* aplicables al NODO.

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
;;;     y cuyo camino es el obtenido añadiendo el OPERADOR al camino del NODO.
;;;   - En caso contrario, devuelve NIL.
;;; Procedimiento:
;;; 1. Sea SIGUIENTE-ESTADO el estado obtenido aplicando el OPERADOR al estado
;;;    del NODO, si es aplicable y NIL cuando no lo sea.
;;; 2. Cuando sea aplicable, devolver el nodo
;;;    2.1 cuyo estado es el ESTADO-SIGUIENTE y
;;;    2.2 cuyo camino es el obtenido añadiendo el OPERADOR al camino del NODO.
;;;    En caso contrario, devolver NIL.

(defun sucesor (nodo operador)
  (let ((siguiente-estado (aplica operador (estado nodo))))		;1
    (when siguiente-estado						;2
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
;;;   - T, si el estado del NODO coincide con el estado de alguno de los nodos
;;;     de la LISTA-DE-NODOS;
;;;   - NIL, en caso contrario.

(defun esta (nodo lista-de-nodos)
  (let ((estado (estado nodo)))
    (loop for n in lista-de-nodos
	  thereis (equalp estado (estado n)))))

;; Ejemplo de sesión

;;; > (load "jarras.lsp")
;;; T
;;; > (load "bprofundidadacotada.lsp")
;;; T
;;; > (busqueda-en-profundidad-acotada :cota 3)
;;; NIL
