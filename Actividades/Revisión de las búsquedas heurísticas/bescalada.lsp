;;; B�squeda en escalada

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

;;; Representaci�n de los nodos heur�sticos

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

;;; Procedimiento de b�squeda en escalada

;;; (BUSQUEDA-EN-ESCALADA)
;;; Valor: El primer nodo final encontrado de acuerdo al procedimiento de
;;;   b�squeda en escalada.
;;; Procedimiento:
;;; 1. Crear la variable local ACTUAL que es el nodo heur�stico cuyo estado es
;;;    el *ESTADO-INICIAL*, cuyo camino es la lista vac�a y cuya heur�stica es
;;;    la del *ESTADO-INICIAL*. 
;;; 2. Repetir mientras que el nodo ACTUAL no sea nulo:
;;;    2.1. si es estado del nodo actual es un estado final, devolver el nodo
;;;         ACTUAL y terminar;
;;;    2.2. en caso contrario, cambiar ACTUAL por su mejor sucesor (es decir,
;;;         uno de sus sucesores cuya heur�stica sea menor que la del nodo
;;;         ACTUAL y menor o igual que las heur�sticas de los restantes
;;;         sucesores, si existen dichos sucesores y NIL en caso contrario).
(defun busqueda-en-escalada ()
  (let ((actual (crea-nodo-h :estado *estado-inicial*                 ; 1
			     :camino nil
			     :heuristica-del-nodo
			     (heuristica *estado-inicial*)))) 
    (loop until (null actual) do                                      ; 2
	  (cond ((es-estado-final (estado actual))                    ; 2.1
		 (return actual))
		(t (setf actual                                       ; 2.2
                         (mejor (sucesores actual)
                                (heuristica-del-nodo actual))))))))   

;;; Funciones auxiliares

;;; (MEJOR NODOS MINIMA-DISTANCIA-AL-FINAL)
;;; Valor: 
;;;   - NIL, si no hay ning�n nodo en la lista NODOS cuya heur�stica sea menor
;;;     que la MINIMA-DISTANCIA-AL-FINAL;
;;;   - un nodo de NODOS cuy heur�stica es menor o igual que los restantes, en
;;;     caso contrario.
(defun mejor (nodos minima-distancia-al-final)
  (when nodos
    (let ((mejor-nodo (first nodos))) ; Inicializar con el primer nodo
      (loop for n in (rest nodos) do  ; Revisar los demás nodos
        (when (< (heuristica-del-nodo n) minima-distancia-al-final)
          (setf mejor-nodo n
                minima-distancia-al-final (heuristica-del-nodo n))))
      mejor-nodo))) ; Devuelve el mejor nodo sin tanta aleatoriedad


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







