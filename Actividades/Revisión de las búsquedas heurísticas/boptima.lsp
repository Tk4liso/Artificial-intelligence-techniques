;;; Búsqueda optima

;;; Funciones y variables dependientes del problema

;;; Se supone que se han definido
;;; 1. La variable *ESTADO-INICIAL* que contiene el estado inicial.
;;; 2. La función (ES-ESTADO-FINAL ESTADO) que determina si el ESTADO es un
;;;    estado final.
;;; 3. La variable *OPERADORES* que contiene la lista de operadores.
;;; 4. Para cada OPERADOR la función (OPERADOR ESTADO-ACTUAL) que devuelve
;;;    el estado obtenido aplicando el OPERADOR al ESTADO-ACTUAL, si el
;;;    OPERADOR es aplicable y NIL, en caso contrario.
;;; 5. La función (COSTO-DE-APLICAR-OPERADOR ESTADO OPERADOR) que devuelve
;;;    el costo de aplicar el OPERADOR al ESTADO.

(proclaim '(special *estado-inicial* *operadores*))

;;;**************************************************************************
;;; Representación de los nodos con costo
;;;**************************************************************************

;;; Los nodos con costo se componen de:
;;; - un estado,
;;; - un camino (que es la lista de operadores que para transformar el
;;;   estado inicial en el actual, escrita en orden inverso) y
;;; - el costo del camino actual.

;;; NODO-C
;;; Es una estructura con constructor CREA-NODO-C y campos ESTADO, CAMINO
;;; y COSTO-CAMINO.
(defstruct (nodo-c (:constructor crea-nodo-c)
                   (:conc-name nil))
  estado
  camino
  costo-camino)

;;;**************************************************************************
;;; Procedimiento de búsqueda optima
;;;**************************************************************************

;;; (BUSQUEDA-OPTIMA)
;;; Valor: El primer nodo final encontrado de acuerdo al procedimiento de
;;;   búsqueda optima (que es un nodo final de cuyo camino es de costo mínimo)
;;; Procedimiento:
;;; 1. Crear las siguientes variables locales
;;;    1.1. ABIERTOS (para almacenar los nodos generados aún no analizados)
;;;         con valor la lista formado por el nodo inicial (es decir, el nodo
;;;	    cuyo estado es el estado inicial, cuyo camino es la lista vacía y
;;;         cuyo costo es 0);
;;;    1.2. CERRADOS (para almacenar los nodos analizados) con valor la
;;;         lista vacía;
;;;    1.3. ACTUAL (para almacenar el nodo actual) con valor la lista vacía.
;;;    1.4. SUCESORES (para almacenar la lista de los sucesores del
;;;         nodo actual) con valor la lista vacía.
;;; 2. Mientras que ABIERTOS no está vacía,
;;;    2.1 Hacer ACTUAL el primer nodo de ABIERTOS
;;;    2.2 Hacer ABIERTOS el resto de ABIERTOS
;;;    2.3 Poner el nodo ACTUAL en CERRADOS.
;;;    2.4 Si el nodo ACTUAL es un final,
;;;        2.4.1 devolver el nodo ACTUAL y terminar.
;;;        2.4.2 en caso contrario, hacer
;;;              2.4.2.1 SUCESORES la lista de sucesores del nodo ACTUAL para
;;;                      los que no existen en ABIERTOS o CERRADOS un nodo con
;;;                      el mismo estado y menor costo.
;;;              2.4.2.2 ABIERTOS la lista obtenida añadiendo los
;;;                      NUEVOS-SUCESORES al final de ABIERTOS y ordenando sus
;;;                      nodos por orden creciente de sus costos.
;;; 3. Si ABIERTOS está vacía, devolver NIL.
(defun busqueda-optima ()
  (let ((abiertos (list (crea-nodo-c :estado *estado-inicial*        ;1.1
                                     :camino nil
                                     :costo-camino 0)))
        (cerrados nil)                                               ;1.2
        (actual nil)                                                 ;1.3
        (sucesores nil))                                             ;1.4
    (loop until (null abiertos) do                                   ;2
      (setf actual (first abiertos))                                 ;2.1
      (setf abiertos (rest abiertos))                                ;2.2
      (setf cerrados (cons actual cerrados))                         ;2.3
      (cond ((es-estado-final (estado actual))                       ;2.4
             (return actual))                                        ;2.4.1
            (t (setf sucesores                                       ;2.4.2.1
                     (nuevos-o-mejores-sucesores actual abiertos cerrados))
               (setf abiertos                                        ;2.4.2.2
                     (ordena-por-costo (append abiertos sucesores))))))))

;;;**************************************************************************
;;; Funciones auxiliares
;;;**************************************************************************

;;; (NUEVOS-O-MEJORES-SUCESORES NODO ABIERTOS CERRADOS)
;;; Valor: La lista de sucesores del nodo ACTUAL para los que no existen en
;;;   ABIERTOS o CERRADOS un nodo con el mismo estado y menor costo.
(defun nuevos-o-mejores-sucesores (nodo abiertos cerrados)
  (elimina-peores (sucesores nodo) abiertos cerrados))

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
;;;     y cuyo camino es el obtenido añadiendo el OPERADOR al camino del NODO.
;;;   - En caso contrario, devuelve NIL.
(defun sucesor (nodo operador)
  (let ((siguiente-estado (aplica operador (estado nodo))))
    (when siguiente-estado                                              
          (crea-nodo-c :estado siguiente-estado
                       :camino (cons operador (camino nodo))
                       :costo-camino
                       (+ (costo-de-aplicar-operador (estado nodo) operador)
                          (costo-camino nodo))))))

;;; (ELIMINA-PEORES NODOS ABIERTOS CERRADOS)
;;; Valor: La lista de los NODOS que no tienen en ABIERTOS ni en CERRADOS otros
;;; con el mismo estado y menor costo.
(defun elimina-peores (nodos abiertos cerrados)
  (loop for nodo in nodos
    when (and (not (esta-mejor nodo abiertos))
              (not (esta-mejor nodo cerrados)))
    collect nodo))

;;; (ESTA-MEJOR NODO LISTA-DE-NODOS)
;;; Valor:
;;;   - T, si el estado del NODO coincide con el estado de alguno de los nodos
;;;     de la LISTA-DE-NODOS y dicho nodo tiene menor costo;
;;;   - NIL, en caso contrario.
(defun esta-mejor (nodo lista-de-nodos)
  (let ((estado (estado nodo)))
    (loop for n in lista-de-nodos
      thereis (and (equalp estado (estado n))
                   (<= (costo-camino n) (costo-camino nodo))))))

;;; (ORDENA-POR-COSTO LISTA-DE-NODOS)
;;; Valor: Una lista cuyos elementos son los de la LISTA-DE-NODOS pero
;;;   ordenados de menor a mayor costo.
(defun ordena-por-costo (lista-de-nodos)
  (sort lista-de-nodos #'< :key #'costo-camino))

;;;****************************************************************************
;;; Procedimiento con traza
;;;****************************************************************************

(defun busqueda-optima-con-traza ()
  (let ((abiertos (list (crea-nodo-c :estado *estado-inicial*
                                     :camino nil
                                     :costo-camino 0)))
        (cerrados nil)
        (actual nil)
        (sucesores nil))                                             
    (loop until (null abiertos) do                                   
          (setf actual (first abiertos))                             
          (setf abiertos (rest abiertos))
          (setf cerrados (cons actual cerrados))
          (cond ((es-estado-final (estado actual))                   
                 (return actual))                                    
                (t (setf sucesores (nuevos-o-mejores-sucesores actual
                                                               abiertos
                                                               cerrados))
		   (informa actual sucesores)
                   (setf abiertos                                    
                         (ordena-por-costo
                           (append abiertos sucesores))))))))

(defun informa (actual sucesores)
  (format t "~&~a Costo: ~,2f~%"
	  (estado actual) (costo-camino actual))
  (loop for nodo in sucesores do
    (format t "~&  ~a Costo: ~,2f~%"
	  (estado nodo) (costo-camino nodo))))