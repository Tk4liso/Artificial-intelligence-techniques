;;; Búsqueda en profundidad iterativa

;;; Funciones y variables dependientes del problema

;;; Se supone que se han definido
;;; 1. La variable *ESTADO-INICIAL* que contiene el estado inicial.
;;; 2. La función (ES-ESTADO-FINAL ESTADO) que determina si el ESTADO es un
;;;    estado final.
;;; 3. La variable *OPERADORES* que contiene la lista de operadores.
;;; 4. Para cada OPERADOR la función (OPERADOR ESTADO-ACTUAL) que devuelve
;;;    el estado obtenido aplicando el OPERADOR al ESTADO-ACTUAL, si el
;;;    OPERADOR es aplicable y NIL, en caso contrario.

;;; Representación de los nodos

;;; Los nodos se componen de un estado y un camino (que es la lista de
;;; operadores que para transformar el estado inicial en el actual, escrita en
;;; orden inverso)

;;; NODO
;;; Es una estructura con constructor CREA-NODO y campos ESTADO y CAMINO.
(defstruct (nodo (:conc-name nil)
		 (:constructor crea-nodo))
  estado
  camino)

;;; Procedimiento de búsqueda en profundidad iterativa

(defun busqueda-en-profundidad-iterativa (&key (cota-inicial 5))
  (loop for n from cota-inicial
	thereis (busqueda-en-profundidad-acotada :cota n)))

(load "bprofundidadacotada" :verbose nil)

;;; Ejemplo de sesión

;;; > (load "jarras.lsp")
;;; T
;;; > (load "bprofundidaditerativa.lsp")
;;; T
;;; > (busqueda-en-profundidad-iterativa)
