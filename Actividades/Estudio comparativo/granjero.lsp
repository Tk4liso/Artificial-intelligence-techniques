;;;****************************************************************************
;;; Problema del granjero
;;;****************************************************************************

(defun crea-estado (a b c d)
  (list a b c d))

(defun posicion-granjero (estado)
  (first estado))

(defun posicion-lobo (estado)
  (second estado))

(defun posicion-cabra (estado)
  (third estado))

(defun posicion-col (estado)
  (fourth estado))

;;;****************************************************************************
;;; Estado inicial
;;;****************************************************************************

(defparameter *estado-inicial* 
  (crea-estado 'i 'i 'i 'i))

;;;****************************************************************************
;;; Estado final
;;;****************************************************************************

(defparameter *estado-final*
  (crea-estado 'd 'd 'd 'd))

(defun es-estado-final (estado)
  (equal estado *estado-final*))

;;;****************************************************************************
;;; Funciones auxiliares
;;;****************************************************************************

(defun opuesta (posicion)
  (cond ((eq posicion 'i) 'd)
	((eq posicion 'd) 'i)))

(defun es-seguro (estado)
  (when (and (if (eq (posicion-lobo estado) (posicion-cabra estado))
		 (eq (posicion-granjero estado) (posicion-lobo estado))
		 t)
	     (if (eq (posicion-cabra estado) (posicion-col estado))
		 (eq (posicion-granjero estado) (posicion-cabra estado))
		 t))
	estado))

;;;****************************************************************************
;;; Operadores
;;;****************************************************************************

(defparameter *operadores* 
  '(pasa-granjero-solo
    pasan-granjero-lobo
    pasan-granjero-cabra
    pasan-granjero-col))

(defun pasa-granjero-solo (estado)
  (es-seguro (crea-estado
	      (opuesta (posicion-granjero estado))
	      (posicion-lobo estado)
	      (posicion-cabra estado)
	      (posicion-col estado))))

(defun pasan-granjero-lobo (estado)
  (when (eq (posicion-granjero estado) (posicion-lobo estado))
	(es-seguro (crea-estado
		    (opuesta (posicion-granjero estado))
		    (opuesta (posicion-lobo estado))
		    (posicion-cabra estado)
		    (posicion-col estado)))))

(defun pasan-granjero-cabra (estado)
  (when (eq (posicion-granjero estado) (posicion-cabra estado))
	(es-seguro (crea-estado
		    (opuesta (posicion-granjero estado))
		    (posicion-lobo estado)
		    (opuesta (posicion-cabra estado))
		    (posicion-col estado))))) 

(defun pasan-granjero-col (estado)
  (when (eq (posicion-granjero estado) (posicion-col estado))
	(es-seguro (crea-estado
		    (opuesta (posicion-granjero estado))
		    (posicion-lobo estado)
		    (posicion-cabra estado)
		    (opuesta (posicion-col estado))))))

(defun aplica (operador estado)
  (funcall (symbol-function operador) estado))

;;;============================================================================