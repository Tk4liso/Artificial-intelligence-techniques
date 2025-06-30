;;;****************************************************************************
;;; Problema de los misioneros
;;;****************************************************************************

(defun crea-estado (a b c d e)
  (list a b c d e))

(defun misioneros-oeste (estado)
  (first estado))

(defun canibales-oeste (estado)
  (second estado))

(defun misioneros-este (estado)
  (third estado)) 
 
(defun canibales-este (estado)
  (fourth estado))
(defun lado (estado)
  (fifth estado))

;;;****************************************************************************
;;; Estado inicial
;;;****************************************************************************

(defparameter *estado-inicial* 
  (crea-estado 3 3 0 0 'o))

;;;****************************************************************************
;;; Estado final
;;;****************************************************************************

(defparameter *estado-final*
  (crea-estado 0 0 3 3 'e))

(defun es-estado-final (estado)
  (equal estado *estado-final*))

;;;****************************************************************************
;;; Funciones auxiliares
;;;****************************************************************************

(defun opuesta (posicion)
  (cond ((eq posicion 'e) 'o)
	((eq posicion 'o) 'e)))
	
(defun es-seguro (estado)
  (when (or
			(and (>= (misioneros-oeste estado) (canibales-oeste estado))
				(>= (misioneros-este estado) (canibales-este estado)))
			(eq (misioneros-oeste estado) 0)
			(eq (misioneros-este estado) 0))
		estado))

;;;****************************************************************************
;;; Operadores
;;;****************************************************************************

(defparameter *operadores* 
  '(misionero-canibal-este
	misionero-canibal-oeste
	misionero-misionero-este
	misionero-misionero-oeste
	canibal-canibal-este
	canibal-canibal-oeste
	misionero-este
	misionero-oeste
	canibal-este
	canibal-oeste))

(defun misionero-canibal-este (estado)
	(when (and (> (misioneros-oeste estado) 0)
			(> (canibales-oeste estado) 0)
			(eq (lado estado) 'o))
  (es-seguro (crea-estado
			(-(misioneros-oeste estado)1)
			(-(canibales-oeste estado)1)
			(+(misioneros-este estado)1)
			(+(canibales-este estado)1)
			(opuesta(lado estado))))))
			
(defun misionero-canibal-oeste (estado)
	(when (and (>(misioneros-este estado)0)
			(>(canibales-este estado)0)
			(eq (lado estado) 'e))
  (es-seguro (crea-estado
			(+(misioneros-oeste estado)1)
			(+(canibales-oeste estado)1)
			(-(misioneros-este estado)1)
			(-(canibales-este estado)1)
			(opuesta(lado estado))))))
			
(defun misionero-misionero-este (estado)
	(when (and(>(misioneros-oeste estado)1)
			(eq (lado estado) 'o))
  (es-seguro (crea-estado
			(-(misioneros-oeste estado)2)
			(canibales-oeste estado)
			(+(misioneros-este estado)2)
			(canibales-este estado)
			(opuesta(lado estado))))))			

(defun misionero-misionero-oeste (estado)
	(when (and(>(misioneros-este estado)1)
			(eq (lado estado) 'e))
  (es-seguro (crea-estado
			(+(misioneros-oeste estado)2)
			(canibales-oeste estado)
			(-(misioneros-este estado)2)
			(canibales-este estado)
			(opuesta(lado estado))))))
			
(defun canibal-canibal-este (estado)
	(when (and(>(canibales-oeste estado)1)
			(eq (lado estado) 'o))
  (es-seguro (crea-estado
			(misioneros-oeste estado)
			(-(canibales-oeste estado)2)
			(misioneros-este estado)
			(+(canibales-este estado)2)
			(opuesta(lado estado))))))
			
(defun canibal-canibal-oeste (estado)
	(when (and(>(canibales-este estado)1)
			(eq (lado estado) 'e))
  (es-seguro (crea-estado
			(misioneros-oeste estado)
			(+(canibales-oeste estado)2)
			(misioneros-este estado)
			(-(canibales-este estado)2)
			(opuesta(lado estado))))))
			

(defun misionero-este (estado)
	(when (and(>(misioneros-oeste estado)0)
			(eq (lado estado) 'o))
  (es-seguro (crea-estado
			(-(misioneros-oeste estado)1)
			(canibales-oeste estado)
			(+(misioneros-este estado)1)
			(canibales-este estado)
			(opuesta(lado estado))))))			

(defun misionero-oeste (estado)
	(when (and(>(misioneros-este estado)0)
			(eq (lado estado) 'e))
  (es-seguro (crea-estado
			(+(misioneros-oeste estado)1)
			(canibales-oeste estado)
			(-(misioneros-este estado)1)
			(canibales-este estado)
			(opuesta(lado estado))))))
			
(defun canibal-este (estado)
	(when (and(>(canibales-oeste estado)0)
				(eq (lado estado) 'o))
  (es-seguro (crea-estado
			(misioneros-oeste estado)
			(-(canibales-oeste estado)1)
			(misioneros-este estado)
			(+(canibales-este estado)1)
			(opuesta(lado estado))))))
			
(defun canibal-oeste (estado)
	(when (and(>(canibales-este estado)0)
			(eq (lado estado) 'e))
  (es-seguro (crea-estado
			(misioneros-oeste estado)
			(+(canibales-oeste estado)1)
			(misioneros-este estado)
			(-(canibales-este estado)1)
			(opuesta(lado estado))))))		

(defun aplica (operador estado)
  (funcall (symbol-function operador) estado))
;;;============================================================================
