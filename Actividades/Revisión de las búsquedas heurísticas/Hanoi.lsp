;;;****************************************************************************
;;; Representaci�n de estados: Torres de Hanoi
;;;****************************************************************************

(defun crea-estado (a b c)
  (list a b c ))


(defun poste1 (estado)
  (first estado))

(defun poste2 (estado)
  (second estado))

(defun poste3 (estado)
  (third estado))

;;;****************************************************************************
;;; Estado inicial
;;;****************************************************************************

(defparameter *estado-inicial* 
  (crea-estado '(1 2 3 4 5 6 7 8 9 10) '() '()))

(defparameter *estado-final* 
  (crea-estado '() '() '(1 2 3 4 5 6 7 8 9 10)))

(defun es-estado-final (estado)
  (equal estado *estado-final*))

;;;****************************************************************************
;;; Funciones auxiliares
;;;****************************************************************************


(defun es-seguro1 (estado)
  (let ((primero (first (poste1 estado)))
	      (segundo (second (poste1 estado)) ) )
  (when (if (and (not (eq primero nil))(not (eq segundo nil))) (< primero segundo)
	    t )
	t )))


(defun es-seguro2 (estado)
  (let ((primero (first (poste2 estado)))
	      (segundo (second (poste2 estado)) ) )	
  (when (if (and (not (eq primero nil))(not (eq segundo nil))) (< primero segundo)
	    t )
	t )))



(defun es-seguro3 (estado)
  (let ((primero (first (poste3 estado)))
	      (segundo (second (poste3 estado)) ) )	
  (when (if (and (not (eq primero nil))(not (eq segundo nil))) (< primero segundo)
	    t )
	t )))


(defun es-seguro (estado)
  (when (and (and  (es-seguro1 estado) (es-seguro2 estado)) (es-seguro3 estado))
	estado))

;;;****************************************************************************
;;; Operadores
;;;****************************************************************************

(defparameter *operadores* 
  '(pasa-1-2
    pasa-1-3
    pasa-2-1
    pasa-2-3
    pasa-3-1
    pasa-3-2))





(defun pasa-1-2 (estado)
  (let ((elemento (first (poste1 estado)))
	(complemento (poste2 estado)) )
   (when (not (eq elemento nil))
  	(es-seguro (crea-estado
		    (rest (poste1 estado))
		    (cons elemento complemento)
		    (poste3 estado)) ) )))


(defun pasa-1-3 (estado)
  (let ((elemento (first (poste1 estado)))
	(complemento (poste3 estado)) )
   (when (not (eq elemento nil))
  	(es-seguro (crea-estado
		    (rest (poste1 estado))
		    (poste2 estado)
		    (cons elemento complemento)) ) )))


(defun pasa-2-1 (estado)
  (let ((elemento (first (poste2 estado)))
	(complemento (poste1 estado)) )
   (when (not (eq elemento nil))
  	(es-seguro (crea-estado
		    (cons elemento complemento)
		    (rest (poste2 estado))
		    (poste3 estado)) ) )))

(defun pasa-2-3 (estado)
  (let ((elemento (first (poste2 estado)))
	(complemento (poste3 estado)) )
   (when (not (eq elemento nil))
  	(es-seguro (crea-estado
		    (poste1 estado)
		    (rest (poste2 estado))
		    (cons elemento complemento)) ) )))


(defun pasa-3-1 (estado)
  (let ((elemento (first (poste3 estado)))
	(complemento (poste1 estado)) )
   (when (not (eq elemento nil))
  	(es-seguro (crea-estado
		    (cons elemento complemento)
		    (poste2 estado)
		    (rest (poste3 estado))) ) )))

(defun pasa-3-2 (estado)
  (let ((elemento (first (poste3 estado)))
	(complemento (poste2 estado)) )
   (when (not (eq elemento nil))
  	(es-seguro (crea-estado
		    (poste1 estado)
		    (cons elemento complemento)
		    (rest (poste3 estado))) ) )))


(defun aplica (operador estado)
  (funcall (symbol-function operador) estado))

;;;============================================================================

;;; Costo

(defun costo-de-aplicar-operador (estado operador)
  (declare (ignore estado operador))
  1)

;;; Heur�sticas

(defun heuristica (estado)
(+ (length (poste1 estado))) (length (poste2 estado)) )


