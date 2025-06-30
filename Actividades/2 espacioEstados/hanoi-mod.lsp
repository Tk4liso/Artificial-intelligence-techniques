; Torres de hanoi

(defun crea-estado(a b c)
	(list a b c))
(defun poste-izquierdo(estado)
	(first estado))
(defun poste-centro(estado)
	(second estado))
(defun poste-derecho(estado)
	(third estado))

; Estado inicial
	(defparameter *estado-inicial*
		(crea-estado '(1 2 3 4 5) '() '()))
; Estado final
	(defparameter *estado-final*
		(crea-estado '() '() '(1 2 3 4 5)))
	
	(defun es-estado-final(estado)
		(equal estado *estado-final*))

; Funciones auxiliares


; Operadores
(defparameter *operadores*
	'(mover-iz-ce
	  mover-iz-de
	  mover-ce-iz
	  mover-ce-de
	  mover-de-iz
	  mover-de-ce))

(defun mover-iz-ce(estado)
	(when(null(eq (poste-izquierdo estado) nil))
		(when(or (eq (poste-centro estado) nil) (> (first(poste-centro estado)) (first(poste-izquierdo estado))) 
		    )
			(crea-estado
				(rest(poste-izquierdo estado))
				(append (list (first (poste-izquierdo estado))) (poste-centro estado))
				(poste-derecho estado)
			)
		)
	)
)

(defun mover-iz-de(estado)
	(when(null(eq (poste-izquierdo estado) nil))
		(when(or (eq (poste-derecho estado) nil) (> (first(poste-derecho estado)) (first(poste-izquierdo estado))) 
		    )
		(crea-estado
				(rest(poste-izquierdo estado))
				(poste-centro estado) 
				(append (list (first (poste-izquierdo estado))) (poste-derecho estado))
			)
		)
	)
)

(defun mover-ce-iz(estado)
	(when(null(eq (poste-centro estado) nil))
		(when(or (eq (poste-izquierdo estado) nil) (> (first(poste-izquierdo estado)) (first(poste-centro estado))) 
		    )
			(crea-estado
				(append (list (first (poste-centro estado))) (poste-izquierdo estado))
				(rest(poste-centro estado))
				(poste-derecho estado)
			)
		)
	)
)

(defun mover-ce-de(estado)
	(when(null(eq (poste-centro estado) nil))
		(when(or (eq (poste-derecho estado) nil) (> (first(poste-derecho estado)) (first(poste-centro estado))) 
		    )
			(crea-estado
				(poste-izquierdo estado)
				(rest(poste-derecho estado))
				(append (list (first (poste-centro estado))) (poste-derecho estado))
			)
		)
	)
)

(defun mover-de-iz(estado)
	(when(null(eq (poste-derecho estado) nil))
		(when(or (eq (poste-izquierdo estado) nil) (> (first(poste-izquierdo estado)) (first(poste-derecho estado))) 
		    )
			(crea-estado
				(append (list (first (poste-derecho estado))) (poste-izquierdo estado))
				(poste-centro estado)
				(rest(poste-derecho estado))
			)
		)
	)
)

(defun mover-de-ce(estado)
	(when(null(eq (poste-derecho estado) nil))
		(when(or (eq (poste-centro estado) nil) (> (first(poste-centro estado)) (first(poste-derecho estado))) 
		    )
			(crea-estado
				(poste-izquierdo estado)
				(append (list(first (poste-derecho estado))) (poste-centro estado))
				(rest(poste-derecho estado))	
			)
		)
	)
)

(defun coste-de-aplicar-operador (estado operador)
	1
)

(defun heuristica (estado)
	(+ (length (poste-centro estado)) (length (poste-derecho estado)))
)


(defun aplica(operador estado)
	(funcall(symbol-function operador)estado))
