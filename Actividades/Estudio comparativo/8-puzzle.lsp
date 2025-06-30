;;;**************************************************************************
;;; Problema del 8-puzzle
;;;**************************************************************************

;;; Estado inicial

(defparameter *estado-inicial* 
  (make-array '(3 3) 
	      :initial-contents '((7 2 4)
				  (5 h 6)
				  (8 3 1))))

;;; Estado final

(defparameter *estado-final* 
  (make-array '(3 3) 
	      :initial-contents '((1 2 3)
				  (8 h 4)
				  (7 6 5))))

(defun es-estado-final (estado)
  (equalp estado *estado-final*))

;;; Funciones auxiliares

(defun copia-tablero (tablero)
  (let ((nuevo-tablero (make-array '(3 3))))
    (loop for i from 0 to 2
	  do (loop for j from 0 to 2 
		   do (setf (aref nuevo-tablero i j) 
			    (aref tablero i j) )))
    nuevo-tablero))

(defun coordenadas (bloque tablero)
  (loop for i from 0 to 2
	thereis (loop for j from 0 to 2 
		      thereis
		      (when (eq (aref tablero i j) bloque) 
			    (list i j)))))

;;; Operadores

(defparameter *operadores* 
  '(mover-izquierda
    mover-arriba
    mover-derecha
    mover-abajo))

(defun mover-izquierda (estado)
  (let* ((lugar-del-hueco (coordenadas 'h estado))
	 (i (first lugar-del-hueco)) 
	 (j (second lugar-del-hueco))
	 (nuevo-estado (copia-tablero estado)))
    (when (> j 0)
	  (setf (aref nuevo-estado i j)       (aref nuevo-estado  i (- j 1)))
	  (setf (aref nuevo-estado i (- j 1)) 'h)
	  nuevo-estado)))

(defun mover-arriba (estado)
  (let* ((lugar-del-hueco (coordenadas 'h estado))
	 (i (first lugar-del-hueco)) 
	 (j (second lugar-del-hueco))
	 (nuevo-estado (copia-tablero estado)))
    (when (> i 0)
	  (setf (aref nuevo-estado i j) (aref nuevo-estado (- i 1) j))
	  (setf (aref nuevo-estado (- i 1) j) 'h)
	  nuevo-estado)))

(defun mover-derecha(estado)
  (let* ((lugar-del-hueco (coordenadas 'h estado))
	 (i (first lugar-del-hueco)) 
	 (j (second lugar-del-hueco))
	 (nuevo-estado (copia-tablero estado)))
    (when (< j 2)
	  (setf (aref nuevo-estado i j) (aref nuevo-estado  i (+ j 1)))
	  (setf (aref nuevo-estado  i (+ j 1)) 'h)
	  nuevo-estado)))

(defun mover-abajo (estado)
  (let* ((lugar-del-hueco (coordenadas 'h estado))
	 (i (first lugar-del-hueco)) 
	 (j (second lugar-del-hueco))
	 (nuevo-estado (copia-tablero estado)))
    (when (< i 2)
	  (setf (aref nuevo-estado i j) (aref nuevo-estado (+ i 1) j))
	  (setf (aref nuevo-estado (+ i 1) j) 'h)
	  nuevo-estado)))

(defun aplica (operador estado)
  (funcall (symbol-function operador) estado))

;;; Costo

(defun costo-de-aplicar-operador (estado operador)
  (declare (ignore estado operador))
  1)

;;; Heur�sticas

(defun heuristica (estado)
  (heuristica-1 estado))

(defun heuristica-1 (estado)
  (loop for i from 1 to 8
	counting (not (equal (coordenadas i estado)
			     (coordenadas i *estado-final*)))))

(defun heuristica-2 (estado)
  (loop for i from 1 to 8
	summing (distancia-manhattan (coordenadas i estado)
				     (coordenadas i *estado-final*))))

(defun distancia-manhattan (c1 c2)
  (+ (abs (- (first c1) (first c2)))
     (abs (- (second c1) (second c2)))))
