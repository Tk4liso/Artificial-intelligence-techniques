;;;########################################################## 
;;;##              Problema de las jarras                  ##
;;;##########################################################

(defun crea-estado (x y)
  (list x y))

(defun contenido-jarra4 (estado)
  (first estado))

(defun contenido-jarra3 (estado)
  (second estado))

;;; Estado inicial

(defparameter *estado-inicial* 
  (crea-estado 0 0))

;;; Estados finales


(defun es-estado-final (estado)
  (= 2 (contenido-jarra4 estado)))

;;; Operadores

(defparameter *operadores* 
  '(llenar-jarra4
    llenar-jarra3
    vaciar-jarra4
    vaciar-jarra3 
    llenar-jarra4-con-jarra3
    llenar-jarra3-con-jarra4
    vaciar-jarra3-en-jarra4
    vaciar-jarra4-en-jarra3))

(defun llenar-jarra4 (estado)
  (when (< (contenido-jarra4 estado) 4)
	(crea-estado 4
		     (contenido-jarra3 estado))))

(defun llenar-jarra3 (estado)
  (when (< (contenido-jarra3 estado) 3)
	(crea-estado (contenido-jarra4 estado)
		     3)))

(defun vaciar-jarra4 (estado)
  (when (> (contenido-jarra4 estado) 0)
	(crea-estado 0
		     (contenido-jarra3 estado))))

(defun vaciar-jarra3 (estado)
  (when (> (contenido-jarra3 estado) 0)
	(crea-estado (contenido-jarra4 estado)
		     0)))

(defun llenar-jarra4-con-jarra3 (estado)
  (let ((y (contenido-jarra3 estado))
	(x (contenido-jarra4 estado)))
    (when (and (> y 0)
	       (< x 4)
	       (> (+ y x ) 4)) 
	  (crea-estado 4 (- y (- 4 x))))))

(defun llenar-jarra3-con-jarra4 (estado)
  (let ((x (contenido-jarra3 estado))
	(y (contenido-jarra4 estado)))
    (when (and (> y 0)
	       (< x 3)
	       (> (+ y x) 3)) 
	  (crea-estado (- y (- 3 x)) 3)))) 

(defun vaciar-jarra3-en-jarra4  (estado)
  (let ((x (contenido-jarra3 estado))
	(y (contenido-jarra4 estado)))
    (when (and (> x 0)
	       (<= (+ y x) 4))
	  (crea-estado (+ x y) 0))))
    
(defun vaciar-jarra4-en-jarra3 (estado)
  (let ((x (contenido-jarra3 estado))
	(y (contenido-jarra4 estado)))
    (when (and (> y 0)
	       (<= (+ y x) 3))
	  (crea-estado 0 (+ x y)))))

(defun aplica (operador estado)
  (funcall (symbol-function operador) estado))