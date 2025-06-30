;;; Verificador de soluciones
;;;============================================================================

(defun verifica (plan &optional (estado *estado-inicial*))
  (cond ((null estado)
         (format t "~& Movimiento no permitido~&")
         nil)
        ((null plan)
         (cond ((es-estado-final estado)
                (format t "~&~a Estado final~&" estado)
                t)
               (t (format t "~&~a No es estado final~&" estado)
                  nil)))
        (t (format t "~&~a ~a" estado (first plan))
           (verifica (rest plan) (aplica (first plan) estado)))))

;;;============================================================================