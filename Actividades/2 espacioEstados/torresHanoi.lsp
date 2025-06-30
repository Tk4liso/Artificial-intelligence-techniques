;;;****************************************************************************
;;; Problema de las Torres de Hanoi
;;;****************************************************************************

(defun crea-estado (a b c)
    (list  a b c) )

(defun A (estado)
    (first estado) )

(defun B (estado)
    (second estado) )

(defun C (estado)
    (third estado) )
 
  
(defun disco (poste)
    (first poste) )

;;; Estado inicial

(defparameter *estado-inicial*
    (crea-estado  '(1 2 3 4 5) '() '() )  )
   
   
;;; Estado final

(defun es-estado-final (estado)
    (when (= 3 (length (B estado)) )
        estado ) )
        

;;; Operadores

(defparameter *operadores*
    '(mov-AB
      mov-AC
      mov-BC )   )
      
;;;Movimiento de A a B o viceversa (si es posible)
(defun mov-AB (estado)
    (let ( (a (A estado))  (b (B estado))  (c (C estado)) )
        (cond ( (and  (= 0 (length a))  (= 0 (length b)))
                    nil)
                    
              ( (and  (= 0 (length a))  (< 0 (length b)))
                    (setq estado (crea-estado (cons   (disco b) a)
                                              (remove (disco b) b)
                                              c)  ))
                    
                    
              ( (and  (< 0 (length a))  (= 0 (length b)))
                    (setq estado (crea-estado (remove (disco a) a)
                                              (cons   (disco a) b)
                                              c)  ))
                                        
              ( (and  (< 0 (length a))  (< 0 (length b)))
                    (if  (< (disco a) (disco b))
                         (setq estado (crea-estado (remove (disco a) a)
                                                   (cons   (disco a) b)
                                                   c) )
                                                   
                         (setq estado (crea-estado (cons   (disco b) a)
                                                   (remove (disco b) b)
                                                   c) )   )))))
     
;;;Movimiento de A a C o viceversa (si es posible)
(defun mov-AC (estado)
    (let ( (a (A estado)) (b (B estado)) (c (C estado)))
        (cond ( (and  (= 0 (length a))  (= 0 (length c)))
                    nil)
                    
              ( (and  (= 0 (length a))  (< 0 (length c)))
                    (setq estado (crea-estado (cons   (disco c) a)
                                              b
                                              (remove (disco c) c)   )  ))
                    
                    
              ( (and  (< 0 (length a))  (= 0 (length c)))
                    (setq estado (crea-estado (remove (disco a) a)
                                              b
                                              (cons   (disco a) c)   )  ))
                                        
              ( (and  (< 0 (length a))  (< 0 (length c)))
                    (if  (< (disco a) (disco c))
                         (setq estado (crea-estado (remove (disco a) a)
                                                   b
                                                   (cons   (disco a) c) ))
                                                   
                         (setq estado (crea-estado (cons   (disco c) a)
                                                   b
                                                   (remove (disco c) c) ))   )))))
                                              
                                              
;;;Movimiento de B a C o viceversa (si es posible)

(defun mov-BC (estado)
    (let ( (a (A estado)) (b (B estado)) (c (C estado)))
        (cond ( (and  (= 0 (length b))  (= 0 (length c)))
                    nil)
                    
              ( (and  (= 0 (length b))  (< 0 (length c)))
                    (setq estado (crea-estado a
                                              (cons   (disco c) b)
                                              (remove (disco c) c)   )  ))
                    
                    
              ( (and  (< 0 (length b))  (= 0 (length c)))
                    (setq estado (crea-estado a
                                              (remove (disco b) b)
                                              (cons   (disco b) c)   )  ))
                                        
              ( (and  (< 0 (length b))  (< 0 (length c)))
                    (if  (< (disco b) (disco c))
                         (setq estado (crea-estado a
                                                   (remove (disco b) b)
                                                   (cons   (disco b) c) ))
                                                   
                         (setq estado (crea-estado a
                                                   (cons   (disco c) b)
                                                   (remove (disco c) c) ))  )))))
                                              
                                              
(defun aplica (operador estado)
    (funcall (symbol-function operador) estado) )