;;;; -*- Mode: Lisp -*-
    
(defun identificatore-p (lista uri-struct)
  (cond ((null lista) NIL)
	((or (equal (first lista) #\/)
	     (equal (first lista) #\?)
	     (equal (first lista) #\#)
	     (equal (first lista) #\@)
	     (equal (first lista) #\:))
	 (values lista uri-struct))
	(T (identificatore-p (rest lista)
			     (append uri-struct (list (first lista)))))))

(defun identificatore-host-p (lista uri-struct)
  (cond ((null lista) NIL)
	((or (equal (first lista) #\/)
	     (equal (first lista) #\.)
	     (equal (first lista) #\?)
	     (equal (first lista) #\#)
	     (equal (first lista) #\@)
	     (equal (first lista) #\:))
	 (values lista uri-struct))
	(T (append uri-structure (list (first lista)))
	   (identificatore-host-p (rest lista)
				  (append uri-structure (list (first lista)))))))

(defun digit-p (num)
  (if (not (characterp num))
      NIL
      (if (and (char<= num #\9)
	       (char>= num #\0))
	  T
	  NIL)))

(defun fragment-p (lista uri-struct)
  (cond ((null lista) NIL)
	((characterp (first lista))
	 (fragment-p (rest lista) (append uri-struct (list (first lista)))))
	(T (values lista uri-struct))))

(defun query-p (lista uri-struct)
  (cond ((null lista) NIL)
	((equal (first lista) #\#)
	 (values lista uri-struct))
	(T (query-p (rest lista) (append uri-struct (list (first lista)))))))

(defun path-p (lista uri-struct)
  (if (null lista)
      NIL
      (identificatore-p lista uri-struct)))

(defun ciclo-path (lista uri-struct)
  (multiple-value-bind (uri uri-struct)
      (is-it-a #\/ lista uri-struct :conserve t)
    (identificatore-p lista uri-struct)))

;;;;------------------------------------
(defun ip-p (lista)
  (setq lista (octect-p lista))
  ;(format t "~a~&" lista)
  (setq lista (is-it-a #\. lista))
  ;(format t "~a~&" lista)
  (setq lista (octect-p lista))
  ;(format t "~a~&" lista)
  (setq lista (is-it-a #\. lista))
  ;(format t "~a~&" lista)
  (setq lista (octect-p lista))
  ;(format t "~a~&" lista)
  (setq lista (is-it-a #\. lista))
  ;(format t "~a~&" lista)
  (setq lista (octect-p lista))
  ;(format t "~a~&" lista)
  )

(defun octect-p (lista)
  (cond ((or (and (digit-p (first lista))
		  (char<= (first lista) #\2)
		  (digit-p (second lista))
		  (char<= (second lista) #\5)
		  (digit-p (third lista))
		  (char<= (third lista) #\5))
	     (and (digit-p (first lista))
		  (char<= (first lista) #\2)
		  (digit-p (second lista))
		  (char<= (second lista) #\4))
	     (and (digit-p (first lista))
		  (char<= (first lista) #\1)
		  (digit-p (second lista))
		  (digit-p (third lista))))
	 (values (rest (rest (rest lista)))))
	((and (digit-p (first lista))
	      (char>= (first lista) #\1)
	      (digit-p (second lista)))
	 (values (rest (rest lista))))
	((digit-p (first lista))
	 (values (rest lista)))
	(T (values lista))))

(defun is-it-a (char lista uri-structure &key (conserve nil))
  (if (equal (first lista) char)
      (if conserve
          (values (rest lista) (append uri-structure (first lista)))
        (values (rest lista) uri-structure))
    (if conserve
        (values lista (append uri-structure (first lista)))
      (values lista uri-structure)))
  )

(defun domain-p (lista uri-structure)
  (identificatore-host-p lista (cons "host" uri-structure)))

(defun authority (lista)
  (setq lista (double-slash lista))
  (format t "~a~&" lista)
  (setq lista (userinfo lista))
  (format t "~a~&" lista)
  (setq lista (host-p lista))
  (format t "~a~&" lista)
  (setq lista (port-p lista))
  (format t "~a~&" lista)
  (values lista))

(defun userinfo (lista)
  (if (is-it-a #\@ (identificatore-p lista))
      (setq lista (identificatore-p lista))
      (setq lista (is-it-a #\@ lista)))
  (values lista))
  

(defun double-slash (lista uri-structure)
  (if (and (equal (first lista) #\/)
	   (equal (second lista) #\/))
      (values (rest (rest lista)))
      (values lista))
  (values uri-structure))

(defun port-p (lista)
  (if (null lista)
      NIL
      (ciclo-port lista))
  )

(defun ciclo-port (lista)
  (if (digit-p (first lista))
      (ciclo-port (rest lista))
      (values lista)
      ))

(defun uri-parser (lista)
  (setq lista (identificatore-p lista ()))
  (setq lista (is-it-a #\: lista))
  (setq lista (double-slash lista))
  (values lista))

(defun scheme-p (lista uri-structure)
  (identificatore-p lista (cons "scheme" uri-structure)))
