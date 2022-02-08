;;;;da completare ciclo-host, host-p

(defun uri-parse (uri)
  (evaluate (init-struct uri)))

(defun evaluate (lista)
  (values (or (uri2 lista)
	      (uri1 lista))))

(defun uri2 (lista)
  NIL)

(defun uri1 (lista)
  (values (authority-block
	   (scheme-block lista))))

(defun scheme-block (lista)
  (close-node (or NIL
		  (is-it-a #\: (identificatore-p lista)))))

;; (defun authority-block (lista)
;;   (values (or NIL
;; 	      (port-block
;; 	       (host-block
;; 		(userinfo-block
;; 		 (double-slash lista)))))))

(defun authority-block (lista)
  (values (or NIL
	      (port-block
	       (host-block
		(userinfo-block
		 (double-slash lista)))))))

(defun identificatore-p (lista)
  "lista -> (uri-struct, uri)"
  (multiple-value-bind (uri-struct uri)
      (values (first lista) (second lista))
    (cond ((null uri)
	   (list uri-struct uri))
	  ((or (equal (first uri) #\/)
	       (equal (first uri) #\?)
	       (equal (first uri) #\#)
	       (equal (first uri) #\@)
	       (equal (first uri) #\:))
	   (list uri-struct uri))
	  (T (identificatore-p (list (append uri-struct (list (first uri)))
				     (rest uri)))))))

(defun identificatore-host-p (lista)
  "lista -> (uri-struct, uri)"
  (multiple-value-bind (uri-struct uri)
      (values (first lista) (second lista))
    (cond ((null uri)
	   (list uri-struct uri))
	  ((or (equal (first uri) #\/)
	       (equal (first uri) #\.)
	       (equal (first uri) #\?)
	       (equal (first uri) #\#)
	       (equal (first uri) #\@)
	       (equal (first uri) #\:))
	   (list uri-struct uri))
	  (T (identificatore-host-p (list (append uri-struct (list (first uri)))
				     (rest uri)))))))


(defun is-it-a (char lista)
  (multiple-value-bind (uri-struct uri)
      (values (first lista) (second lista))
    (if (equal char (first uri))
	(values (list uri-struct
		      (rest uri)))
	NIL)))

(defun double-slash (lista)
  (multiple-value-bind (uri-struct uri)
      (values (first lista) (second lista))
    (if (and (equal (first uri) #\/)
	     (equal (second uri) #\/))
	(list uri-struct (rest (rest uri))))))

(defun userinfo-block (lista)
  (close-node (or (is-it-a #\@ (identificatore-p lista))
		  lista)))

(defun host-block (lista)
  (close-node (or (indirizzo-p lista)
		  (host-p lista))))

(defun port-block (lista)
  (multiple-value-bind (uri-struct uri)
	  (values (first lista) (second lista))
    (if (null lista)
	NIL
	(if (null (port-p (is-it-a #\: lista)))
	    (list (append uri-struct '(#\8 #\0)) uri)
	    (close-node (port-p (is-it-a #\: lista)))))))

(defun port-p (lista)
  (if (null lista)
      NIL
      (multiple-value-bind (uri-struct uri)
	  (values (first lista) (second lista))
	(if (null uri)
	    (list uri-struct uri)
	    (if (digit-p (first uri))
		(port-p (list (append uri-struct (list (first uri))) (rest uri)))
		(list uri-struct uri))))))

(defun host-p (lista)
  (values (ciclo-host (identificatore-host-p lista))))

(defun ciclo-host (lista)
  (if (not (null (second lista)))
      (if (null (is-it-a #\. lista))
	  (values lista)
	  (ciclo-host (identificatore-host-p (is-it-a #\. lista))))
      (values lista)))

(defun digit-p (num)
  (if (not (characterp num))
      NIL
      (if (and (char<= num #\9)
	       (char>= num #\0))
	  T
	  NIL)))

(defun indirizzo-p (lista)
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
  (multiple-value-bind (uri-struct uri)
      (values (first lista) (second lista))
    (cond ((or (and (digit-p (first uri))
		    (char<= (first uri) #\2)
		    (digit-p (second uri))
		    (char<= (second uri) #\5)
		    (digit-p (third uri))
		    (char<= (third uri) #\5))
	       (and (digit-p (first uri))
		    (char<= (first uri) #\2)
		    (digit-p (second uri))
		    (char<= (second uri) #\4))
	       (and (digit-p (first uri))
		    (char<= (first uri) #\1)
		    (digit-p (second uri))
		    (digit-p (third uri))))
	   (values (list (append uri-struct (list (first uri) (second uri) (third uri)))
			 (rest (rest (rest uri))))))
	  ((and (digit-p (first uri))
		(char>= (first uri) #\1)
		(digit-p (second uri)))
	   (values (list (append uri-struct (list (first uri) (second uri)))
			 (rest (rest uri)))))
	  ((digit-p (first uri))
	   (values (list (append uri-struct (list (first uri)))
			 (rest uri))))
	  (T NIL))))

(defun insert-struct-node (lista node)
  (multiple-value-bind (uri-struct uri)
      (values (first lista) (second lista))
    (if (null uri-struct)
	(list node uri)
	(list (list uri-struct node) uri))))

(defun init-struct (uri)
  (if (null uri)
      NIL
      (list () (coerce uri 'list))))

(defun close-node (lista)
  (multiple-value-bind (uri-struct uri)
      (values (first lista) (second lista))
    (list (list uri-struct) uri)))

;; (defun uri-display (uri-structure)
;;   (format t "~{~{~a:~10t~a~%~}~%~}" uri-structure))
