(defun uri-parse (lista)
  (values (or (uri2 lista)
	      (uri1 lista))))

(defun uri2 (lista)
  NIL)

(defun uri1 (lista)
  (values (authority-block
	   (scheme-block lista))))

(defun scheme-block (lista)
  (values (or NIL
	      (is-it-a #\: (identificatore-p lista)))))

;; (defun authority-block (lista)
;;   (values (or NIL
;; 	      (port-block
;; 	       (host-block
;; 		(userinfo-block
;; 		 (double-slash lista)))))))

(defun authority-block (lista)
  (values (or NIL
	      (userinfo-block
	       (double-slash lista)))))

(defun identificatore-p (lista)
  "lista -> (uri-struct, uri)"
  (multiple-value-bind (uri-struct uri)
      (values (first lista) (second lista))
    (cond ((null uri) NIL)
	  ((or (equal (first uri) #\/)
	       (equal (first uri) #\?)
	       (equal (first uri) #\#)
	       (equal (first uri) #\@)
	       (equal (first uri) #\:))
	   (list uri-struct uri))
	  (T (identificatore-p (list (append uri-struct (list (first uri)))
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
  (values (or (is-it-a #\@ (identificatore-p lista))
	      lista)))

(defun host-block (lista)
  (values (or (indirizzo-p lista)
	      (host-p lista))))

(defun port-block (lista)
  (if (null lista)
      NIL
      (values (or (port-p (is-it-a #\: lista))
		  80))))

(defun port-p (lista)
  NIL)

(defun host-p (lista)
  NIL)

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
	   (values (list (append uri-struct (first uri) (second uri) (third uri))
			 (rest (rest (rest lista))))))
	  ((and (digit-p (first uri))
		(char>= (first uri) #\1)
		(digit-p (second uri)))
	   (values (list (append uri-struct (first uri) (second uri))
			 (rest (rest lista)))))
	  ((digit-p (first uri))
	   (values (list (append uri-struct (first uri))
			 (rest lista))))
	  (T NIL))))
