;; Helper functions for handling function arguments.

(defpackage arglistxtra (:fallback usr))
(in-package arglistxtra)

;; Useful argument handling macros and functions
;; listtypep returns t if "lst" is a list of elements each of which has a supertype
;; listed (unquoted) in the remaining args, e.g: (listtypep '(1 #\a 3) number chr)
(defmacro listtypep (lst : . types)
  (with-gensyms (var)
		(let ((all (eq (car types) 'all)))
		  ^(let ((,var ,lst))
		     (and (listp ,var)
			  ,(if types
			       (if all
				   ^(all ,var (op some ',types (op typep @@1)))
				   ^(some ',types (op typep (car ,lst))))
			     t))))))
;; listlisttypep returns t if "lst" is a list of lists each of which contains elements
;; which have a supertype listed in the remaining args.
(defmacro listlisttypep (lst : . types)
  (with-gensyms (var)
		(let ((all (eq (car types) 'all))
		      (allall (eq (car types) 'allall)))
		  ^(let ((,var ,lst))
		     (and (listp ,var)
			  ,(if (or all allall) ^(all ,var 'listp)
			       ^(listp (car ,var)))
			  ,(if types
			       (if allall
				   ^(all ,var (op all @1 (op some ',types (op typep @@1 @1))))
				   ^(some ',types (op typep (caar ,var))))
			     t))))))

;; tolist checks if "arg" is a list, and if so either returns it unchanged,
;; or if "n" is supplied, pads it to length "n" with its final value.
;; If "arg" is not a list, a list of "n" copies of "arg" is returned,
;; or a single copy if "n" is not supplied.
(defun tolist (arg : n)
  (if (listp arg)
      (if n
	  (if (> n (len arg))
	      (pad arg (car (last arg)) (- n (len arg)))
	    (sub-list arg 0 n))
	arg)
    (repeat (list arg) (or n 1))))
;; tolistlist checks if "arg" is a list of lists, and if not converts it to one,
;; in an analogous manner to the tolist function (handling both atoms and lists).
(defun tolistlist (arg : n)
  (if (listlisttypep arg)
      (if n
	  (if (> n (len arg))
	      (pad arg (car (last arg)) (- n (len arg)))
	    (sub-list arg 0 n))
	arg)
    (repeat (list (tolist arg)) (or n 1))))
;; argtolist combines listtypep and tolist to check that "arg" contains the
;; right type of data, and pad it into a list of length n if necessary.
(defmacro argtolist (arg : n . types)
  (with-gensyms (lvar nvar)
		^(let ((,lvar ,arg)
		       (,nvar ,n))
		   (if (listtypep ,lvar . ,types)
		       (tolist ,lvar ,nvar)
		     (if (or ,(not types)
			     (some ',types (op typep ,lvar)))
			 (tolist ,lvar ,nvar)
		       (error "Invalid arg: ~a" ,lvar))))))
;; argtolistlist combines listlisttypep and tolistlist to check that "arg" contains the
;; right type of data, and pad it into a length n list of lists if necessary.
(defmacro argtolistlist (arg : n . types)
  (with-gensyms (lvar nvar)
		^(let ((,lvar ,arg)
		       (,nvar ,n))
		   (if (listlisttypep ,lvar . ,types)
		       (tolist ,lvar ,nvar)
		     (if (listtypep ,lvar . ,types)
			 (tolistlist ,lvar ,nvar)
		       (if (or ,(not types)
			       (some ',types (op typep ,lvar)))
			   (tolistlist (list ,lvar) ,nvar)
			 (error "Invalid arg: ~a" ,lvar)))))))

