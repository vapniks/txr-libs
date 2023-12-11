;; Helper functions for handling functions & their arguments.

(defpackage arglistxtra (:fallback usr))
(in-package arglistxtra)

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

;; Given a list (e.g. a rest function argument), remove all keyword args and corresponding values from the list.
;; By default all symbols starting with : are treated as keyword args to remove, but if you want
;; to remove specific keywords then supply them as arguments after the list.
;; Note: if the final symbol in lst is a keyword it will be removed even though it has no corresponding value.
(defun remove-keyword-args (lst . keys)
  (let (retval)
    (while lst
      (let ((hd (car lst)))
	(if (or (and (not keys)
		     (keywordp hd))
		(memq hd keys))
	    (set lst (cddr lst))
	  (set lst (cdr lst)
	       retval (append retval (list hd))))))
    retval))


;; Function for making pattern functions easily accessible from lisp code.
;; Creates a lisp function which calls a given pattern function with the given arguments.
;; The new function also takes keyword arguments :input & :files for the <input> & <files> arguments
;; to match-fun.
;; Arguments:
;; patfunc - an unquoted pattern function symbol
;; retval - if non-nil then this will be the return value of the function, otherwise the return value
;;          is the same as for match-fun. This is useful if you are calling the function in an @(output) block
;;          in which case the return value should be a string to be inserted into the output.
;; newname - an unquoted symbol for the name of the new function, by default this is the original pattern function,
;;           with a * at the end, e.g. if the pattern function is mypatfunc, then by default the new function will
;;           be mypatfunc*
(defmacro defpatfun (patfunc : retval (newname (intern (join (symbol-name patfunc) "*"))))
  (if (match-fboundp patfunc)
      (with-gensyms (args input files rval)
		    ^(defun ,newname ,args
		       (let* ((,input (prop ,args :input))
			      (,files (prop ,args :files))
			      (,rval (match-fun ',patfunc
						(remove-keyword-args ,args :input :files)
						,input ,files)))
			 ,(or retval rval))))
    (error (join "No such pattern function: " (symbol-name patfunc)))))

