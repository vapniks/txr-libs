;; Various macros and functions for testing & profiling code


(load "randseq")
(defpackage quickcheck (:fallback usr randseq))
(in-package quickcheck)

(defmacro error-to-sym (expr) ^(catch ,expr (error (cond) :error)))

(defmacro vtest (:env env expr expected)
  (catch (let ((expr-expn (macroexpand expr env))
	       (expval (gensym)))
	   ^(let ((,expval ,expected))
	      (ifa (not (equal (error-to-sym ,expr-expn) ,expval))
		   (error "test case ~s failed: produced ~s; expected ~s"
			  ',expr it ,expval))))
    (error (exc)
	   (unless (eq expected :error)
	     (error "test case ~s failed to expand: expected is ~s" expr expected)))))

(defmacro test (expr expected) ^(vtest ,expr ',expected))

(defmacro stest (expr expected) ^(vtest ,^(tostring ,expr) ,expected))

(defmacro mtest (. pairs) ^(progn ,*(mapcar (op cons 'test) (tuples 2 pairs))))

;; some basic stats functions
(defun mean args (/ (sum args) (length args)))

(defun median args
  (let ((len (length args))
	(lst (sort args)))
    (if (oddp len)
	(nth (floor len 2) lst)
      (/ (+ (nth (int-flo (/ len 2)) lst)
	    (nth (int-flo (- (/ len 2) 1)) lst))
	 2))))

(defun var args (let ((m (apply 'mean args)))
		  (/ (sum (mapcar (opip (- @1 m) square) args))
		     (- (len args) 1))))

(defun stdev args (sqrt (apply 'var args)))

;; Like lset but from pushing to places.
;; Pushes values obtained from final arg (a list), into places in initial args.
(defmacro lpush (. places-source)
  (let ((places (butlast places-source))
        (source (last places-source))
        (orig (gensym))
        (iter (gensym)))
    (unless places
      (compile-error '(lpush) "Require one or more places followed by expression"))
    ^(let* ((,orig ,(car source))
            (,iter ,orig))
       ,*(butlast (mappend (ret ^((push (car ,iter) ,@1) (set ,iter (cdr ,iter))))
                           places))
       ,orig)))

;; Run the profiler on body n times, and if print is t print mean & variances of profiling info.
(defmacro profn (n print . body)
  (with-gensyms (times mbytes gcbytes res)
		^(let* (,times ,mbytes ,gcbytes ,res)
		   (dotimes (i ,n)
		     (set ,res (prof ,*body))
		     (lpush ,mbytes ,gcbytes ,times (sub-list ,res 1 4)))
		   (when ,print
		     (format t "~12a   ~15a ~15a\n" "Quantity" "Mean" "Stdev")
		     (mapdo (op format t "~12a : ~15,1f ~15,1f\n"
				@1 (apply 'mean @2) (apply 'stdev @2))
			    '("Runs" "Time (ms)" "Malloc-bytes" "gc-bytes")
			    (list (pad nil ,n ,n) ,times ,mbytes ,gcbytes)))
		   (tuples 2 (maprod (op apply @2 @1)
				     (list ,times ,mbytes ,gcbytes)
				     '(mean stdev))))))

;; Run the profiler n times on each function in fnlst (an unquoted list) with the same
;; args supplied to each call, and report results. The elements of fnlst can be symbols
;; representing functions, lambda forms, or other forms which return functions (op, ip, ret, etc).
(defmacro proffns (n fnlst . args)
  (with-gensyms (stats cwidths hline cmplxs fnnames)
		^(let* ((,stats (list ,*(mapcar (ret ^(profn ,n nil
							     (apply ,(if (symbolp @1)
									 ^(quote ,@1)
									 @1)
								    (list ,*args))))
						fnlst)))
			(,fnnames ',(mapcar (ret (if (symbolp @1) (symbol-name @1)
						   (cat-str (list "func" (tostring @2)))))
					    fnlst (range 1 (len fnlst))))
			(,cwidths (mapcar (op apply max (length @1)
					      (mapcar (opip tostring length) (flatten @2))) 
					  ,fnnames ,stats))
			(,hline (mkstring (apply '+ 21 (- (* (len ,cwidths) 3) 1) ,cwidths) #\-))
			(,cmplxs ',(mapcar 'fncomplexity fnlst)))
		   (format t "Args: ~a\n" (list ,*args))
		   (pprinl ,hline)
		   (format t "~18a : " "Function")
		   (mapdo (op format t "~*a | ") ,cwidths ,fnnames)
		   (format t "\n~a\n" ,hline)
		   (mapdo (lambda (statname i)
			    (let* ((s (mapcar (op nth i @1) ,stats))
				   (best (car (grade (mapcar 'car s)))))
			      (format t "~18a : " (fmt "Mean ~a" statname))
			      (dotimes (j (length s))
				(format t "~*a | " (nth j ,cwidths) (fmt (if (= j best) "*~a" "~a")
									 (nth 0 (nth j s)))))
			      (format t "\n~18a : " (fmt "Stdev ~a" statname))
			      (mapdo (op format t "~*a | " @1 (fmt "(~,1f)" (nth 1 @2))) ,cwidths s)
			      (format t "\n~a\n" ,hline)))
			  '("time (ms)" "malloc-bytes" "gc-bytes") '(0 1 2))
		   (format t "~a" ,hline)
		   (mapdo (ret (format t "\n~18a : " @2)
			       (mapdo (ret (format t "~*a | " @1 (and @2 (fmt "~,1f" @2))))
				      ,cwidths
				      (mapcar (op prop @1 @@1) ,cmplxs)))
			  '(:nlines :nfrms :maxdepth)
			  '("No. of lines" "No. of forms" "Max depth"))
		   (format t "\n~a\n" ,hline))))

;; Run proffns separately for each element of args. Each element of args should be an
;; unquoted list of arguments to use for that particular run.
(defmacro proffns* (n fnlst . args)
  ^(progn ,*(mapcar (lambda (arg) ^(proffns ,n ,fnlst . ,arg)) args)))

(defun fncomplexity (fn)
  (let* ((fn2 (if (symbolp fn) (symbol-function fn) fn))
	 (frm (if (interp-fun-p fn2) (func-get-form fn2) fn2)))
    (when frm (list :nlines (length (spl "\n" (tostring frm)))
		    :nfrms (- (length (spl "(" (tostring frm))) 1)
		    :nsyms (length (flatcar frm))
		    :maxdepth (maxdepth frm)
		    :avgdepth (apply 'mean (formdepths frm))
		    :avgsamedepth (apply 'mean (runlengths (formdepths frm)))))))

;; Turn a cons cell into a list, e.g. '(1 . 2) -> '(1 2)
(defun uncons (lst) (let ((l (last lst))) (when (cdr l) (set (last lst) (list (car l) (cdr l)))) lst))

(defun maxdepth (form)
  (+ 1 (apply 'max (mapcar (iffi 'atom (ret 0) 'maxdepth) (uncons form)))))

(defun symdepths (form)
  (mapcar (op + 1) (mappend (iffi 'atom (ret '(0)) 'symdepths) (uncons form))))

(defun formdepths (form)
  (cons 1 (mapcar (op + 1) (mappend (iffi 'atom (ret nil) 'formdepths) (uncons form)))))

(defun runlengths (lst) (mapcar 'length (partition-by 'identity lst)))

(defstruct qc-type nil
	   iter
	   (:method next (self) (seq-next self.iter nil)))

(defstruct (qc-int : range) qc-type
	   (range 0..9)
	   (:postinit (self) (set self.iter (rndints* self.range))))

(defstruct (qc-float : range) qc-type
	   (range 0..1)
	   (:postinit (self) (set self.iter (rndfloats* self.range))))

(defstruct qc-bool qc-type
	   (:postinit (self) (set self.iter (rndbools*))))

(defstruct (qc-range : fr tr len dir step inf) qc-type
	   (fr 0..10) (tr 0..10) len dir step (inf t)
	   (:postinit (self) (set self.iter (rndrngs* self.fr self.tr self.len
						      self.dir self.step self.inf))))

(defstruct (qc-elem incl : excl) qc-type
	   incl excl
	   (:postinit (self) (set self.iter (rndelems* self.incl self.excl))))

(defstruct (qc-char : incl excl) qc-type
	   (incl #\0..#\z) excl
	   (:postinit (self) (set self.iter (rndchars* self.incl self.excl))))

(defstruct (qc-string : arg1 arg2 arg3 filterx) qc-type
	   arg1 arg2 arg3 filterx
	   (:postinit (self) (set self.iter (rndstrings* self.arg1 self.arg2
							 self.arg3 self.filterx))))

(defstruct (qc-regex : lengths maxd allowed regex okchr) qc-type
	   (lengths 1..20) (maxd 5) allowed regex okchr
	   (:postinit (self) (set self.iter (rndregexs* self.lengths self.maxd
							self.allowed self.regex self.okchr))))

(defstruct (qc-list seq : n uniq) qc-type
	   seq (n 1..10) uniq
	   (:postinit (self) (set self.iter (rndlists* self.seq self.n self.uniq))))

(defstruct (qc-vector seq : n uniq) qc-type
	   seq (n 1..10) uniq
	   (:postinit (self) (set self.iter (rndvecs* self.seq self.n self.uniq))))

(defmacro quotesym (sym) (if (symbolp sym) ^(quote ,sym) sym))

;; quickcheck calls "func" with random assignments to the arguments "args", and passes
;; the arguments & results of that call to the "pred" function/expression. If pred returns nil
;; then an error message is emitted. This is repeated n times.

;; "pred" can be one of the following:
;; - an unquoted symbol for a function of 1 arg (return value of func) or 2 args (return value & args of func)
;; - a regular expression (treated as a function of 1 arg; the results of calling func)
;; - a form whose first element is one of these symbols; op do lop ap ip ado ido opip oand ret dup chain chand
;;   andf orf notf iff iffi apf ipf callf f^$ f^ f$ lambda fun. In this case quickcheck will try to guess if this
;;   represents a function of 1 or 2 args by looking at the 2nd or 3rd element of the form. If it thinks it is a
;;   function of 1 arg it will pass it the return value of func, otherwise it will pass both the return value and
;;   args of func.
;; - a form in which @1/results will be replaced by the results of the "func" call, and @2/args will be replaced by
;;   its arguments. Alternatively, an symbol macro that is equivalent to such a form.
;; e.g. the following values for pred yield the same results; equal, (equal @1 @2) & (equal results args)
;; and so do these; #/foo/ (search-regst @1 #/foo/) (search-regst results #/foo/) (op search-regst @1 #/foo/)
;; and these; (andf 'listp (opip len (< 3))) (and (listp results) (< 3 (len results))) (and (listp @1) (< 3 (len @1)))
;; and these; (mapf 'eq 'len 'len) (eq (len results) (len args)) (eq (len @1) (len @2))
;;
;; If "printres" is non-nil then for each invocation of "func" the arguments and results of that call are printed.
;;
;; The args of func used in the macro call can contain struct objects with supertype qc-type (e.g. qc-int, qc-string, etc).
;; The "next" member function of these objects will be used to generate new arguments to be tested, and this process
;; is applied recursively (so you can use a qc-list struct that returns a list of qc-int structs for example).
;; You can use dotten notation to apply the function being tested to a list of arguments,
;; e.g. to test the rmin function with random lists of ranges:
;;
;; (let ((ranges (new (qc-list (list (new (qc-range))) 1..5)))) (quickcheck 10 (rmin . ranges) numberp))
;;
;; Note: you can create your own qc struct types to use with quickcheck, but they must have qc-type as a supertype
;; and they must initialize the "iter" member to an iterator (see above).

;; TODO - add shrink facility like Haskells quickcheck, or better; use logical inference (minikanren/witness sets)
;;        based on boolean attributes derived from arguments.
(defmacro quickcheck (n (func . args) : predfn prinres :env e)
  (let ((hfns1 '(f^$ f^ f$ dup apf ipf ap ip ado ido))
	(hfns2 '(mapf lop op do fun ret lambda))
	(hfns?a '(oand andf orf notf iff iffi chain chand))
	(hfns?b '(opip))
	(hfns?c '(callf)))
    (with-gensyms (newargs results excpt pass i err args2 getargs predfn2)
		  ^(labels ((,getargs (args)
				      (typecase args
					(qc-type (call (fun ,getargs) args.(next)))
					(list (mapcar (fun ,getargs) args))
					(vec (mapcar (fun ,getargs) args))
					(t args)))
			    (,predfn2 (results args)
				      ,(flet ((oneargp (fn)
						       (let* ((f (tostring fn))
							      (n (int-str (search-regst f #/[0-9]+/)))
							      (v (search-regst f #/variadic/)))
							 (not (or (> n 1) v)))))
					 (typecase predfn
					   (regex ^(search-regst results ,predfn))
					   (sym (let ((llb (lexical-lisp1-binding e predfn)))
						  (if (memq llb '(:symacro :var))
						      predfn
						    (if (oneargp (eval ^(fun ,predfn) e))
							^(call (fun ,predfn) results)
							^(call (fun ,predfn) results args)))))
					   (list (cond ((memq (car predfn) hfns1)
							^(call ,predfn results))
						       ((memq (car predfn) hfns2)
							^(call ,predfn results args))
						       ((memq (car predfn) hfns?a)
							(if (and (symbolp (cadr predfn))
								 (oneargp (eval ^(symbol-function ,(cadr predfn)))))
							    ^(call ,predfn results)
							    ^(call ,predfn results args)))
						       ((memq (car predfn) hfns?b)
							(if (and (symbolp (cadr predfn))
								 (oneargp (eval ^(fun ,(cadr predfn)))))
							    ^(call ,predfn results)
							    ^(call ,predfn results args)))
						       ((memq (car predfn) hfns?c)
							(if (and (symbolp (caddr predfn))
								 (oneargp (eval ^(symbol-function ,(caddr predfn)))))
							    ^(call ,predfn results)
							    ^(call ,predfn results args)))
						       (t predfn)))
					   (t t)))))
		     (format t "Testing ~a\n" ',func)
		     (let (,newargs ,results ,excpt ,err
				    (,pass t) (,i 0)
				    (,args2 ,(typecase args
					       (sym args)
					       (list ^(list ,*args)))))
		       (while (and (< ,i ,n) ,pass)
			 (set ,newargs (,getargs ,args2)
			      ,results (catch* (apply ',func ,newargs)
					       (error (e . a) e))
			      ,excpt (car (memq ,results
						(remq t (uniq (flatten (exception-subtype-map))))))
			      ,pass (and (not ,excpt) (,predfn2 ,results ,newargs)))
			 ,(when prinres ^(format t "Args : ~a\nVal  : ~a\n\n" ,newargs ,results))
			 (inc ,i))
		       (when ,excpt
			 (error "Exception: ~a\nWhen ~a called with args: ~a"
				,excpt ',func ,newargs))
		       ,(when predfn
			  ^(when (not ,pass)
			     (error "Predicate failed when ~a called with args: ~a\nproducing results: ~a"
				    ',func ,newargs ,results))))))))

;; func-get-info returns a list of the following information about its function argument:
;; the function type (interpreted or intrinsic), number of required args, number of optional args,
;; and whether or not the function is variadic (allows any number of args).
;; Note: if the function arg is variadic then the 3rd element of the returned list will be 0,
;; and the 4th will be "t".
(defun func-get-info (func)
  (let* ((f (typecase func
	      (string func)
	      (fun (tostring func))
	      (sym (tostring (symbol-function func)))
	      (t (error "Invalid arg: ~a" func))))
	 (type (cond ((search-regex f #/interpreted/) 'interpreted)
		     ((search-regex f #/intrinsic/) 'intrinsic)
		     (t 'unknown)))
	 (variadic (not (null (search-regex f #/variadic/))))
	 (l (search-regst f (regex-compile "lambda +(\\([^()<>]+\\)|[^()<>\\s]+)")))
	 (l2 (when l (sub-str l 7)))
	 (p (search-regst f #/[0-9]+ +param/))
	 (o (search-regst f #/[0-9]+ +optional/))
	 reqparams optparams variadic)
    (if l2
	(if (search-regst l2 #/nil/)
	    (set reqparams 0 optparams 0 variadic nil)
	  (if (search-regst l2 #/\(/)
	      (let* ((parts1 (partition* (split-str l2 " ") (op posqual ".")))
		     (parts2 (partition* (car parts1) (op posqual ":"))))
		(set reqparams (len (car parts2))
		     optparams (len (cadr parts2))
		     variadic (not (null (cadr parts1)))))
	    (set reqparams 0 optparams 0 variadic t)))
      (set reqparams (if p (int-str (sub-str p nil -6)) 0)
	   optparams (if o (int-str (sub-str o nil -9)) 0)
	   variadic (not (null (search-regex f #/variadic/)))))
    (list type reqparams optparams variadic)))


