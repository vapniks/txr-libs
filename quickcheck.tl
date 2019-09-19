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
	(nth (int-flo (floor (/ len 2))) lst)
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

;; quickcheck calls "func" with random assignments to the arguments "args", and passes
;; the arguments & results of that call to the "pred" function/expression. If pred returns nil
;; then an error message is emitted. This is repeated n times.
;; "pred" can be either a function of 2 args, or a form in which @1/results will be replaced
;; by the results of the "func" call, and @2/args will be replaced by its arguments,
;; e.g. the following 3 values for pred yield the same results; 'equal, (equal @1 @2) & (equal results args)
;; If "printres" is non-nil then for each invocation of "func" the arguments and results
;; of that call are printed.
;; The args of func used in the macro call can contain struct objects with supertype qc-type (e.g. qc-int, qc-string, etc).
;; The "next" member function of these objects will be used to generate new arguments to be tested.
;; e.g. (rrev (new (qc-range -10..10 -10..10))) will call rrev with random ranges whose endpoints are between -10 & 10.
;; Note: you can create your own qc struct types to use with quickcheck, but they must have qc-type as a supertype
;; and they must initialize the "iter" member to an iterator (see above).

;; TODO - add shrink facility like Haskells quickcheck, or better; use logical inference (minikanren/witness sets)
;;        based on boolean attributes derived from arguments.
(defmacro quickcheck (n (func . args) : pred prinres)
  (with-gensyms (newargs results excpt pass i err args2 nextfn)
		^(let (,newargs ,results ,excpt ,err
				(,nextfn (iff (op subtypep (typeof @1) 'qc-type)
					      (umeth next)
					      'identity))
				(,pass t) (,i 0)
				(,args2 (list ,*args)))
		   (while (and (< ,i ,n) ,pass)
		     (set ,newargs (mapcar ,nextfn ,args2)
			  ,results (catch* (apply ',func ,newargs) (error (e . a) e))
			  ,excpt (car (memq ,results
					    (remq t (uniq (flatten (exception-subtype-map))))))
			  ,pass (and (not ,excpt)
				     ,(if pred
					  ^(call ,(if (functionp pred)
						      pred
						    (list 'ret ^(let ((results @1) (args @2))
								  ,pred)))
						 ,results ,newargs)
					  t)))
		     ,(when prinres ^(format t "Args : ~a\nVal  : ~a\n\n" ,newargs ,results))
		     (inc ,i))
		   (when ,excpt
		     (error "Exception: ~a\nWhen function called with args: ~a"
			    ,excpt ,newargs))
		   ,(when pred
		      ^(when (not ,pass)
			 (error "Predicate failed when ~a called with args: ~a\nproducing results: ~a"
				',func ,newargs ,results))))))





