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
;; args supplied to each call, and report results.
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
		   ;;(format t "~^56a" "Function order for each mean stat (low to high)\n")
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

;; quickcheck calls "func" with random assignments to the arguments "args", and passes
;; the arguments & results of that call to the "pred" function/expression. If pred returns nil
;; then an error message is emitted. This is repeated n times.
;; "pred" can be either a function of 2 args, or a form in which @1/results will be replaced
;; by the results of the "func" call, and @2/args will be replaced by its arguments,
;; e.g. the following 3 values for pred yield the same results; 'equal, (equal @1 @2) & (equal results args)
;; If "printres" is non-nil then for each invocation of "func" the arguments and results
;; of that call are printed.
;; The args of func used in the macro call can contain random iterators which will be
;; replaced with correspoding values retrieved from those iterators before calling func.
;; This is done by testing the name of the symbols in args recursively. Any
;; form whose initial symbol name matches the regexp "rnd.*\*" will be replaced with a call
;; to "seq-next" on that iterator when func is called. The search is done recursively
;; so you can use lists of iterators, etc.
;; e.g. (rrev (rndrngs* (rndrngs*) (rndrngs*))) is replaced by
;; (apply 'rrev (list (seq-next (rndrngs* (seq-next (rndrngs*) nil) (seq-next (rndrngs*) nil)) nil)))

;; TODO - add shrink facility like Haskells quickcheck? (working on rndlists*)
;;        some way to check/ensure coverage of tests (functions to modify iterators?)

(defmacro quickcheck (n (func . args) : pred prinres)
  (labels ((swaparg (arg) (if (listp arg)
			      (if (and (symbolp (car arg))
				       (match-regex (symbol-name (car arg)) #/rnd.*\*/))
				  ^(seq-next (,(car arg) ,*(mapcar (fun swaparg) (cdr arg))) nil)
				  ^,(mapcar (fun swaparg) arg))
			    (if (and (symbolp arg)
				     (match-regex (symbol-name arg) #/rnd.*\*/))
				^(seq-next ,arg nil)
				arg))))
    (let ((newargs1 [swaparg args]))
      (with-gensyms (newargs2 results)
		    ^(dotimes (i ,n)
		       (let* ((,newargs2 (list ,*(mapcar (ret (if (symbolp @1) ^(quote ,@1) @1)) newargs1)))
			      (,results (apply ',func ,newargs2)))
			 ,(when prinres ^(format t "Args : ~a\nVal  : ~a\n\n" ,newargs2 ,results))
			 ,(when pred
			    ^(if (not [,(if (functionp pred) pred
					  (list 'ret ^(let ((results @1) (args @2)) ,pred)))
				      ,results ,newargs2])
				 (error "Predicate failed with args ~a" ,newargs2)))))))))

