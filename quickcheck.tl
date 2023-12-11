;; Various macros and functions for testing & profiling code


(unless (find-package "randseq") (load "randseq"))
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

;; Useful for debugging macros
(defmacro showenv (: (e1 nil x) :env e0)
  ^(labels ((s (e)
	       (when e
		 (let ((f (env-fbindings e))
		       (v (env-vbindings e))
		       (n (env-next e)))
		   (when f (format t "fbindings: ~a\n" f))
		   (when v (format t "vbindings: ~a\n" v))
		   (when n (format t "next env: ~a\n" n))
		   (s n)))))
     ,(if x
	  (if e1 ^(s ',e1) ^(pprinl "Empty env"))
	(if e0 ^(s ',e0) ^(pprinl "Empty env")))))

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

;; Run an sexp multiple times and return means & standard deviations of timings & memory usage.
;; Arguments:
;; iters - number of times to run code being profiled.
;; print - whether or not to print the results in a formatted table.
;; reps - number of times that body should be repeated within each iteration (default = 1).
;;        This is useful for profiling fast builtin functions.
;; body - the code to be profiled.
;; The final means and standard deviations are calculated from the timings & memory usage
;; of each iteration, not each repetition.
(defmacro profn (iters reps print . body)
  (with-gensyms (times mbytes gcbytes res)
		^(let* (,times ,mbytes ,gcbytes ,res)
		   ,(if reps
			^(dotimes (i ,iters)
			   (set ,res (prof (dotimes (j ,reps) ,*body)))
			   (lpush ,mbytes ,gcbytes ,times
				  (sub-list ,res 1 4)))
			^(dotimes (i ,iters)
			   (lpush ,mbytes ,gcbytes ,times
				  (sub-list (prof ,*body) 1 4))))
		   (when ,print
		     (format t "~12a   ~15a ~15a\n" "Quantity" "Mean" "Stdev")
		     (mapdo (op format t "~12a : ~15,6f ~15,6f\n"
				@1 (apply 'mean @2) (apply 'stdev @2))
			    '("Runs" "Time (ms)" "Malloc-bytes" "gc-bytes")
			    (list (pad nil ,iters ,iters) ,times ,mbytes ,gcbytes)))
		   (tuples 2 (maprod (op apply @2 @1)
				     (list ,times ,mbytes ,gcbytes)
				     '(mean stdev))))))

;; Function to print a table containing some data.
;; TODO: output formats - orgmode, html, latex, markdown: https://www.markdownguide.org/extended-syntax#tables
;; TODO: use outputtables.txr for this
;; TODO: handle data arg in csvdata struct form
(defun printtbl (:key data -- (format "org") title colnames rownames ostream transpose)
  (unless (match-fboundp 'orgTable)
    (load "outputtables.txr"))
  (let ((maxwidths (mapcar (op apply 'max (mapcar (opip tostring length) @1))
			   data))
	(tblfunc (intern (join format "Table"))))
    (when colnames (upd maxwidths (op mapcar 'max @1 colnames)))
    (if (match-fboundp tblfunc)
	(match-fun tblfunc (list data title colnames rownames ostream))
      (error (format nil "Unable to find ~a function" tblfunc)))
    )
  )


;; Call a function multiple times with different sets of arguments, and return means & standard deviations
;; of timings & memory usage.
;; Arguments:
;; iters - number of times to run code being profiled for each set of arguments.
;; print - whether or not to print the results in a formatted table.
;; reps - number of times that the function should be called within each iteration (default = 1).
;;        This is useful for profiling fast builtin functions.
;; fn - the function being profiled (either a symbol or an sexp which evaluates to a function)
;; args - lists of arguments to be tested & compared in results.
;; (defmacro profargs (iters reps print fn . args)
;;   (mapcar (profn ??)
;; 	  args)
;;   )

;; Run the profiler multiple times on each function in fnlst (an unquoted list) with the same
;; args supplied to each call, and report means & standard deviations of timings & memory usage.
;; Arguments:
;; iters - number of times to run code being profiled.
;; fnlist - a list of functions (either symbols or sexps which evaluate to functions).
;; print - whether or not to print the results in a formatted table.
;; reps - number of times that each function call should be repeated within each iteration (default = 1).
;;        This is useful for profiling fast builtin functions.
;; The final means and standard deviations are calculated from the timings & memory usage
;; of each iteration, not each repetition.
(defmacro proffns (iters reps fnlst . args)
  (with-gensyms (stats cwidths hline cmplxs fnnames)
		^(let* ((,stats (list ,*(mapcar
					 (ret ^(profn ,iters ,reps nil
						      (apply ,(if (symbolp @1)
								  ^(quote ,@1)
								  @1)
							     (list ,*args))))
					 fnlst)))
			(,fnnames ',(mapcar (ret (if (symbolp @1) (symbol-name @1)
						   (append "func" (tostring @2))))
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
			      (mapdo (op format t "~*a | " @1 (fmt "(~,2f)" (nth 1 @2))) ,cwidths s)
			      (format t "\n~a\n" ,hline)))
			  '("time (ms)" "malloc-bytes" "gc-bytes") '(0 1 2))
		   (format t "~a" ,hline)
		   (mapdo (ret (progn
				 (format t "\n~18a : " @2)
				 (mapdo (ret (format t "~*a | " @1 (and @2 (fmt "~,1f" @2))))
					,cwidths
					(mapcar (op prop @1 @@1) ,cmplxs))))
			  '(:nlines :nfrms :maxdepth)
			  '("No. of lines" "No. of forms" "Max depth"))
		   (format t "\n~a\n" ,hline))))

;; Run proffns separately for each element of args. Each element of args should be an
;; unquoted list of arguments to use for that particular run.
(defmacro proffns* (iters reps fnlst . args)
  ^(progn ,*(mapcar (lambda (arg) ^(proffns ,iters ,reps ,fnlst . ,arg)) args)))

(defun fncomplexity (fn)
  (let* ((fn2 (if (symbolp fn) (symbol-function fn) fn))
	 (frm (if (interp-fun-p fn2) (func-get-form fn2) fn2)))
    (when (listp frm)
      (list :nlines (length (spl "\n" (tostring frm)))
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

;; TODO: struct for random files/dirs/symlinks/etc?

(defmacro quotesym (sym) (if (symbolp sym) ^(quote ,sym) sym))

;; func-get-info returns a list of the following information about its function or otherwise callable argument:
;; its type: regex, seq (a list, string, lazy string or vector), hash, interpreted or intrinsic (function)
;; number of required args, number of optional args, and whether or not the function is variadic (allows any number of args).
;; Note: the returned info might not be accurate for intrinsic functions created from combinators, e.g. (andf 'seqp 'listp)
;; In that case use the "numargs" macro instead.
(defun func-get-info (func)
  (pprinl "FUNC-GET-INFO") ;;DEBUG
  (showenv) ;;DEBUG
  (typecase func
    (null (error "Invalid arg: ~a" func))
    (sequence (list 'seq 1 1 nil))
    (hash (list 'hash 1 1 nil))
    (regex (list 'regex 1 2 nil))
    (t (let* ((func2 (if (functionp func) func
		       (if (symbolp func) (symbol-function func)
			 (error "Invalid arg: ~a" func)))))
	 (format t "func: ~a\n(typeof func): ~a\nfunc2: ~a\n"
		 func (typeof func) func2) ;;DEBUG
	 (if (interp-fun-p func2)
	     (let ((src (func-get-form func)))
	       ;; handle cases like (op rangep) which appear to be variadic but aren't really:
	       (if (symbolp (cadr src))
		   (let* ((d (caddr src))
			  (fn (caddr d)))
		     (if (and (consp d)
			      (eq 'dwim (car d))
			      (eq 'sys:apply (cadr d)))
			 (func-get-info (if (listp fn) (eval fn) fn)) ;eval is for lambdas
		       (list 'interpreted 0 0 t)))
		 (let* ((args (cadr src))
			(parts (partition* (butlastn 0 args) (op posq :))))
		   (list 'interpreted (if (eq (car args) :) 0 (len (car parts)))
			 (len (call (if (eq (car args) :) 'car 'cadr) parts))
			 (true (cdr (last args)))))))
	   (if (functionp func2)
	       (let ((p (fun-fixparam-count func2))
		     (o (fun-optparam-count func2)))
		 (list 'intrinsic (- p o) o (fun-variadic func2)))
	     (error "Invalid arg: ~a" func)))))));; TODO: return nil if func2 isn't a function?

;; numargs takes a lisp form or symbol and returns a list containing the following information:
;; 1) The input type, one of - list (e.g. (op less 5)), lfun (flet function), var (let bound variable),
;; symacro (a symbol macro), sym (a symbol - could be a macro, function or dynamically bound variable),
;; qsym (a quoted symbol), anon (a lambda form), macro (a normal macro), special (a special operator such as defvarl),
;; seq (a (lazy) string or vector), hash or regex.
;; 2) The number of required args when treated as a function.
;; 3) The number of optional args (if this is negative then the function is invalid.
;; 4) Whether or not the function is variadic (i.e. whether or not it accepts any number of args).
;;
;; If any of these values are negative numbers then the function is invalid, e.g. (andf 'rangep 'print)
;; (rangep & print have different numbers of arguments), or (op wrap @1) (wrap requires 3 arguments).

;; Note: this function makes heuristic guesses by examining the first arg of any list input.
;; It should be correct for most cases, but if the input is a variable or symbol macro bound to an intrinsic function
;; then numargs cannot know if the function was created from a combinator such as andf, orf, etc.
;; In such cases it will be reported as a variadic function with no required or optional args,
;; even if there may be required args in reality (note that macros op, do, lop & ldo create interpreted functions
;; and so they should be reported correctly in this case).

;; TODO: builtin macros such as; flet labels defparm
;; TODO: why doesn't this work: (flet ((fn1 (a b) a)) (symacrolet ((fn2 fn1)) (numargs fn2)))
;;       - because (fun fn2) wont work because fun is a macro?
;; TODO: return more info about how to call function, and whether its intrinsic or interpreted
(defmacro numargs (frm :env e)
  (pprinl "NUMARGS");;DEBUG
  (eval ^(showenv ,e));;DEBUG
  (typecase frm
    (sym (let ((llc (lexical-lisp1-binding e frm)))
	   (caseq llc
		  (:symacro ^(let* ((frm2 (equot ,frm))
				    (llc2 (lexical-lisp1-binding ,e frm2))) ;;TODO fix documentation after fixing this
			       (format t "frm2: ~a\n" frm2);;DEBUG
			       (typecase frm2
			       	 (list (pprinl "symacro list") ;DEBUG
				       (ifa (functionp (ignerr (eval frm2)))
					    (if (interp-fun-p it)
						(cons 'symacro (cdr (func-get-info it)))
					      (cons 'symacro (cdr (eval ^(numargs ,frm2)))))
					    '(symacro 1 1 nil)))
			       	 (sym (format t "symacro sym, type: ~a\n" llc2) ;DEBUG
				      ;; (flet ((fn (a b : c d e . f) a)) (symacrolet ((fn2 fn)) (numargs fn2)))
				      ;;** (expr-1298:1) unbound variable randseq:fn
				      ;;** during evaluation of form (functionp randseq:fn)
				      ;;** ... an expansion of (functionp randseq:fn2)
				      (cons 'symacro
					    (cdr (func-get-info
						  (let ((frm3 (eval ^(fun ,frm2) ,e)))
						    (format t "frm3: ~a, (typeof frm3): ~a\n" frm3 (typeof frm3));;DEBUG
						    (caseq llc2
							   (:symacro (format t "symacro\n")
								     (equot frm2))
							   (:var (format t "var\n")
								 ;; try to extract frm2 from env?
								 frm3)
							   (:fun (format t "fun\n")
								 frm3)
							   (t frm2)))))))
			       	 (sequence (pprinl "symacro sequence") ;DEBUG
					   '(symacro 1 1 nil))
			       	 (hash (pprinl "symacro hash") ;DEBUG
				       '(symacro 1 1 nil))
			       	 (regex (pprinl "symacro regex") ;DEBUG
					'(symacro 1 2 nil))
			       	 (t (pprinl "symacro t") ;DEBUG
				    '(symacro 0 0 nil)))))
		  (:var ^(cons 'var (cdr (func-get-info ,frm))))
		  (:fun ^(cons 'lfun (cdr (func-get-info (fun ,frm)))))
		  (t (if (macro-form-p ^(,frm) e) ;TODO: fix this for defvar, defparm & similar cases if possible
			 (catch (progn (macroexpand-1 ^(,frm) e)
				       ^(list 'macro 0 0 nil))
			   (error (err)
				  (let* ((str (search-regst err #/params \([^()]*\)/))
					 (args (when str (split-str (sub-str str 7) " ")))
					 (parts1 (when args (partition* args (op posqual "."))))
					 (parts2 (when args (partition* (car parts1) (op posqual ":")))))
				    ^(list 'macro ,(len (car parts2)) ,(len (cadr parts2))
					   ,(true (cadr parts1))))))
		       (if (special-operator-p frm)
			   '(list 'special 0 0 nil) ;TODO: figure out params of special operators if possible
			 ^(let ((i (func-get-info
				    ,(if (catch (functionp (eval frm))
					   (error (err) nil))
					 frm ^(quotesym ,frm)))))
			    (list 'sym (second i) (third i) (fourth i)))))))))
    (fun ^(let ((i (func-get-info ,frm))) (list 'fun (second i) (third i) (fourth i))))
    (list (symacrolet
	   ((fst (car frm)) (snd (cadr frm))
	    (i2 '(second i)) (i3 '(third i)) (i4 '(fourth i)))
	   (labels ((fn (x) (if (listp x)
				(if (and (eq (car x) 'sys:var)
					 (integerp (cadr x)))
				    (cadr x)
				  (unless (eq (car x) 'sys:expr) (maxarg x)))
			      nil))
		    (maxarg (lst) (let ((l (remq nil (mapcar (fun fn) lst))))
				    (when l (apply 'max l)))))
	     (cond ((memq fst '(f^$ f^ f$ dup)) '(list 'list 1 0 nil))
		   ((eq fst 'flipargs) '(list 'list 2 0 nil))
		   ((memq fst '(aret retf)) '(list 'list 0 0 t))
		   ((memq fst '(do ldo)) ^(list 'list ,(or (maxarg frm) 1) 0 t))
		   ((memq fst '(lop op))
		    ^(let* ((i ',(eval (macroexpand-1 ^(numargs ,snd) e))) ;TODO - make this a function in labels?
			    (m ,(maxarg frm))
			    (s ,(- (len frm) 2))
			    (tot (+ ,i2 ,i3))
			    (req (if m (if (< m ,i2) (- m ,i2) m)
				   (if (<= s ,i2)
				       (- ,i2 s)
				     (if (or ,i4 (<= s tot))
					 0
				       (- tot s)))))
			    (opt (if m 0 (let ((o (- tot s req)))
					   (if ,i4 (max o 0) o)))))
		       (list 'list req opt ,i4)))
		   ((memq fst '(juxt notf iff iffi chain chand opip oand fun))
		    ^(cons 'list (cdr (numargs ,snd))))
		   ((memq fst '(apf ipf))
		    ^(let ((i ',(eval (macroexpand-1 ^(numargs ,snd) e)))) ;TODO - make this a function in labels?
		       (list 'list 1 (- (+ (second i) (third i)) 1) ,i4)))
		   ((memq fst '(ap ip))
		    ^(let ((i ',(eval (macroexpand-1 ^(numargs ,snd) e)))) ;TODO - make this a function in labels?
		       (list 'list 1 (- (or ,(maxarg frm)
					    (max 0 (- ,i2 ,(- (len frm) 2))))
					1)
			     ,i4)))
		   ((memq fst '(ado ido))
		    ^(let ((i ',(eval (macroexpand-1 ^(numargs ,snd) e)))) ;TODO - make this a function in labels?
		       (list 'list 1 ,(if (maxarg frm) (- (maxarg frm) 1) 0) t)))
		   ((eq fst 'callf)
		    ^(let* ((info (remq nil
					(list (when ,(third frm) (numargs ,(third frm)))
					      (when ,(fourth frm) (numargs ,(fourth frm)))
					      (when ,(fifth frm) (numargs ,(fifth frm)))
					      (when ,(sixth frm) (numargs ,(sixth frm))))))
			    (req (apply 'max (mapcar 'cadr info)))
			    (opt (apply 'min (mapcar
					      (ret (let ((n (- (+ (cadr @1) (caddr @1)) req)))
						     (if (cadddr @1) (max 0 n) n)))
					      info))))
		       (list 'list req opt (true (all (mapcar 'cadddr info))))))
		   ((memq fst '(andf orf))
		    ^(let* ((info (remq nil
					(list (when ,(second frm) (numargs ,(second frm)))
					      (when ,(third frm) (numargs ,(third frm)))
					      (when ,(fourth frm) (numargs ,(fourth frm)))
					      (when ,(fifth frm) (numargs ,(fifth frm)))
					      (when ,(sixth frm) (numargs ,(sixth frm))))))
			    (req (apply 'max (mapcar 'cadr info)))
			    (opt (apply 'min (mapcar
					      (ret (let ((n (- (+ (cadr @1) (caddr @1)) req)))
						     (if (cadddr @1) (max 0 n) n)))
					      info))))
		       (list 'list req opt (true (all (mapcar 'cadddr info))))))
		   ((eq fst 'mapf) ^(list 'list 1 ,(max 0 (- (len frm) 3)) t))
		   ((eq fst 'ret) ^(list 'list ,(or (maxarg frm) 0) 0 t))
		   ((eq fst 'quote) ^(let ((i (func-get-info ,frm))) (list 'qsym ,i2 ,i3 ,i4)))
		   ((eq fst 'lambda) ^(let ((i (func-get-info ,frm))) (list 'anon ,i2 ,i3 ,i4)))
		   (t '(list 'list 0 0 nil))))))
    (t ^(func-get-info ,frm))))

;; applyargs takes a function (or quoted symbol for a function, or variable or symbol macrolet containing
;; a function), and some arguments for that function, and then applies fn to the appropriate number of arguments,
;; e.g. (applyargs 'wrap 1 5 9 10 20) will apply the 'wrap function to args 1, 5 & 9, but ignore 10 & 20 because
;; wrap takes just 3 args.
(defmacro applyargs (fn :env e1 . args)
  (with-gensyms (nargs type r o v args2)
		^(let* ((,nargs (numargs ,fn))
			(,args2 (if (fourth ,nargs)
				    (list ,*args)
				  (sub (list ,*args) 0 (+ (second ,nargs) (third ,nargs))))))
		   (apply (caseq (car ,nargs) ;; lfun var sym macro - symacro qsym anon other
				 ;; Note: "when" forms are used to prevent warnings on macro expansion
				 (lfun ,(when (and (symbolp fn)
						   (not (macro-form-p fn e1)))
					  ^(fun ,fn)))
				 (var ,(when (and (symbolp fn)
						  (not (macro-form-p fn e1)))
					 ^(op ,fn)))
				 (sym ,(when (symbolp fn) ^(quotesym ,fn))) ;; op prevents warnings
				 ;; TODO!!! (have tried in vain. Is it possible to handle macros properly?)
				 ;; add macro to environment of further call? see notes from Kaz
				 (macro (error "Cannot handle macro argument!"))
				 (t ,fn))
			  ,args2))))

;; quickcheck calls "func" with random assignments to the arguments "args", and passes
;; the arguments & results of that call to the "pred" function/expression. If pred returns nil
;; then an error message is emitted. This is repeated n times.

;; "pred" can be one of the following:
;; - An unquoted symbol for a dynamic or lexically scoped function of 1 arg (return value of func),
;;   or 2 args (return value & args of func).
;; - A regular expression (treated as a function of 1 arg; the results of calling func).
;; - A form whose first element is one of these symbols; op do lop ap ip ado ido opip oand ret dup chain chand
;;   andf orf notf iff iffi apf ipf callf f^$ f^ f$ lambda fun. In this case quickcheck will try to guess if this
;;   represents a function of 1 or 2 args by looking at the 2nd or 3rd element of the form. If it thinks it is a
;;   function of 1 arg it will pass it the return value of func, otherwise it will pass both the return value and
;;   args of func.
;; - A form in which @1/results will be replaced by the results of the "func" call, and @2/args will be replaced by
;;   its arguments. 
;; - A symbol macro which expands to a form of the above type.
;; - A lexical variable
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
;;        This article about shrinking might be useful: https://hypothesis.works/articles/integrated-shrinking/
;;        and this one: https://hypothesis.works/articles/types-and-properties/
(defmacro quickcheck (n (func . args) : predfn prinres :env e)
  (with-gensyms (newargs results excpt pass i args2 getargs fninfo type nargs p)
		^(labels ((,getargs (args)
				    (typecase args
				      (qc-type (call (fun ,getargs) args.(next)))
				      (list (mapcar (fun ,getargs) args))
				      (vec (mapcar (fun ,getargs) args))
				      (t args))))
		   (format t "Testing ~a\n" ',func)
		   (let* (,newargs ,results ,excpt
				   (,pass t) (,i 0)
				   (,args2 ,(typecase args
					      (sym args)
					      (list ^(list ,*args))))
				   (,fninfo (numargs ,predfn))
				   (,type (car ,fninfo))
				   (,nargs (+ (second ,fninfo) (third ,fninfo)))
				   (,p (caseq ,type
					      ;; Note: "when" forms are used to prevent warnings on macro expansion
					      (lfun ,(when (and (symbolp predfn)
								(not (macro-form-p predfn e)))
						       ^(fun ,predfn)))
					      (sym ,(when (symbolp predfn) ^(quotesym ,predfn)))
					      (var ,(when (and (symbolp predfn)
							       (not (macro-form-p predfn e)))
						      ^(op ,predfn))) ;; op prevents warnings
					      ((list symacro qsym anon)
					       (if (and (eq ,nargs 0)
							(not (fourth ,fninfo)))
						   (ret (let ((results @1)
							      (args @2))
							  ,(when (or (listp predfn)
								     (eq (lexical-lisp1-binding e predfn)
									 :symacro))
							     predfn)))
						 (let ((results 1) (args 2))
						   ,(when (listp predfn) predfn))))
					      (t ,(when (regexp predfn) predfn)))))
		     (while (and (< ,i ,n) ,pass)
		       (set ,newargs (,getargs ,args2)
			    ,results (catch* (apply ',func ,newargs) (error (e . a) e))
			    ,excpt (car (memq ,results
					      (remq t (uniq (flatten (exception-subtype-map))))))
			    ,pass
			    (and (not ,excpt)
				 ;; TODO; use applyargs instead of cond
				 ;; if blah blah then let results = ,results & args = ,newargs before calling applyargs?
				 ;;(applyargs ,predfn ,results ,newargs)
				 (cond ((or (> ,nargs 1) (fourth ,fninfo)
					    (and (not (fourth ,fninfo))
						 (= ,nargs 0)
						 (memq ,type '(list symacro))))
					(call ,p ,results ,newargs))
				       ((> ,nargs 0) (call ,p ,results))
				       (t t))

				 ))
		       ,(when prinres ^(format t "Args : ~a\nVal  : ~a\n\n" ,newargs ,results))
		       (inc ,i))
		     (when ,excpt
		       (error "Exception: ~a\nWhen ~a called with args: ~a"
			      ,excpt ',func ,newargs))
		     ,(when predfn
			^(when (not ,pass)
			   (error "Predicate failed when ~a called with args: ~a\nproducing results: ~a"
				  ',func ,newargs ,results)))))))


