;; Functions for creating random sequences of objects of different types.
;; For each type dealt with, e.g. ints, 3 functions will be created:
;;   rndints  - creates an infinite lazy list of random integers
;;   rndint   - create a single random integer
;;   rndints* - creates an iterator of random integers that can be accessed with seq-next
;; Read the documentation below for further details (function arguments etc.)

(load "seqxtra")
(load "arglistxtra")
(defpackage randseq (:fallback usr seqxtra arglistxtra))
(in-package randseq)

;; Create lazy list of random integers/floats in range given by r argument.
;; r can be a 3-element range representing an arithmetic progression, e.g. 0..10..2
;; (even numbers between 0 & 10). If r is a single number or a range whose start & end
;; are the same, then just return an infinite list of copies of that number.
(defun rndints (: (r 0..9))
  (if (numberp r)
      (pad nil r)
    (if (= (to r) (from r))
	(pad nil (to r))
      (let* ((hastep (rangep (to r)))
	     (fr (if hastep (from r)
		   (min (from r) (to r))))
	     (tr (if hastep (to r)
		   (max (from r) (to r))))
	     (step (when hastep (to tr)))
	     (mod (if hastep (int-flo (floor (+ 1 (/ (- (from tr) fr) step))))
		    (+ 1 (- tr fr)))))
	(if hastep (gun (+ fr (* (rand mod) step)))
	  (gun (+ fr (rand mod))))))))
;; random floating point numbers in range r
(defun rndfloats (: (r 0..1))
  (let ((fr (from r)) (tr (to r)))
    (gun (+ fr (* (random-float) (- tr fr))))))

;; normal random variates with a given mean & variance
(defun rndnorms (: (mean 0) (var 1))
  (mapcar* (opip inv-cum-norm (* (sqrt var)) (+ mean)) (rndfloats)))

;; random choices from t & nil
(defun rndbools () (gen (ret t) (> (rand 2) 0)))

;; Returns lazy list of ranges. Arguments:
;; fr/tr = range of allowed values for start/end of ranges
;; len   = range of allowed lengths of ranges (can also be a single number)
;; dir   = if +ve/-ve number then only increasing/decreasing ranges returned
;; step  = optional step size for ranges; can be an integer for a fixed step size,
;;         or a range for step sizes chosen uniformely at random within the range,
;; inf   = if "inf" is non-nil and "len" is nil, then ":" will be allowed as the start/endpoint of ranges.
;; (Note: ":" will be selected with the same probability as the other numbers in "tr").
(defun rndrngs (: fr tr len dir step inf)
  (let* ((fr (if (numberp fr) fr..fr (or fr 0..10)))
	 (tr (if (numberp tr) tr..tr (or tr 0..10)))
	 (fr1 (min (from fr) (to fr)))
	 (fr2 (max (from fr) (to fr)))
	 (tr1 (min (from tr) (to tr)))
	 (tr2 (max (from tr) (to tr)))
	 (minlen (+ 1 (max 0 (- tr1 fr2) (- fr1 tr2))))
	 (maxlen (+ 1 (- (max fr2 tr2) (min fr1 tr1))))
	 (inf (and inf (not len)))
	 (pred (or (typecase len
		     (null 'true)
		     (integer (when (in-range minlen..maxlen len)
				(opip length abs (+ 1) (= len))))
		     (range (when (and (in-range minlen..maxlen (from len))
				       (in-range minlen..maxlen (to len)))
			      (opip length abs (+ 1) (in-range len)))))
		   (error "Invalid len arg: ~a" len)))
	 (joinfn (if (memq dir '(nil 0)) 'rcons
		   (if (> dir 0) (iff 'less 'rcons (flipargs 'rcons))
		     (iff 'less (flipargs 'rcons) 'rcons))))
	 (fromnums (if inf
		       (rndelems (rlist fr :))
		     (rndints fr)))
	 (tonums (if inf
		     (rndelems (rlist tr :))
		   (rndints tr)))
	 (rnglst (keep-if* pred (mapcar* joinfn fromnums tonums))))
    (if step ;; Make sure step is consistent with dir, len & inf 
	(mapcar* (ret (rcons (from @1)
			     (if (eq (abs @2) 1)
				 (to @1)
			       (rcons (to @1)
				      (if inf @2
					(let ((d (> (to @1) (from @1)))
					      (s (max (abs @2) 1)))
					  (if d s (- s))))))))
		 rnglst (rndints step))
      rnglst)))

;; Create a lazy list of random elements taken uniformely from list "incl",
;; after removing any elements that are also in list "excl".
;; If any element of "incl" is an iterator, then values will be retrieved
;; from the iterator instead of returning a copy of the iterator itself.
;; Elements in list "excl" will be excluded from the results. 
;; Note: excluded values are removed from the iterator stream before its value is added to results.
;; so the probability associated with the iterator is unaffected.
(defun rndelems (incl : excl)
  (let ((incl (diff incl excl)) val1 val2)
    (gen (ret t) (progn (while* (memqual val1 excl) (set val1 (nth (rand (len incl)) incl)))
			(typecase val1
			  (seq-iter (while* (memqual val2 excl)
					    (set val2 (seq-next val1 nil)))
				    val2)
			  (t val1))))))

;; Create lazy list of random chars.
;; Only include chars that are in "in", but not in "ex".
;; The "in" & "ex" args may be single chars/integers, ranges or chars/integers,
;; strings, or predicate functions that work on chars (e.g. chr-isalnum), or
;; of any of those things.
(defun rndchars (: (incl #\0..#\z) excl)
  (let* ((allchars (range #\nul 255))
	 (fn (opip [iffi 'atom 'list]
		   (mappend (do typecase @1
			      (chr (list @1))
			      (integer (list @1))
			      (range (rlist @1))
			      (fun (keep-if @1 allchars))
			      (sym (keep-if @1 allchars))
			      (str (mapcar 'int-chr @1))))
		   (mapcar 'chr-int)))
	 (chars (diff [fn incl] [fn excl])))
    (gun (nth (rand (len chars)) chars))))

;; Create lazy list of random strings.
;; This can be called in 2 different ways: if arg1 is a string or regexp
;; then random strings matching that regexp will be created. In this case
;; arg2 defines the maximum number of repetitions allowed for the
;; regexp repetition operators + & * (default value is 10), and arg3 defines
;; the range of characters allowed for wildcard & char class operators
;; (default is #\0..#\z). In this case the regexp supplied in arg1 cannot contain
;; any of the following operators: ~, %, & However you can use arg4 with any regexp
;; to perform further filtering of the results (see below).
;; If arg1 is a number or range of numbers, then random strings of that length (or range
;; of lengths) will be created, with arg2 & arg3 playing the same role as the "incl" & "excl"
;; args in rndchars (with the same default values).
;; In both cases if the optional 4th arg "filterx" is supplied then created strings will
;; be tested against this regexp and only those that match will be returned. This final
;; option should be used with care since it can slow things down alot.
(defun rndstrings (: arg1 arg2 arg3 filterx)
  (let ((rx (if (stringp arg1)
		(regex-compile arg1)
	      arg1))
	(maxn (or arg2 10))
	(chars (or arg3 #\0..#\z))
	(len (typecase arg1
	       (number arg1)
	       (range (car (rndints arg1)))
	       (null (car (rndints 1..10)))))
	(incl (or arg2 #\0..#\z))
	(excl (or arg3 #\"))
	(filterx (if (stringp filterx)
		     (regex-compile filterx)
		   filterx))
	lazylst)
    (labels ((errorfn (sym) (error "rndstrings doesn't support ~a operator" sym))
	     (treesub (arg) (typecase arg
			      (chr arg) (str arg)
			      (sym (rndchar (caseq arg
						   (:space 'chr-isspace)
						   (:word-char '(chr-isalpha #\_))
						   (:digit 'chr-isdigit)
						   (t chars))
					    (caseq arg
						   (:cspace 'chr-isspace)
						   (:cword-char '(chr-isalpha #\_))
						   (:cdigit 'chr-isdigit)
						   (t nil))))
			      (list (let* ((head (car arg)) (tail (cdr arg))
					   (dotail (mapcar (fun treesub) tail))
					   (rndnum (rand (+ 1 maxn))))
				      (caseq head
					     (compound (cat-str dotail)) 
					     (or (rndelem dotail))
					     (? (when (rndbool) (car dotail)))
					     (0+ (cat-str (when (> rndnum 0)
							    (mappend (ret (mapcar (fun treesub) tail))
								     (range 1 rndnum)))))
					     (1+ (cat-str (mappend (ret (mapcar (fun treesub) tail))
								   (range 0 rndnum))))
					     (set (when tail (rndelem tail)))
					     (cset (rndchar chars tail))
					     (ng0+ (errorfn '%))
					     (~ (errorfn '~))
					     (and (errorfn '&))
					     (nil nil)
					     (t (cons head dotail))))))))
      (set lazylst (if len (gun (make-like (take len (rndchars incl excl)) "a"))
		     (gun [treesub (regex-source rx)])))
      (if filterx (keep-if* (f^$ filterx) lazylst) lazylst))))

;; rndregexs generates random regular expressions of maximum depth "maxd" (as measured by the parse tree),
;; and random lengths in the range given by "lengths" (or all the same length if "lengths" is a single integer).
;; The "allowed" arg can be used to limit the chars and symbols used in the regexps (leave as nil to use defaults),
;; and the "okchr" arg specifies which chars are allowed to appear after a backslash #\\ (defaults to all chars that
;; are valid in this position).
;; To further specify the form of the regexps produced you can supply a regexp as the final argument in string form,
;; containing one or more occurrences of "\\X". These "\\X"s will be replaced with random regexps whose lengths,
;; maximum depths, and allowed chars & symbols are specified in successive elements of the corresponding arguments
;; (which will be padded to lists of the correct length if necessary).
;; e.g. (rndregexs '(5 1..10) (3 3) nil "foo\\Xbar\\Xchoo") will produce regexps that start with "foo" followed by a
;; random regexp of length 5 and max depth 3, followed by "bar", followed by a regexp of length between 1 & 10 and
;; max depth 3, followed by "choo". 
(defun rndregexs (: (lengths 1..20) (maxd 5) allowed regex okchr)
  (let* ((allowed (or allowed (rlist #\0..#\z :space :word-char :digit
				     :cspace :cword-char :cdigit 'wild
				     'compound 'or 'and '~ '? '0+ '1+ 'ng0+ 'set 'cset)))
	 (chars1 (isec allowed (rlist #\0..#\z :space :word-char :digit)))
	 (allchars (append chars1 (isec allowed '(:cspace :cword-char :cdigit wild))))
	 (ops (isec allowed '(compound or and ~ ? 0+ 1+ ng0+ set cset)))
	 (okchr (or okchr '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\? #\D #\S #\W #\[ #\\ #\]
			    #\a #\b #\d #\e #\f #\n #\r #\s #\t #\v #\w))))
    (labels ((treebuild ;; This is messy, but it works
	      (d l parent pos syms bs)
	      (let ((maxl (rndint lengths)))
		(if (and (< d maxd) (< l maxl))
		    (let* ((left (- maxl l))
			   (newops (if (memq parent '(set cset))
				       nil
				     (isec ops
					   (caseq left
						  (1 nil)
						  (2 (caseq parent
							    ((or and ~) '(compound ~ ? 0+ 1+))
							    (ng0+ (if (= pos 1)
								      '(compound ~ ? 0+ 1+)))
							    ((? 0+ 1+) nil)
							    (t '(compound ? 0+ 1+))))
						  (3 (caseq parent
							    (and '(compound ~ and ? 0+ 1+ set))
							    (~ '(compound ~ ? 0+ 1+ set))
							    (or '(compound ~ and or ? 0+ 1+ set))
							    (ng0+ (if (= pos 1)
								      '(compound or and ~ ? 0+ 1+ set)
								    '(compound set)))
							    ((? 0+ 1+) '(compound set))
							    (t '(compound ? 0+ 1+ set))))
						  (4 (caseq parent
							    (ng0+ (if (= pos 1) ops
								    '(compound ~ ? 0+ 1+ set cset)))
							    (t (diff ops '(or and ng0+)))))
						  (t ops)))))
			   (syms (append (if bs okchr allchars) newops))
			   (this (if parent (rndelem syms) 'compound))
			   (xtra (caseq this
					((ng0+ cset) 3)
					(set 2)
					((and or ~ 0+ 1+ ? compound)
					 (- (caseq parent ;; parent cannot be set/cset here
						   ((? 0+ 1+) 3)
						   (ng0+ (if (= pos 0) 3 1))
						   ((or and ~)
						    (if (or (and (eq this 'or)
								 (memq parent '(and ~)))
							    (and (eq this 'and)
								 (eq parent '~)))
							3 1))
						   (compound (if (memq this '(and or ~)) 3 1))
						   (t 1))
					    (if (eq this 'compound) 1 0)))
					(t 0)))
			   (n (if parent (rndint 1..(max 1 (- left xtra))) left)))
		      (if (member this allchars)
			  (cons (+ 1 l) this)
			(let* ((elems
				(caseq this
				       ((set cset) (cons (+ l n xtra) (take n (rndelems chars1))))
				       (t (reduce-left 
					   (ret (let* ((tail (cdr @1))
						       (bs (eq (car tail) #\\))
						       (elem (treebuild (+ d 1) (car @1)
									this (len tail)
									syms bs)))
						  (cons (car elem)
							(if bs (cons (car tail) 
								     (cons (cdr elem) (cdr tail)))
							  (cons (cdr elem) tail)))))
					   (caseq this
						  (compound (rlist 1..n))
						  ((and or ng0+) '(1 2))
						  ((? 0+ 1+ ~) '(1)))
					   (cons (+ l xtra) nil))))))
			  (cons (car elems) ^(,this ,*(if (eq this 'compound)
							  (remq nil (cdr elems))
							(mapcar
							 (ret (or @1 (rndelem allchars)))
							 (cdr elems))))))))
		  (cons l nil))))
	     (errorfn (sym) (error "Invalid ~a arg: ~a" sym (symbol-value sym))))
      (if regex
	  ;; count number of \\X's in regex then uses mapcar to create regexps to place in \\X's
	  (let* ((rxparts (spl "\\X" regex))
		 (nholes (- (len rxparts) 1))
		 (lengths (argtolist lengths nholes all number range))
		 (maxd (argtolist maxd nholes all number))
		 (allowed (argtolistlist allowed nholes allall chr sym))
		 (rxseqs (mapcar (opip rndregexs seq-begin) lengths maxd allowed)))
	    (gun (regex-compile (cat-str (weave rxparts 
						(mapcar (opip (seq-next @1 nil)
							      tostring (sub-str @1 2 -1))
							rxseqs))))))
	(gun (regex-compile (cdr [treebuild 0 0 nil 0 allowed nil])))))))

;; rndlists returns a lazy-list of lists of random elements from "seq" - a list, range,
;; string (treated as a list of chars) or iterator. If seq is a lazy list or iterator then 
;; elements will be retrieved sequentially instead of randomly (it is assumed that in this
;; case seq is a lazy-list of random values). This means you can use rndlists recursively
;; to produce random lists of random lists.
;; If n is an integer then all returned lists will have the same length n. If n is a list
;; or range then the lengths of the returned lists will be chosen at random from n, and if
;; it is an iterator then the list lengths will be chosen sequentially from the iterator.
(defun rndlists (seq : (n 1..10) uniq)
  (let ((iter (typecase seq
		(seq-iter seq)
		(lcons (seq-begin seq))
		(str (rndchars seq))
		(sequence (rndelems seq))
		(range (rndints seq))
		(t (error "Invalid sequence arg: ~a" seq))))
	(n (typecase n
	     (integer n)
	     (range (rndints n))
	     (list (rndelems n))
	     (seq-iter n))))
    (lcons-iter (iter-tuples n iter uniq))))

(defun rndvecs (seq : (n 1..10) uniq) (mapcar* 'vec-list (rndlists seq n uniq)))

;; Just for fun:
(defun collatz (n) (giterate (op /= 1) (iff 'evenp (opip (/ @1 2) int-flo) (opip (* 3) (+ 1))) n))

;; Create functions that return iterators corresponding to previous random lazy lists,
;; and functions that return single random elements.
;; Iterator function names are same as previous rand.. functions but with a * at the end,
;; and singular function names have the final "s" removed.
;; Both take the same arguments. E.g. (rndints* 5..20) returns an iterator that yields
;; random integers between 5 & 20 (using the seq-next function), whereas (rndint 5..20)
;; returns a single value between 5 & 20.
(mapcar (lambda (sym)
	  (let* ((symname (cat-str (list "rnd" (symbol-name sym))))
		 (fnorig (intern symname))
		 (fnnew1 (intern (cat-str (list symname "*"))))
		 (fnnew2 (intern (sub-str symname 0 -1))))
	    (eval ^(defun ,fnnew1 args (seq-begin (apply ',fnorig args))))
	    (eval ^(defun ,fnnew2 args (car (take 1 (apply ',fnorig args)))))))
	'(ints floats chars rngs strings elems lists vecs norms bools regexs))

