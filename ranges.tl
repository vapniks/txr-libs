;;;; Useful lisp functions for data extraction using TXR

(defpackage ranges (:fallback usr))
(in-package ranges)

;; TODO!! Check how these work with 3-element ranges, e.g. 1..10..2 or infinite ranges, e.g 1..:..2 (all odd numbers),
;;        and char ranges, e.g. #\a..#\z
;; TODO: define syntactic places for start, end & stepsize of ranges

;; Some extra range functions (put them in a separate package)

;; Extract the start of a range object (: at start is treated as 0)
(defun rfrom (r) (let ((fr (from r))) (if (eq fr :) 0 fr)))
;; Extract the end of a range object
(defun rto (r) (if (rangep (to r)) (from (to r)) (to r)))
;; Extract the stepsize of a range object
(defun rstep (r) (let ((tr (to r)))
		   (when (rangep tr)
		     (let ((s (to tr)))
		       (if (eq s :)
			   (if (less (rfrom r) (from tr))
			       1 -1)
			 s)))))
;; Extract the stepsize of a range object, even for simple ranges with stepsize 0/1/-1, e.g. 10..10/0..10/10..0
(defun rstep* (r) (or (rstep r)
		      (let ((fr (rfrom r)))
			(signum (- (ifa (eq (to r) :) (+ fr 1) it)
				   fr)))))
;; Alter the start, end or stepsize of a range object, and return the result.
;; To set the end to : (i.e. infinity) use any non-nil symbol.
;; If the stepsize arg is explicitly set to nil then the returned range will have no stepsize (even if the original did).
(defun rset (r : fr tr (s nil sp))
  (set fr (or fr (from r))
       tr (if tr
	      (if (symbolp tr) : tr)
	    (rto r)))
  (if sp (rcons fr (if s (rcons tr s) tr))
    (rcons fr (let ((st (rstep r)))
		(if st (rcons tr st) tr)))))

;; Test if a range object is reversed.
(defun rrevp (r) (when r (let ((l (take 2 (rlist r)))) ;; this method works for all range types (e.g. 0..:..-1)
			   (> (car l) (or (cadr l) (car l))))))
;; Reverse a range object (stepsize will be negated).
;; NOTE: ranges that have : as an endpoint will have their endpoints swapped around as with other ranges.
;;       This means that, unlike ranges without : endpoints, the "reverse" of such a range corresponds to
;;       a list which is not necessarily the reverse of the list corresponding to the original range.
;;       (since a : at the start of a range object indicates 0 not infinity)
(defun rrev (r) (when r (if (rangep (to r))
			    (rcons (from (to r))
				   (rcons (from r) (- (to (to r)))))
			  (rcons (to r) (from r)))))
;; Idempotent version of rrev: reverse a range object only if it's unreversed, or vice-versa if unrev is non-nil.
(defun rrev* (r : unrev) (if (rrevp r)
			     (if unrev (rrev r) r)
			   (if unrev r (rrev r))))
;; Return length of range object, or : if it is infinite.
(defun rlen (r) (symacrolet ((tr (to r)) (ftr (from (to r)))
			     (fr (from r)) (ttr (to (to r))))
			    (cond ((null r) r)
				  ((eq tr :) :)
				  ((eq fr :) (rlen (rcons 0 tr)))
				  ((rangep tr)
				   (if (or (eq ftr :) (= (signum (- fr ftr)) (signum ttr)))
				       :
				     (int-flo (ceil (/ (+ (abs (- ftr fr)) 1) (abs ttr))))))
				  (t (+ (abs (len r)) 1)))))

;; rsplit splits its range argument at n into two disjoint ranges returned in a cons cell,
;; or if n is a list, into more than two ranges.
;; By default the endpoint of a returned range to the left of a split point will be set to
;; that split point (i.e. treating ranges as not including their endpoints).
;; If inc is non-nil then the endpoint of a returned range will be one less/more than the
;; split point (depending on the direction of the range).
;; TODO: Make sure this also works with char ranges.
(defun rsplit (r : (n 0) inc)
  (labels ((fn (lst r ns) (let ((fr (from r))
				(tr (to r))
				(n (car ns)))
			    (if (not (and n (/= fr tr)
					  (or (= (signum (- n fr))
						 (signum (- tr n)))
					      (and inc (= n tr)))))
				(list (cons r lst))
			      (if inc
				  (fn (cons (rcons fr [(if (> fr tr) '+ '-) n 1]) lst)
				      (rcons n tr)
				      (cdr ns))
				(fn (cons (rcons fr n) lst)
				    (rcons n tr)
				    (cdr ns)))))))
    (if (and (rangep r))
	(let ((n (if (numberp n)
		     (list n)
		   (uniq (sort n)))))
	  (reverse (car (fn nil r n)))))))

;; Return a list of all ends points of ranges in args, ignoring nil values
;; (this is used by rmin,rmax, & rspan)
(defun rends args
  (let ((args2 (remq nil args)))
    (remq nil (nconc (mapcar (opip from (if (eq @1 :) 0 @1)) args2)
		     (mapcar (chain 'to (iffi 'rangep 'from)) args2)))))

;; Return the smallest endpoint of the ranges supplied as args (nil values in args will be ignored).
(defun rmin args (apply 'min (apply 'rends args)))
;; Return the largest endpoint of the ranges supplied as args (nil values in args will be ignored).
(defun rmax args (apply 'max (apply 'rends args)))
;; Return a single range spanning all its args (which should be ranges. nil values will be ignored).
(defun rspan args
  (let ((ends (apply 'rends args)))
    (rcons (apply 'min ends) (apply 'max ends))))

;; rolap returns 'left / 'right if r1 is to the left/right of r2 without overlap,
;; 'olapl / 'olapr if r1 overlaps r2 to the left/right, 'within / 'contains if r1 is within / contains r2,
;; and 'equal if they are the same. Note: stepsizes are ignored, only the endpoints are taken into consideration.
;; TODO: write another function rolap* which takes stepsizes into account? (tricky when stepsizes differ).
(defun rolap (r1 r2)
  (let ((f1 (rfrom r1)) (t1 (rto r1))
	(f2 (rfrom r2)) (t2 (rto r2)))
    (cond ((less t1 f2) 'left)
	  ((greater f1 t2) 'right)
	  ((less f1 f2) (if (less t1 t2) 'olapl 'contains))
	  ((greater t1 t2) (if (greater f1 f2) 'olapr 'contains))
	  ((and (eq f1 f2) (eq t1 t2)) 'equal)
	  (t 'within))))

;; rsort sorts a list of ranges, by smallest then largest endpoints, then stepsize in reverse order
;; (the rationale is that ranges with smaller stepsizes are likely to get closer to the endpoint).
;; Ranges are first transformed so that all are increasing (i.e. "from" value <= "to" value)
(defun rsort args (macrolet ((comp (fn pos) ^(mapf ',fn ',pos ',pos)))
		    (sort (mapcar (lop 'rrev* t) args)
			  (iff (ret (null @1)) 'nilf
			       (iff (ret (null @2)) 'tf
				    (iff (comp eq rfrom)
					 (iff (comp eq rto)
					      (comp greater rstep)
					      (comp less rto))
					 (comp less rfrom)))))))

;; runi returns a list of ranges which cover the union of the ranges in args, and no more. Stepsizes are ignored.
;; nil args and ranges with the same to & from values are treated as empty, and do not alter results.
;; TODO: create runi* function that takes account of stepsizes?
(defun runi args
  (sort (reduce-left
	 (do let ((r1 (car @1)) (r2 @2))
	   (cond ((null r1) (list r2))
		 ((lequal (to r2) (to r1)) @1)
		 ((lequal (from r2) (ifa (eq (to r1) :) it (succ it)))
		  (cons (rcons (from r1) (to r2)) (cdr @1)))
		 (t (cons r2 @1))))
	 (mapcar (opip last car) ;; for each start point keep longest range
		 (partition-by 'from (sort (mapcar (chain (op rset @1 (rfrom @1) nil nil) (lop rrev* t))
						   (remq nil args)))))
	 nil)))

;; risec returns a single range representing the intersection of its argument ranges,
;; or nil if such intersection is empty. If args contains any nil values then nil is returned.
(defun risec args
  (and (not (memq nil args))
       (reduce-left
	(ret (cond ((eq @1 0) @2)
		   ((greater (from @2) (to @1)) (return-from risec))
		   ((lequal (to @2) (to @1)) @2)
		   (t (rcons (from @2) (to @1)))))
	(mapcar 'car ;; sort ranges and remove redundant ones
		(partition-by 
		 'from (sort (mapcar (chain (op rset @1 (rfrom @1) nil nil)
					    (lop rrev* t))
				     (remq nil args)))))
	0)))

;; The first argument to rdiff may be a single range or a list of ranges.
;; The function will return a cons cell whose car contains a list of ranges representing the set
;; difference of the union of rAs minus the union of rBs, and whose cdr contains the set
;; difference of the union of rBs minus the union of rAs.
;; The returned ranges will be sorted in increasing order.
(defun rdiff (rAs . rBs)
  (let* ((rAs (apply 'runi (if (listp rAs) rAs (list rAs))))
	 (finalres))
    (set finalres
	 (reduce-left ;; outer loop 
	  (lambda (tuple rA) 
	    (placelet ((Adiff (first tuple))  ;rAs\rBs
		       (Bdiff (second tuple)) ;rBs\rAs
		       (Btodo (third tuple))) ;unprocessed rBs
		      (let (Aremain Bremain)
			(set Aremain
			     (reduce-left ;; inner loop
			      (lambda (ra rB) ;; ra is remaining part of rA to be processed
				(if ra
				    (symacrolet
				     ((frB (from rB)) (trB (to rB))
				      (fra (from ra)) (tra (to ra))
				      (pushBdiff (push (rcons frB (- fra 1)) Bdiff))
				      (pushAdiff (push (rcons fra (- frB 1)) Adiff))
				      (pushBremain (push (rcons (+ tra 1) trB) Bremain)))
				     (caseq (rolap rB ra) ;; position of rB relative to ra
					    (left (push rB Bdiff) ra)
					    (olapl pushBdiff (rcons (+ trB 1) tra))
					    (within (when (/= fra frB) pushAdiff)
						    (when (/= tra trB) (rcons (+ trB 1) tra)))
					    (equal nil)
					    (contains (when (/= fra frB) pushBdiff)
						      (when (/= tra trB) pushBremain)
						      nil)
					    (olapr pushAdiff pushBremain nil)
					    (right (push ra Adiff) (push rB Bremain) nil)))
				  (progn (push rB Bremain) nil)))
			      Btodo ;; inner loop iterates over unprocessed ranges in rBs,
			      rA)) ;; and accumulates unprocessed parts of this rA (Aremain)
			(when Aremain (push Aremain Adiff))
			(list Adiff Bdiff (reverse Bremain)))))
	  rAs ;; outer loop iterates over the ranges in rAs,
	  (list nil nil (apply 'runi rBs)))) ;; and accumulates differences and unprocessed rBs (see above)
    (cons (reverse (first finalres))
	  (nconc (reverse (second finalres)) (third finalres)))))

;; Return a list of range objects covering exactly the same elements as the input list (including repetitions)
;; in the same order, so that (chain 'listr (ap rlist)) is equivalent to the identity function on lists of integers.
;; The input list may also contain ranges which will be treated appropriately.
;; Note: (chain (ap rlist) 'listr) is not necessarily the identity on lists of ranges because the numbers could
;; group differently, e.g. '(#R(1 5) #R(6 10)) -> '(#R(1 10))
;; TODO: make this work with char ranges
(defun listr (lst)
  (let* ((sigfn (op apply (if (rrevp @1) '- '+) @rest))
	 (r1p (op apply '= (from @1) (to @1) @rest))
	 (res (reduce-left (lambda (a b)
			     (let ((h (car a))
				   (accum (op cons @1 (cdr a))))
			       (typecase h
				 (number ;; allowing numbers on accumulator makes code faster (but longer)
				  (if (typecase b
					(number (= 1 (abs (- b h))))
					(range (or (= (from b) [sigfn b h 1])
						   [r1p b (- h 1)] 
						   ;;[r1p b (+ h 1)] not needed (covered by first part of or clause)
						   )))
				      [accum h..(if (rangep b) (to b) b)]
				      (cons b [accum h..h])))
				 (range
				  (if (typecase b
					(number (or (= b [sigfn h (to h) 1])
						    [r1p h (+ b 1)]))
					(range (or (= [sigfn b 1] [sigfn h 1]
						      (- (from b) (to h)))
						   [r1p h (+ (from b) 1)]
						   [r1p b (- (to h) 1)])))
				      [accum (from h)..(if (rangep b) (to b) b)]
				      (cons b a)))
				 (t (cons b a)))))
			   (remq nil lst)
			   nil)))
    (when (numberp (car res))
      (set (car res) (car res)..(car res)))
    (reverse res)))

;; listr* works like listr except that the endpoints of the returned ranges are 
;; fixed so that when used for indexing with dwim brackets (e.g. [lst 0..3])
;; they produce the same results as indexing by the corresponding list elements,
;; i.e. the endpoints are incremented/decremented (depending on the direction of the range).
;; In addition any reverse ranges (e.g. 3..0) are split into single element ranges
;; (e.g. 3..4 2..3 1..2) so they can also be used for indexing (normally reverse
;; ranges cannot), and ranges which straddle 0 are split in two.
;; If the input list contains ranges, these will be treated as though they do not contain
;; their endpoints, so (listr* (list 0..10)) returns (#R(0 10)) not (#R(0 11)).
;; If nosplit is non-nil then reverse ranges, and ranges containing
;; both positive & negative values will not be split.
;; TODO: make this work with char ranges
(defun listr* (lst : nosplit) 
  (let* ((lst2 (mapcar (do typecase @1
			 (number @1)
			 (range (when (/= (to @1) (from @1))
				  (rcons (from @1)
					 [(if (rrevp @1) '+ '-) (to @1) 1]))))
		       lst)))
    (mappend (opip (rcons (from @1) ;; TODO - can this be made simpler with collect-each*?
			  [(if (rrevp @1) '- '+) (to @1) 1])
		   (if (or nosplit (not (in-range* @1 0)))
		       (list @1) (rsplit @1)) 
		   (mappend (do if (and (rrevp @1) (not nosplit)) 
			      (mapcar 'rcons (rlist* @1) (rlist* (+ @1 1)))
			      (list @1))
			    @1))
	     (listr lst2))))

;; slice a list by indices in a range, single number, a list of ranges & numbers, or a predicate function.
;; Negative indices count backwards from the end of the list, e.g. (slice-list (range 0 10) -2) = 9,
;; but indices whose absolute value is greater than or equal to the list length are ignored.
;; NOTE: unlike range indexing with dwim brackets, endpoints are included, and unlike the select function
;;       the indices do not need to be in order, and the return results will be in the same order as the
;;       input indices.
;; Arguments:
;;  lst - the list to be sliced
;;  idxs - the slice indices, either a single number, a range object (can have : endpoints and stepsize),
;;         a vector of integers, a predicate function which returns non-nil for elements of lst to be returned,
;;         or a list containing any combination of the previous types.
;; Return value: a subsequence of the input list (lst)
(defun slice-list (lst idxs)
  (typecase idxs
    (number (let ((x [lst idxs])) (when x (list x))))
    (range (let* ((isrev (rrevp idxs))
		  (ids (if (and isrev (eq (rto idxs) :))
			   ;;backward infinite range needs special treatment
			   (let ((l (length lst))
				 (s (rstep* idxs))
				 (f (rfrom idxs)))
			     (+ (* (ceil (/ (+ f l) s)) s -1) f)..f..(- s))
			 (rrev* idxs t)))
		  (fr (rfrom ids))
		  (to (ifa (eq (rto ids) :) (length lst) it)) ;; what about backward infinite ranges, e.g. 1..:..-1 ?
		  (step (rstep* ids)))
	     [(if isrev 'reverse 'identity)
	     (if (= (signum fr) (signum to))
		 (if (> step 1)
		     [lst (rlist ids)]
		     [lst fr (+ 1 to)])
	       (if (> step 1)
		   (let ((ids2 (rlist fr..to..step)))
		     (format t "step = ~a, lst = ~a, ids2 = ~a\n" step lst ids2) ;DEBUG
		     (let ((idx1 (keep-if (op > 0) ids2))
			   (idx2 (keep-if (op <= 0) ids2)))
		       (format t "lst = ~a, idx1 = ~a, idx2 = ~a, res1 = ~a, res2 = ~a\n"
			       lst idx1 idx2 (select lst idx1) (select lst idx2)) ;DEBUG
		       (append (select lst idx1) (select lst idx2))
		       ))
		 (append [lst fr 0]
			 [lst 0 (+ 1 to)])))])) ;dont use nconc here
    (list (mappend (op slice-list lst) ;; indexing with ranges is usually faster than indexing with numbers
		   (listr (flatten (mapcar (iffi 'vectorp 'list-vec 'identity)
					   idxs)))))
    (vec [lst idxs])
    (sym (if (symbol-function idxs)
	     (slice-list lst (symbol-function idxs))
	   (throwf 'type-error "Invalid type for idxs arg ~a" idxs)))
    (fun (keep-if idxs lst))
    (t (throwf 'type-error "Invalid type for idxs arg ~a" idxs))))

