;; take & drop functions that work with iterators & ranges (as well as sequences)
;; The "end" arg is used to indicate the end of the iterator sequence. It should
;; be a value that is not emitted by the iterator, or otherwise indicates the end.
;; Also : lcons-iter  - convert an iterator into an lcons 
;;        seq-iterp   - test if object is an iterator
;;        iter-tuples - create iterator that returns tuples from sequence/iterator/range

(defpackage seqxtra (:fallback usr))
(in-package seqxtra)

(defun take* (n seq : (end 'end))
  (when (and n seq)
    (typecase seq
      (seq-iter (take-while
		 (op neq end)
		 (mapcar (ret (seq-next seq end))
			 (range* 0 n))))
      (range (take n (rlist seq)))
      (t (take n seq)))))

(defun take-while* (predfn seq : (keyfn 'identity) end)
  (typecase seq
    (seq-iter (doloop (lst (val (seq-next seq end)))
		      ([(opip keyfn predfn) val] (reverse lst))
		      (push val lst)))
    (range (take-while predfn (rlist seq) keyfn))
    (t (take-while predfn seq keyfn))))

(defun take-until* (predfn seq : (keyfn 'identity) end)
  (typecase seq
    (seq-iter (doloop (lst (val (seq-next seq end)))
		      ((not [(opip keyfn predfn) val])
		       (push val lst) (reverse lst))
		      (push val lst)))
    (range (take-until predfn (rlist seq) keyfn))
    (t (take-until predfn seq keyfn))))

;; take-uniq* returns the first n unique values from seq (an iterator/range/list/string/vector)
(defun take-uniq* (n seq : (end 'end))
  (let* ((seq (typecase seq
		(seq-iter seq)
		(range (seq-begin (rlist seq)))
		(t (seq-begin seq))))
	 (val (seq-next seq end))
	 accum)
    (while (and (< (len accum) n)
		(neq val end))
      (push val accum)
      (if (< (len accum) n)
	  (while* (memqual val accum)
		  (set val (seq-next seq end)))))
    (reverse accum)))

(defun drop* (n seq : end)
  (typecase seq
    (seq-iter (dotimes (i n) (seq-next seq end)) seq)
    (range (drop n (rlist seq)))
    (t (drop n seq))))
;; For iterators drop-while* will also drop the first element that predfn evaluates as true,
;; whereas drop-while keeps it for sequences (but doesn't work with iterators).
;; Unfortunately this is unavoidable.
(defun drop-while* (predfn seq : keyfn end)
  (typecase seq
    (seq-iter (while [(opip (if keyfn [keyfn @1] @1) predfn)
		     (seq-next seq end)])
	      seq)
    (range (drop-while predfn (rlist seq) keyfn))
    (t (drop-while predfn seq keyfn))))

(defun drop-until* (predfn seq : keyfn end)
  (typecase seq
    (seq-iter (while (not [(opip (if keyfn [keyfn @1] @1) predfn)
			  (seq-next seq end)]))
	      seq)
    (range (drop-until predfn (rlist seq) keyfn))
    (t (drop-until predfn seq keyfn))))

(defun seq-iterp (arg) (eq (typeof arg) 'seq-iter))

;; lcons-iter creates a lazy list from an iterator (to go the other way use iter-begin)
(defun lcons-iter (iter) (gen (ret t) (seq-next iter nil)))

;; iter-tuples creates an iterator that returns tuples of elements from the input sequence/iterator/range.
;; If n is an integer then the iterator returns tuples of that length.
;; If n is an iterator, sequence or range, then the iterator will return tuples whose lengths are
;; determined by the values retrieved from n (in the same order). If n is a finite sequence or range its
;; values will be recycled after the last value is reached.
;; If the "uniq" arg is non-nil then only unique values will appear in each tuple (duplicate values will
;; be dropped).
;; The "fill" argument is used for padding tuples when "seq" has run out of values.
;; The "end" argument is emitted by the returned iterator to indicate the end of the sequence or list
;; within the sequence.
;; The "end" arg should have the same value as in the "take*" function to ensure "take*" doesn't
;; pad results with empty tuples.
(defun iter-tuples (n seq : uniq (fill nil fillp) (end 'end))
  (let ((iter (typecase seq
		(sequence (seq-begin seq))
		(lcons (seq-begin seq))
		(seq-iter seq)
		(range (seq-begin (rlist seq)))
		(str (seq-begin seq))
		(t (error "Invalid sequence arg: ~a" seq))))
	(n (typecase n
	     (integer (seq-begin (pad nil n)))
	     (lcons (seq-begin (repeat n)))
	     (sequence (seq-begin (repeat n)))
	     (seq-iter n)
	     (range (seq-begin (repeat (rlist n))))
	     (t (error "Invalid n arg: ~a" n))))
	(takefn (if uniq 'take-uniq* 'take*)))
    (seq-begin (gen (ret t) (let* ((m (seq-next n nil))
				   (head (if (eq m 0) nil
					   (or [takefn m iter end] end))))
			      (if (and fillp (listp head))
				  (pad head fill (- m (len head)))
				head))))))

;; cum-iter creates a list/lcons containing the cumulative partial sums
;; of its argument list/lcons.
(defun cum-seq (seq)
  (let ((seq (if (seq-iterp seq)
		 seq
	       (seq-begin seq))))
    (expand-right (ret (let* ((val (seq-next (cdr @1) nil)))
			 (when val
			   (let ((sum (+ (car @1) val)))
			     (cons sum (cons sum seq))))))
		  (cons 0 seq))))

;; diff-seq creates a list/lcons containing the first differences of
;; its argument list/lcons. The first value will be the same as the
;; first value of the input seq
(defun diff-seq (seq)
  (let ((seq (if (seq-iterp seq)
		 seq
	       (seq-begin seq))))
    (expand-right (ret (let* ((val (seq-next (cdr @1) nil)))
			 (when val (cons (- val (car @1)) (cons val seq)))))
		  (cons 0 seq))))
