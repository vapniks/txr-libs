;;;; Useful lisp functions for data extraction using TXR

(load "ranges")

(defpackage csv (:fallback ranges usr))

(in-package csv)

(defex csvdata-error type-error)

(defstruct (csvdata ncols : headers data sep fmtstr quote trim fmtstr keyfn) nil
	   (ncols 0) headers data (sep ",")
	   quote ;; whether to quote fields or not when printing
	   trim ;; whether to trim fields or not when printing
	   fmtstr ;; for printing the csvdata (if nil then default format string used)
	   keyfn ;; keyfun arg of the unique function, or nil if rows don't need to be unique
	   (:static pprint t) ;; whether to pretty print by default in the REPL
	   ;; (negates pretty-p arg of self.print, but doesn't affect printdata or writefile)
	   (:static nprintlines 20) ;; number of lines to print
	   (:static maxhashsize 500) ;; maximum number of hash entries allowed (to avoid using too much memory)
	   ;; TODO
	   (:postinit (self) ;; on initialization check for bad rows and uniquify if necessary
		      ;; TODO
		      ;; (let ((badrows self.(invalidRows self.data t)))
		      ;; 	(if badrows
		      ;; 	    (throwf 'csvdata-error "Invalid csvdata rows: ~a" badrows)
		      ;; 	  (when self.keyfn self.(uniquify))))
		      1
		      )
	   (:method uniquify (self) ;; remove duplicate rows according to keyfn
		    (set self.data (unique self.data self.keyfn)))
	   (:method invalidRows (self data : checktypes) ;; return indices of invalid rows
		    (labels ((pred1 (row) (= (length row) self.ncols))
			     (pred2 (row) (all row (ret (typecase @1 (string t)
								  (number t)
								  (t nil))))))
		      (where (if checktypes
				 (notf (andf (fun pred1) (fun pred2)))
			       (notf (fun pred1)))
			     data)))
	   (:method append (self data) ;; append new rows to data
		    (let* ((newrows (if self.(compatp data)
				      data.data
				      (if (proper-listp (car data))
					  data
					(list data))))
			   (badrows self.(invalidRows newrows t)))
		      (if badrows
			  (throwf 'csvdata-error "Attempting to append invalid rows: ~a" badrows)
			(progn
			  (set self.data (append self.data newrows))
			  (when self.keyfn self.(uniquify))))))
	   (:method compatp (self obj) ;; check if another csvdata object is compatible with this one
		    (and (structp obj)
			 (eq (struct-type obj) (struct-type self))
			 (= obj.ncols self.ncols)))
	   ;; memoize the colwidths. Reset each time csvdata is altered
	   ;; TODO: fix this to work better with ranges
	   (:instance %memo% (make-hash nil nil t))
	   (:method colwidth (self n : (rowids 0..(- self.(length) 1)))
		    (when (or (test-dirty self)
			      (> (hash-count self.%memo%) self.maxhashsize))
		      (clearhash self.%memo%)
		      (clear-dirty self))
		    (let ((rowids (if (listp rowids)
				      (sort rowids)
				    rowids)))
		      (when (< n self.ncols)
			(or (gethash self.%memo% (list n rowids))
			    (sethash self.%memo% (list n rowids)
				     (apply 'max (mapcar
						  (opip car tostringp length)
						  (cons (list (nth n self.headers))
							[self rowids n]))))))))
	   ;; args for printdata method:
	   ;; rowids   = number, list or range of rows to print (nil = all)
	   ;; colids   = number, list or range of cols to print (nil = all)
	   ;; pretty-p = if non-nil then ignore self.fmtstr, make column widths fit fields exactly,
	   ;;            and add a column to hold row numbers
	   ;; stream   = output stream
	   ;; Note: indexing with ranges is python-style - the end value is not included.	   
	   (:method printdata (self : rowids colids (inchead t) pretty-p (stream *stdout*))
		    (let* ((rx (regex-compile (cat-str (list self.sep "|\""))))
			   (fieldquotedp (ret (= #\" (chr-str @1 0)
						 (chr-str @1 (- (length @1) 1)))))
			   (badfieldp (andf 'stringp rx (notf fieldquotedp)))
			   (quotefield (op cat-str (list "\"" @1 "\"")))
			   (fixbadfield (iffi badfieldp quotefield))
			   (procfield (cond ((and self.trim self.quote)
					     (chain 'tostringp 'trim-str quotefield))
					    (self.quote (chain 'tostringp quotefield))
					    (self.trim (chain 'tostringp 'trim-str fixbadfield))
					    (t fixbadfield)))
			   (rowids (or rowids 0..(- self.(length) 1)))
			   rowids2
			   (colids (slice-list (rlist 0..(- self.ncols 1))
					       (or colids 0..(- self.ncols 1))))
			   (headers (if self.headers (slice-list self.headers colids)))
			   (rows [self rowids colids])
			   (fmtstr (cat-str
				    (nconc
				     (if self.fmtstr
					 (list self.fmtstr)
				       (interpose self.sep (repeat '("~a") (length colids))))
				     '("\n"))))
			   (printline (ret (opip (mapcar procfield @1)
						 (apply 'format @@1 fmtstr)))))
		      (if pretty-p
			  (set rowids2 (typecase rowids
					 (range (rlist rowids))
					 (list rowids)
					 (number (list rowids))
					 (t (range* 0 self.(length))))
			       fmtstr (cat-str (list "~"
						     (tostring
						      (max 3
							   (length
							    (tostring
							     (apply 'max rowids2)))))
						     "a: "
						     (cat-str
						      (interpose
						       self.sep
						       (mapcar (op cat-str
								   (list "~"
									 (tostring
									  (+ 2 self.(colwidth @1 rowids2)))
									 "a"))
							       colids)))
						     "\n"))
			       printline (ret (opip (cons @2 (mapcar procfield @1))
						    (apply 'format @@1 fmtstr @1)))))
		      (when (and inchead headers)
			[[printline stream] headers "row"])
		      (if pretty-p
			  (mapdo [printline stream] rows rowids2)
			(mapdo [printline stream] rows))))
	   ;; default function used for printing in the REPL
	   (:method print (self stream pretty-p)
		    (let ((nrows self.(length)))
		      (if self.pprint
			  self.(printdata 0..(- self.nprintlines 1)
					  nil t (not pretty-p) stream)
			  self.(printdata 0..self.nprintlines nil t pretty-p stream))
		      (when (> nrows self.nprintlines)
			(format stream "...plus ~a more rows (~a columns & ~a rows in total)\n"
				(- nrows self.nprintlines) self.ncols nrows))))
	   (:method writefile (self file : append rowids colids)
		    (with-stream (fstr (open-file file (if append "a" "w")))
				 self.(printdata rowids colids (if append nil t) nil fstr)))
	   (:method nrows (self) (length self.data))
	   (:method length (self) (length self.data))
	   (:method filesize (self)
		    (apply '+ (* self.(length) self.ncols)
			   (mapcar (op apply '+ (mapcar 'length @1)) self.data)))
	   (:method car (self) (car self.data))
	   (:method cdr (self) (cdr self.data))
	   (:method rplaca (self newcar) (set (car self.data) newcar))
	   (:method rplacd (self newcdr) (set (cdr self.data) newcdr))
	   (:method nullify (self) self.data)
	   (:function from-list (lst) (new (csvdata (length (car lst)) nil lst)))
	   ;; accessor/setter methods. Allow using csvdata object as a place function (see section 9.20.3 of manpage).
	   ;; TODO: indexing method for picking out disjoint rectangles of data?
	   ;; TODO : allow indexing colids using regexps
	   (:method lambda (self rowids : colids) 
		    (set colids (typecase colids
				  (null 0..(- self.ncols 1))
				  (range colids)
				  (number colids)
				  (string (posqual colids self.headers))
				  (sym (posqual (symbol-name colids) self.headers))
				  (list (mapcar (do typecase @1
						  (range @1)
						  (number @1)
						  (string (posqual @1 self.headers))
						  (sym (posqual (symbol-name @1) self.headers))
						  (t @1))
						(flatten colids)))
				  (t colids))) ;; invalid types will be caught by slice-list
		    (mapcar (op slice-list @1 colids)
			    (slice-list self.data rowids))) ;; rowids handled by slice-list
	   (:method lambda-set (self rowids colids newdata) ;; TODO - allow negative indices, and regexps for colids?
		    (let* ((rowids (typecase rowids
				     (null (range* 0 self.(length)))
				     (range (rlist rowids))
				     (number (list rowids))
				     (list (mappend (do typecase @1
						      (range (rlist @1))
						      (number (list @1))
						      (t (throwf 'csvdata-error
								 "Invalid rowids arg ~a" rowids)))
						    (flatten rowids)))
				     (t (throwf 'csvdata-error "Invalid rowids arg ~a" rowids))))
			   (colids (typecase colids
				     (null (range* 0 self.ncols))
				     (range (rlist colids))
				     (number (list colids))
				     (string (posqual colids self.headers))
				     (sym (posqual (symbol-name colids) self.headers))
				     (list (mappend (do typecase @1
						      (range (rlist @1))
						      (number (list @1))
						      (string (list (posqual @1 self.headers)))
						      (sym (list (posqual (symbol-name @1) self.headers)))
						      (t (throwf 'csvdata-error
								 "Invalid colids arg ~a" colids)))
						    (flatten colids)))
				     (t (throwf 'csvdata-error "Invalid colids arg ~a" colids))))
			   (rlen (length rowids))
			   (clen (length colids)))
		      (flet ((pad2 (d l) 
				   (if (atom d)
				       (pad nil d l)
				     (pad d (car (last d))
					  (- l (length d))))))
			(set newdata (pad2 newdata rlen)) ;; pad new data with extra rows to fit rowids
			(dotimes (i (length rowids))
			  (let* ((oldrow (nth (nth i rowids) self.data))
				 (newrow (pad2 (nth i newdata) clen)));; pad each row to fit colids
			    (dotimes (j (length colids))
			      (set (nth (nth j colids) oldrow)
				   (nth j newrow)))))
			;; make sure dirty bit is set 
			(set self.ncols self.ncols))))
	   (:method delete (self rowids colids)) ;TODO!!!
	   ;; TEST
	   (:method sort (self : lessfn keyfn)
		    (set self.data (sort self.data lessfn keyfn)))
	   ;; (:method write ()) ;; write .csv data to file? 
	   
	   ;; (:method searchRows (self regex : cols)) ;; return rows which match regexp in columns cols (if missing, all columns), use find-if function?
	   ;; (:function from-list (lst) (let ((ncols (length (car lst))))
	   ;; 				 (if (all lst (chain 'length (op = ncols)))
	   ;; 				     (csvdata ncols nil lst))))
	   ;; (:method join (self data)) ;; function to horizontally join another set of columns onto data??
	   )


;; Read .csv formatted data according to RFC-4180
;; ARGS:
;; stream   = stream from which to read csv data.
;; KEYWORD ARGS:
;; header   = whether or not the first line of the stream contains column headers,
;;            or a list of column headers.
;; sep      = separator between fields (default = ",")
;; emptystr = string to use for empty fields (default = nil, i.e. the empty string "")
;; trim     = if non-nil trim whitespace from unquoted fields (default = nil). Note: will not remove quoted whitespace.
;; unquote  = if non-nil remove quotes from double quoted fields (default = nil).
;; fixchars = if non-nil check field for invalid control chars and remove them (default = nil).
(defun read-csv-stream (:key stream -- header sep emptystr trim unquote fixchars)
  (let* ((sep (if (stringp sep) sep ","))
	 (line0 (if (and header (listp header))
		    (cat-str (interpose sep header))
		  (get-line stream)))
	 (rownum (if header 0 1))
	 (fieldrx (regex-compile
		   "([^,\"\']*(\"([^\"]|\\\\\")*\"|\'([^\']|\\\\\')*\'|)[^,\"\']*)+"))
	 (selpts (op select @2 (range* @1 (length @2) 2))) ;; extracts odd/even indexed entries of list
	 (splitpts (chain (op sub (tok fieldrx t @1) 1 -1) 
			  (juxt (op selpts 0) (op selpts 1))))
	 (headerpts [splitpts line0])
	 (ncols (length (car headerpts)))
	 (invalidp (do or (/= (length (car @1)) ncols)
		     (some (cadr @1) (op nequal sep))))
	 ;; field filters
	 (rmbadchars (op regsub #/[\000-\007\013-\037\177-\377]/ " "))
	 (fillempty (do if (equal @1 "") emptystr @1))
	 (rmquotes (do if (and (> (length @1) 1)
			       (= #\" (chr-str @1 0)
				  (chr-str @1 (- (length @1) 1))))
		     (sub-str @1 1 -1) @1))
	 (fnlist (mapcar 'and
			 (list emptystr trim unquote fixchars)
			 (list fillempty 'trim-str rmquotes rmbadchars)))
	 (fieldfn (if (some fnlist) (apply 'chain (remove-if 'null fnlist)))))
    (if [invalidp headerpts]
      (throw 'csvdata-error
	     (cat-str (list (if header "Header" "First") " row is invalid")))
      (let* (data
	     (rowdata (unless header (car headerpts)))
	     (addrow (if fieldfn
			 (op cons (mapcar fieldfn rowdata) data)
		       (op cons rowdata data))))
	(if rowdata (set data [addrow]))
	(whilet ((rownum (+ 1 rownum))
		 (line (get-line stream)))
		(set rowdata (spl "," line))
		(when (/= (length rowdata) ncols)
		  (if (< (length rowdata) ncols)
		      (throwf 'csvdata-error "Not enough fields in row ~a" rownum)
		    (progn
		      (set rowdata [splitpts line])
		      (if [invalidp rowdata]
			(throwf 'csvdata-error "Row ~a contains invalid data" rownum)
			(set rowdata (car rowdata))))))
		(set data [addrow]))
	(new (csvdata ncols (if header (car headerpts)) (nreverse data) sep))))))

(defun read-csv-file (:key file -- (header t) sep emptystr trim unquote fixchars)
  (with-stream (fstr (open-file file "r"))
	       (read-csv-stream fstr :header header :sep sep :emptystr emptystr
				:trim trim :unquote unquote :fixchars fixchars)))
