;;
;; Eliza-19690731-DOCFNSp1-03of06.lisp
;;

* /docfns/   31 July 1969  1007:42

(test
  (lambda (d s)
    (prog (cd psv)
	  (setq psv (cdr parselist))
      lp  (cond
	    ((null d)
	      (cond
	        (s (go rn))
		(t (setq parselist (car parselist))
		  (return t))))
	    ((eq 0 (setq cd (car d)))
	      (go t0))
	    ((null s)
	      (go rn))
	    ((numberp cd)
	      (tconc s parselist)
	      (cond
	        ((setq s (nth s cd))
		  (go t3))
	        (t (go rn))))
            ((atom cd)
	      (cond
	        ((eq cd (car s)))
		(t (go rn))))
	    ((car cd)
	      (cond
	        ((member (car s)
	            cd))
		(t (go rn))))
	    ((test4 (car s)
		    (cdr cd)))
	    (t (go rn)))
    	  (tconc s parselist)
      t3  (setq s (cdr s))
          (setq d (cdr d))
	  (go lp)
      t0  (tconc s parselist)
          (cond
	    ((null (setq d (cdr d)))
	      (setq parselist (car parselist))
	      (return t)))
      t1  (cond
	    ((test d s)
	      (return t))
	    ((setq s (cdr s))
	      (go t1)))
      rn  (rplacd parselist (cond
	      (psv (rplacd psv nil))))
          (return nil)
      )))
