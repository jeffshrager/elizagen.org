; Transcribed from The BBN-LISP System, Daniel G. Bobrow et al, February, 1966,
; AFCRL-66-180

(prog nil
   (cond
      ((null (fntyp (quote putdq))) (putd (print (quote putdq))
(quote (nlamda (x) (prog2
         (putd (car x) (cadr x))
	 (car x)))))))
(return (putdq load (lambda (x) (prog (xx yy zz)
   (clearbuf)
   (setq zz (typein nil))
l1 (cond
      ((equal (setq xx (read)) (quote stop)) (return (prog2
	 (clearbuf)
	 (typein zz)))))
   (setq xx (eval xx))
   (cond
      (x (print xx)))
   (go l1))))))

(putdq define
   (lambda (x) (cond
      ((null x) nil)
      (t (cons ((lambda (y) (prog2
	    (putd (car y) (cond
	       ((null (cdddr y)) (cadr y))
	       (t (cons (quote lambda) (cdr y)))))
	    (car y)))
	 (car x)) (define (cdr x)))))))

(putdq defineq
   (nlamda (x) (define x)))

(defineq

(add
   (lambda (x y z) (prog nil
      loop  (cond
	       ((null (cdr x)) (rplacd x (list
		  y
		  (list
		     z))))
	       ((equal (cadr x) y) (rplaca (cddr x) (append
(caddr x) (list
                  z))))
	       ((setq x (cddr x)) (go loop)))
	    (return y))))

(add1
   (lambda (x) (plus
      x
      1)))

(append
   (lambda (x y) (cond
      ((null x) y)
      (t (cons (car x) (append (cdr x) y))))))

(assoc
   (lambda (xsas ysas) (cond
      ((null ysas) nil)
      ((equal (caar ysas) xsas) (car ysas))
      (t (assoc xsas (cdr ysas))))))

(attach
   (lambda (x y) (rplaca (rplacd y (cons (car y) (cdr y)))
x)))

(copy
   (lambda (x) (cond
      ((null x) nil)
      ((atom x) x)
      (t (cons (copy (car x)) (copy (cdr x)))))))

(deflist
   (lambda (l ind) (prog nil
      loop  (cond
	       ((null l) (return nil)))
	    (put (caar l) ind (cadar l))
	    (setq l (cdr l))
	    (go loop))))

(difference
   (lambda (x y) (plus
      x
      (minus y))))

(e
   (nlamda (xeeee) (eval xeeee)))

(ersetq
   (nlamda (ersetx) (errorset (car ersetx) t)))

(get
   (lambda (x y) (cond
      ((null x) nil)
      ((equal (car x) y) (cadr x))
      (t (get (cdr x) y)))))

(getp
   (lambda (x y) (prog (z)
	    (setq z (cdr x))
      loop  (cond
	       ((null z) (return nil))
	       ((eq (car z) y) (return (cadr z))))
	    (setq z (cddr z))
	    (go loop))))

(intersection
   (lambda (x y) (cond
      ((null x) nil)
      ((member (car x) y) (cons (car x) (intersection
(cdr x) y)))
      (t (intersection (cdr x) y)))))

(last
   (lambda (x) (prog (xx)
      l     (cond
	       ((atom x) (return xx)))
	    (setq xx x)
	    (setq x (cdr x))
	    (go l))))

(lconc
   (lambda (x p) (prog (xx)
	    (return (cond
	       ((null x) p)
	       ((cdr (setq xx (last x))) (error (list
		  (quote lconc)
		  x)))
	       ((null p) (cons x xx))
	       ((null (car p)) (rplaca (rplacd p xx) x))
	       (t (prog2
		  (rplacd (cdr p) x)
		  (rplacd p xx))))))))

(length
   (lambda (x) (prog (n)
	    (setq n 0)
      l     (cond
	       ((atom x) (return n)))
	    (setq x (cdr x))
	    (setq n (add1 n))
	    (go l))))

(lessp
   (lambda (x y) (cond
      ((equal x y) nil)
      ((greaterp x y) nil)
      (t t))))

(map
   (lambda (mapx mapf) (cond
      ((null mapx) nil)
      (t (prog2
	 (mapf mapx)
	 (map (cdr mapx) mapf))))))

(mapc
   (lambda (mapcx mapcf) (cond
      ((null mapcx) nil)
      (t (prog2
	 (mapcf (car mapcx))
	 (mapc (cdr mapcx) mapcf))))))

(mapcar
   (lambda (mpcrx mpcrf) (cond
      ((null mpcrx) nil)
      (t (cons (mpcrf (car mpcrx)) (mapcar (cdr mpcrx) mpcrf
))))))

(mapcon
   (lambda (mpcnx mpcnf) (cond
      ((null mpcnx) nil)
      (t (nconc (mpcnf mpcnx) (mapcon (cdr mpcnx) mpcnf
))))))

(mapconc
   (lambda (mpcncx mpcncf) (cond
      ((null mpcncx) nil)
      (t (nconc (mpcncf (car mpcncx)) (mapconc (cdr mpcncx) mpcncf
))))))

(maplist
   (lambda (mplstx mplstf) (cond
      ((null mplstx) nil)
      (t (cons (mplstf mplstx) (maplist (cdr mplstx) mplstf
))))))

(minusp
   (lambda (x) (greaterp 0 x)))

(nill
   (nlamda (xnil) nil))

(nlsetq
   (nlamda (nlsetx) (errorset (car nlsetx) nil)))

(not
   (lambda (x) (cond
      ((null x) t)
      (t nil))))

(prop
   (lambda (x y u) (cond
      ((null x) (u))
      ((equal (car x) y) (cdr x))
      (t (prop (cdr x) y u)))))

(punch
   (lambda (x) (prog (y z)
	    (setq y (punchon t))
	    (setq z (typeout nil))
	    (print x)
	    (punchon y)
	    (typeout z)
	    (return x))))

(put
   (lambda (x y z) (prog nil
      loop  (cond
	       ((null (cdr x)) (rplacd x (list
		  y
		  z)))
	       ((equal (cadr x) y) (rplaca (cddr x) z))
	       ((setq x (cddr x)) (go loop)))
	    (return y))))

(rdflx
   (lambda (x) (prog (xx yy)
	    (setq yy (typein t))
	    (cond
	       (x (go r1)))
	    (setq xx (ersetq (read)))
	    (go r2)
      r1    (cond
	       ((setq xx (nlsetq (read))) (setq xx (car xx
)))
	       ((print x) (go r1)))
      r2    (typein yy)
	    (return xx))))

(remainder
   (lambda (x y) (cdr (divide x y))))

(remove
   (lambda (a x) (cond
      ((null x) nil)
      ((equal a (car x)) (remove a (cdr x)))
      (t (cons (car x) (remove a (cdr x)))))))

(remprop
   (lambda (x y) (prog nil
      loop  (cond
	       ((null (cdr x)) (return y))
	       ((equal (cadr x) y) (rplacd x (adddr x)))
	       (t (setq x (cdr x))))
	    (go loop))))

(reverse
   (lambda (x) (prog (u)
      loop  (cond
	       ((null x) (return u)))
	    (setq u (cons (car x) u))
	    (setq x (cdr x))
	    (go loop))))

(sassoc
   (lambda (xsas ysas usas) (cond
      ((null ysas) (usas))
      ((equal (caar ysas) xsas) (car ysas))
      (t (sassoc xsas (cdr ysas) usas)))))

(setnq
   (nlamda (xsetnq) (setn (car xsetnq) (eval (cadr xsetnq)
))))

(setqq
   (nlamda (x) (set (car x) (cadr x))))

(soundexin
   (nlamda (x) (mapcar x (quote (lambda (ysdx) (put (soundex
ysdx) (quote name) ysdx))))))

(soundexout
   (lambda (x) (getp x (quote name))))

(sub1
   (lambda (x) (plus
      x
      -1)))

(sub2
   (lambda (a z) (cond
      ((null a) z)
      ((equal (caar a) z) (cdar a))
      (t (sub2 (dr a) z)))))

(sublis
   (lambda (a y) (cond
      ((atom y) (sub2 a y))
      (t (cons (sublis a (car y)) (sublis a (cdr y)))))))

(subst
   (lambda (x y z) (cond
      ((equal y z) x)
      ((atom z) z)
      (t (cons (subst x y (car z)) (subst x y (cdr z)))))))

(tconc
   (lambda (x p) (prog (xx)
	    (return (cond
	       ((null p) (cons (setq xx (cons x nil)) xx))
	       ((null (car p)) (prog2
		  (rplaca p (cons x nil))
		  (rplacd p (car p))))
	       (t (rplacd p (cdr (rplacd (cdr p)
(rplacd (cons x (cdr p)) nil))))))))))

(time
   (lambda (x n) (prog (y m c c1)
	    (setq m n)
	    (setq c (clock))
      t1    (cond
	       ((zerop m) (setq c1 (clock)))
	       (t (progn
		  (setq y (eval x))
		  (setq m (sub1 m))
		  (go t1))))
	    (setq m (divide (plus
	       c1
	       (minus c)) n))
	    (prin1 (car m))
	    (prin1 period)
	    (prin1 (quotient (times
	       (cdr m)
	       10) n))
	    (prin1 blank)
	    (print (quote seconds))
	    (return y))))

(union
   (lambda (x y) (cond
      ((null x) y)
      ((member (car x) y) (union (cdr x) y))
      (t (cons (car x) (union (cdr x) y))))))

(zerop
   (lambda (x) (equal x 0)))

(break
   (lambda (fn when what) (prog (xx yy zz)
	    (cond
	       ((null (setq xx (getd fn))) (return (prog2
		  (putd fn (list
		     (quote nlamda)
		     (quote (l))
		     (list
			(quote break1)
			nil
			when
			(setq xx (list
			   fn
			   (quote (undefined))))
			what)))
		  xx)))
	       ((eq (setq yy (fntyp f)) (quote fsubr)) (return
 (cons fn (quote (is an fsubr)))))
	       ((null (eq yy (quyote subr))) (go b2)))
	    (setq yy (rdflx (print (cons fn (quote (is a subr
 need args))))))
	    (putd (setq zz (gensym)) xx)
	    (setq xx (putd fn (list
	       (quote lambda)
	       yy
	       (cons zz yy))))
     b2     (cond
	       ((eq (caaddr xx) (quote break1)) (setq xx (
list
	          (car xx)
		  (cadr xx)
		  (cadr (caddr xx))))))
	    (putd fn (list
	       (car xx)
	       (cadr xx)
	       (list
		  (quote break1)
		  (caddr xx)
		  when
		  (list
		     fn)
		  what)))
	    (return fn))))

(unbreak
   (lambda (fn) (prog (xx yy)
	    (return (cond
	       ((null (setq xx (getd fn))) (cons fn (quote
 (not a function))))
	       ((and
		  (or
		     (eq (setq yy (fntyp fn)) (quote expr)
)
		     (eq yy (quyote fexpr)))
		  (eq (caaddr xx) (quote break1))) (prog2
		  (putd fn (list
		     (car xx)
		     (cadr xx)
		     (cadr (caddr xx))))
		  fn))
	       (t (cons fn (quote (not broken)))))))))

(breaklist
   (nlamda (x) (maplist x (quote (lambda (x) (break (car x
) t nil))))))

(unbreaklist
   (nlamda (x) (maplist x (quote (lambda (x) (unbreak (car
 x)))))))

(breakprog
   (lambda (bpx bpy) (maplist bpy (quote (lambda (z) (breakat
 bpx (car z) t nil))))))

(unbreakprog
   (lambda (x) (prog (xx)
	    (setq xx (bp1 x))
      u1    (cond
	       ((eq (caadr xx) (quote break1)) (rplacd xx
(cddr xx)))
	       ((setq xx (cdr xx)) (go u1))
	       (t (return nil)))
	    (go u1))))

(breakat
   (lambda (fn where when what) (prog (a)
	    (setq a (bp1 fn))
      b1    (cond
	       ((equal (car a) where) (return (prog2
		  (rplacd a (cons (list
		     (quote break1)
		     nil
		     when
		     (list
			fn
			(quote at)
			where)
		     what) (cdr a)))
		  where)))
	       ((setq a (cdr a)) (go b1)))
	    (return (cons where (quote (not found)))))))

(unbreakat
   (lambda (fn where) (prog (a)
	    (setq a (bp1 fn))
      u1    (cond
	       ((equal (car a) where) (return (cond
		  ((eq (caadr a) (quote break1)) (prog2
		     (rplacd a (cddr a))
		     where))
		  (t (cons fn (append (quote (not broken at
)) (list
		     where)))))))
	       ((setq a (cdr a)) (go u1)))
	    (return (cons where (quote (not found)))))))

(break1
   (nlamda (brk1x) (prog (brk1xx brk1yy brk1zz)
	    (cond
	       ((null (setq brk1xx (eval (cadr brk1x)))) (
return (eval (car brk1x))))
	       ((null (equal brk1xx (quote (nil)))) (go b0
)))
	    (print (append (quote (crack in)) (caddr brk1x
)))
	    (cond
	       ((cadddr brk1x) (print (eval (cadddr brk1x)
))))
	    (go b3)
      b0    (setq brk1yy (print (append (quote (break in))
 (caddr brk1x))))
	    (cond
	       ((cadddr brk1x) (print (eval (cadddr brk1x)
))))
      b1    (cond
	       ((eq (setq brk1xx (rdflx brk1yy)) (quote quit
)) (error (caddr brk1x)))
	       ((eq brk1xx (quote stop)) (go b3))
	       ((eq brk1xx (quote return)) (go b2))
	       ((eq brk1xx (quote eval)) nil)
	       ((eq brk1xx (quote ok)) (go b3))
	       ((and
		  (ersetq (setq brk1xx (eval brk1xx)))
		  (nlsetq (print brk1xx))) (go b1))
	       ((print brk1yy) (go b1)))
	    (cond
	       ((null (setq brk1zz (ersetq (eval (car brk1x
))))) (print brk1yy))
	       ((print (append (caddr brk1x) (quote (evaluated
)))) (set (caaddr brk1x) (car brk1zz))))
	    (go b1)
      b2    (cond
	       ((and
		  (setq brk1zz (rdflx nil))
		  (setq brk1zz (ersetq (eval (car brk1zz))
))) (go b4))
	       ((print brk1yy) (go b1)))
      b3    (cond
	       ((or
		  brk1zz
		  (setq brk1zz (ersetq (eval (car brk1x)))
)) nil)
	       ((print brk1yy) (go b1)))
      b4    (cond
	       ((eq brk1xx (quote ok)) (print (caddr brk1x
)))
	       ((prog2
		  (print (append (quote (vaue of)) (caddr
 brk1x)))
		  (null (nlsetq (print (car brk1zz))))) (print
 (quote ok))))
            (return (car brk1zz)))))

(bp1
   (lambda (x) (prog (xx)
	    (return (cond
	       ((and
		  (or
		     (eq (setq xx (fntyp x)) (quote expr))
		     (eq xx (quote fexpr)))
		  (eq (caaddr (setq xx (getd x))) (quote prog
))) (cadddr xx))
	       (t (error (cons x (quote (not a program))))
))))))

(prettydef
   (lambda (x) (prog (a)
	    (setq a (punchon t))
	    (prin1 (quote "("))
	    (print (quote defineq))
	    (prettyprint x)
	    (print (quote ")"))
	    (punchon a)
	    (return x))))

(prettyprint
   (lambda (l) (map l (quote (lambda (j) (prog (t1)
	       (terpri)
	       (prin1 lpar)
	       (print (car j))
	       (printdef (cond
		  ((getd (car j)))
		  (t (quote undefined))))
	       (prin1 rpar)
	       (terpri)))))))

(printdef
   (lambda (e) (prog (i iunit iunit1)
	    (setnq i 1)
	    (setq iunit (quote "   "))
	    (setq iunit1 3)
	    (prin1 iunit)
	    (superprint e)
	    (return nil))))

(superprint
   (lambda (e) (cond
      ((atom e) (cond
	 ((member e (quote (""" " " "(" ")"
"
" "	" "." ","))) (prin1 (pack (list
            (quote """)
	    e
	    (quote """)))))
	 (t (prin1 e))))
      (t (prog (ep m)
	       (setq ep e)
	       (prin1 lpar)
	  a    (cond
		  ((member (car ep) (quote (and
		     or
		     select
		     selectq
		     list
		     plus
		     times
		     cond
		     prog2
		     progn))) (go pl))
		  ((eq (car ep) (quote prog)) (go pp))
		  ((atom (car ep)) nil)
		  ((or
		     (eq (caar ep) (quote lambda))
		     (eq (caar ep) (quote nlamda))) (go pl
)))
	       (superprint (car ep))
	       (setq ep (cdr ep))
	       (cond
		  ((null ep) (return (prin1 rpar)))
		  ((atom ep) (go pd)))
	       (prin1 blank)
	       (go a)
	  pk   (setnq i (sub1 i))
	  pd   (prin1 blank)
	       (prin1 period)
	       (prin1 blank)
	       (prin1 ep)
	       (return (prin1 rpar))
	  pl   (setnq i (add1 i))
	       (superprint (car ep))
	  pm   (setq ep (cdr ep))
	       (cond
		  ((null ep) (go pj))
		  ((atom ep) (go pk)))
	       (endline)
	       (superprint (car ep))
	       (go pm)
	  pj   (setnq i (sub1 i))
	       (return (prin1 rpar))
	  pp   (prin1 (car ep))
	       (setq ep (cdr ep))
	       (setnq i (add1 i))
	       (cond
		  ((null ep) (go pj))
		  ((atom ep) (go pk)))
	       (prin1 blank)
	       (superprint (car ep))
	  py   (setq ep (cdr ep))
	       (cond
		  ((null ep) (go pj))
		  ((atom ep) (go pk)))
	       (endline)
	       (cond
		  ((atom (car ep)) (go pz)))
	       (prin1 iunit)
	       (prin1 iunit)
	  px   (setnq i (plus
		  i
		  2))
	       (superprint (car ep))
	       (setnq i (plus
		  i
		  -2))
	       (go py)
	  pz   (prin1 (car ep))
	       (setnq m (plus
		  iunit1
		  iunit1
		  (minus (length (unpack (car ep))))))
	  aa   (setnq m (sub1 m))
	       (prin1 blank)
	       (cond
		  ((null (or
		     (zerop m)
		     (minusp m))) (go aa)))
	       (setq ep (cdr ep))
	       (cond
		  ((null ep) (go pj))
		  ((atom ep) (gp pk))
		  ((atom (car ep)) (go pz)))
	       (go px))))))

(endline
   (lambda nil (prog (j)
	    (setnq j i)
	    (terpri)
      a     (cond
	       ((zerop j) (return nil))
	       ((minusp j) (error i)))
	    (prin1 iunit)
	    (setnq j (sub1 j))
	    (go a))))

(trace
   (lambda (x) (prog (a b c g)
	    (setq a x)
      loop  (cond
	       ((null x) (return a)))
	    (setq b (getd (setq c (car x))))
	    (setq x (cdr x))
	    (cond
	       ((null b) (progn
		  (print (cons c (quote (undefined))))
		  (go loop)))
	       ((tracp c b) (progn
		  (print (cons c (quote (was traced))))
		  (go loop))))
	    (putd (setq g (gensym)) b)
	    (putd c (list
	       (quote nlamda)
	       (quote (q1qq))
	       (list
		  (quote trac1)
		  (list
		     (quote quote)
		     c)
		  (list
		     (quote quote)
		     g)
		  (quote q1qq))))
	    (go loop))))

(untrace
   (lambda (x) (prog (a b c g)
	    (set (quote a) x)
      loop  (cond
	       ((null x) (return a)))
	    (set (quote g) (car x))
	    (set (quote x) (cdr x))
	    (cond
	       ((tracp g (set (quote b) (getd g))) (progn
		  (set (quote b) (cdaddr b))
		  (putd (cadar b) (getd (set (quote c) (cadadr
b))))
		  (remob c)))
	       (t (print (cons g (quote (not traced))))))
	    (go loop))))

(tracp
   (lambda (x y) (and
      (eq (fntyp x) (quote fexpr))
      (eq (caaddr y) (quote trac1)))))

(trac1
   (lambda (ctrac gtrac xtrac) (prog (atrac)
	    (print (cons ctrac (quote (entered with))))
	    (set (quote xtrac) (cond
	       ((eq (fntyp gtrac) (quote fsubr)) (print xtrac
))
	       ((eq (fntyp gtrac) (quote fexpr)) (print xtrac
))
	       (t (evalprint xtrac))))
	    (set (quote atrac) (eval (cons gtrac xtrac)))
	    (print (cons ctrac (quote (has value))))
	    (return (print atrac)))))

(evalprint
   (lambda (xvalp) (prog (avalp)
      loop  (cond
	       ((null xvalp) (return avalp)))
	    (set (quote avalp) (nnconc avalp (list
	       (list
		  (quote quote)
		  (print (eval (car xvalp)))))))
	    (set (quote xvalp) (cdr xvalp))
	    (go loop))))

(editf
   (lambda (x) (prog2
      (putd x (edite (getd x)))
      x)))

(editv
   (lambda (x) (prog2
      (set x (edite (eval x)))
      x)))

(editp
   (lambda (x) (prog2
      (rplacd x (edite (cdr x)))
      x)))

(edite
   (lambda (x) (prog (l y c)
	    (typein t)
	    (setq l (list
	       x))
	    (print (quote edit))
      a     (cond
	       ((null (ersetq (setq c (read)))) (go a))
	       ((null c) (return (car (lastr l))))
	       ((numberp c) (edit1f c))
	       ((eq c (quote copy)) (setq y (copy l)))
	       ((eq c (quote restore)) (setq l (cond
		  (y y)
		  (t l))))
	       ((eq c (quote p)) (edit3f (quote (p 0))))
	       ((atom c) (print qmark))
	       ((numberp (car c)) (edit2f c))
	       (t (edit3f c)))
	    (go a))))

(edit1f
   (lambda (c) (cond
      ((eq c 0) (cond
	 ((null (cdr l)) (print qmark))
	 (t (setq l (cdr l)))))
      ((greaterp c 0) (cond
	 ((greaterp c (length (car l))) (print qmark))
	 (t (setq l (cons (car (nth (car l) c)) l)))))
      (t (print qmark)))))

(edit2f
   (lambda (c) (cond
      ((greaterp (car c) 0) (cond
	 ((greaterp (car c) (length (car l))) (print qmark
))
	 (t (rplaca l (edit2af (sub1 (car c)) (car l) (cdr
 c) nil)))))
      ((or
	 (eq (car c) 0)
	 (null (cdr c))
	 (greaterp (minus (car c)) (length (car l)))) (print
 qmark))
      (t (rplaca l (edit2af (sub1 (minus (car c))) (car l)
 (cdr c) t))))))

(edit2af
   (lambda (n x r d) (prog2
      (cond
	 ((null (eq n 0)) (rplacd (nth x n) (nconc r (cond
	    (d (cdr (nth x n)))
	    (t (cddr (nth x n)))))))
	 (d (attach (car r) x))
	 (r (rplaca x (car r)))
	 ((rplaca x (cadr x)) (rplacd x (cddr x))))
      x)))

(edit3f
   (lambda (x) (cond
      ((eq (car x) (quote i)) (edit2f (list
	 (cadr x)
	 (eval (caddr x)))))
      ((eq (car x) (quote e)) (ersetq (print (eval (cadr x
)))))
      ((eq (car x) (quote n)) (nconc (car l) (cdr x)))
      ((eq (car x) (quote p)) (bpnt (cdr x)))
      ((member (car x) (quote (ri ro li lo))) (errorset (nconc
 x (quote ((car l)))) t))
      (t (print qmark)))))

(bpnt
   (lambda (x) (prog (y n)
	    (cond
	       ((zerop (car x)) (setq y (car l)))
	       ((greaterp (car x) (length (car l))) (go b1
))
	       ((minusp (car x)) (go b1))
	       (t (setq y (car (nth (car l) (car x))))))
	    (cond
	       ((null (cdr x)) (setq n 2))
	       ((null (numberp (cadr x))) (go b1))
	       ((minusp (cadr x)) (go b1))
	       (t (setq n (cadr x))))
	    (return (cond
	       ((nlsetq (print (leveln y n))) nil)
	       (t (print (quote edit)))))
      b1    (return (print qmark)))))

(leveln
   (lambda (x n) (cond
      ((atom x) x)
      ((zerop n) (quote ^))
      (t (mapcar x (quote (lambda (x) (leveln x (sub1 n)))
))))))

(nth
   (lambda (x n) (cond
      ((atom x) nil)
      ((greaterp n 1) (nth (cdr x) (sub1 n)))
      (t x))))

(lastr
   (lambda (x) (cond
      ((null x) (error (quote (null list))))
      ((null (cdr x)) x)
      (t (lastr (cdr x))))))

(ri
   (lambda (m n x) (prog (a b)
	    (setq a (nth x m))
	    (setq b (nth (car a) n))
	    (cond
	       ((or
		  (null a)
		  (null b)) (return (print qmark))))
	    (rplacd a (nconc (cdr b) (cdr a)))
	    (rplacd b nil))))

(ro
   (lambda (n x) (prog (a)
	    (setq a (nth x n))
	    (cond
	       ((or
		  (null a)
		  (atom (car a))) (return (print qmark))))
	    (rplacd (lastr (car a)) (cdr a))
	    (rplacd a nil))))

(li
   (lambda (n x) (prog (a)
	    (setq a (nth x n))
	    (cond
	       ((null a) (return (print qmark))))
	    (rplaca a (cons (car a) (cdr a)))
	    (rplacd a nil))))

(lo
   (lambda (n x) (prog (a)
	    (setq a (nth x n))
	    (cond
	       ((or
		  (null a)
		  (atom (car a))) (return (print qmark))))
	    (rplacd a (cdar a))
	    (rplaca a (caar a)))))

)
