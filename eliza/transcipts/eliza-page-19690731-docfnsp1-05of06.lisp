;; TRANSCRIBED BY PAUL NATHAN - pnathan@vandals.uidaho.edu
* /DOCFNS/ 31 JULY 1969  1007:42

          ((EQ V1 V2)
           (GO T3))
          (PF (COND
              (TPF (SPACES 1))
              (T (TERPRI)
                (SETQ TPF T)))
              (PRIN1 (CAR V1)))
          (T (TCONC (CAR V1)
              SENT)))
       (SETQ V1 (CDR V1))
       (GO T2)
  )))

(MEMORY
 (LAMBDA NIL
   (PROG (PARSELIST X)
      LP (COND
           ((NULL MEMSTACK)
            (RETURN)))
         (SETQ PARSELIST (CONS NIL NIL))
         (COND
           ((TEST (CAAR MEMSTACK)
               SENTENCE)
            (RPLACA (SETQ X (CDAADR (GETP (QUOTE NONE)
                    (QUOTE MEM))))
                    (CONS (CAR X)
                      (CONS (RECONSTRUCT (CAAR (ADVANCE MEMSTACK)))
                        (CDAR X))))))
         (SETQ MEMSTACKL (CDR MEMSTACK))
         (GO LP)
   )))

(BCONC
 (LAMBDA (WHAT LIST)
   (COND
     ((NULL LIST)
      (CONS (SETQ LIST (CONS NIL WHAT))
        LIST))
     ((NULL (CAR LIST))
      (RLACA LIST (CDR (RPLACD LIST (CONS NIL WHAT)))))
     ((RPLACA LIST (CAR (RPLACA (CAR LIST)
              (RPLACA (CONS LIST WHAT)
                NIL))))))))
(RPLOQ
 (NLAMBDA RPLQ
          (RPLACD (CAR RPLQ)
            (CDR RPLQ))))
