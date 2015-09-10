*  /DOCFNS/   31 JULY 1969  1007:42

(TEST4
  (LAMBDA (CS L)
    (PROG NIL
       LP  (COND
             ((GETP CS (CAR L))
               (RETURN T))
             ((SETQ L (CDR L))
               (GO LP)))
           (RETURN NIL)
        )))

(ADVANCE
  (LAMBDA (RULES)
    (RPLACA (CDAR RULES)
            (COND
               ((NULL (CDADAR RULES))
                 (CDDAR RULES))
               ((CDADAR RULES))))))

(RECONSTRUCT
  (LAMBDA (RULE PF)
    (PROG (SENT CR V1 V2 TPF QMF)
          (COND
            ((NULL PF)
             (SETQ SENT (CONS))))
      LP  (COND
            ((NULL RULE)
              (COND
                (PF (COND
                    ((NULL QMF)
                      (PRINT1 (QUOTE ?))))
                  (TERPRI)))
              (RETURN (CAR SENT)))
            ((NUMBERP (SETQ CR (CAR RULE)))
              (GO T1))
            (PF (COND
                ((MEMBER CR TRMLS)
                  (PRINT1 CR)
                  (SETQ QMF T))
                (T (COND
                    (TPF (SPACES 1))
                    (T (TERPRI)
                      (SETQ TPF T)))
                  (PRIN1 CR))))
            (T (TCONC CR SENT)))
      T3  (SETQ RULE (CDR RULE))
          (GO LP)
      T1  (SETQ V1 (CAR (SETQ CR (NTH PARSELIST CR))))
          (SETQ V2 (CADR CR))
      T2  (COND
              
