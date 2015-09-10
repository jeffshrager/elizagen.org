;   <SOURCES>DOCTOR.;2   TUE 13-JUN-72 10:16AM


              (RETURN
                (RECONSTRUCT
                  [APPEND (QUOTE (IT'S BEEN MY PLEASURE, THAT'S))
                          (CONS (PACK (LIST (QUOTE $)
                                            (QUOTIENT (SETQ TIMON
                                                        (IDIFFERENCE
                                                          (CLOCK)
                                                          TIMON))
                                                      10000)
                                            (QUOTE %,)
                                            (QUOTIENT (REMAINDER TIMON
                                                               10000)
                                                      100)))
                                (QUOTE (PLEASE %,)))
                  T]
           (ANALYZE)
           (GO A])

(MAKESENTENCE
  [LAMBDA NIL
    (PROG (FLAG WORD SENTENCE KEYSTACK)
      A1  (SETQ KEYSTACK (CONS))
          (SETQ SENTENCE (CONS))
      A   (SETQ WORD (RATOM))
          [COND
            ((NUMBERP WORD)
             (SETQ WORD (PACK (LIST (QUOTE *)
                                    WORD]
          [COND
            ((EQ WORD RUBOUT)
             (RETURN (TERPRI)))
            ((MEMB WORD TRMLIS)
             (TERPRI)
             (RETURN (RPLACD SENTENCE KEYSTACK)))
            ((MEMB WORD PCTLIS)
             (COND
               ((NULL (CDR KEYSTACK))
                (GO A1))
               ((NULL (SETQ FLAG (MAKESENTENCE)))
                (RETURN))
               ([AND (CDDR FLAG)
                     (NOT (IGREATERP (GET (CDR KEYSTACK)
                                          (QUOTE PRIORITY))
                                     (GET (CDR FLAG)
                                          (QUOTE PRIORITY]
                (RETURN FLAG))
               (T (RETURN (RPLACD SENTENCE KEYSTACK]
          (TCONC SENTENCE (COND
                   ((GETP WORD (QUOTE TRANSLATION)))
                   (WORD)))
