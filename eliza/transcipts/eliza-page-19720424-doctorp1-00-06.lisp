;; eliza-page-19720424-doctorp1-00-of-06.tiff

; <source8>doctor.;2    TUE 13-JUN-72 10:16AM                 PAGE 1:1

;; TRANSCRIBED BY PAUL NATHAN - pnathan@vandals.uidaho.edu

(PROGN (LISPXPRIN1 (QUOTE "FILE CREATED ")
                   T)
       (LISPXPRIN1 (QUOTE "13-JUN-72 4:20:07")
                   T)
       (LISPXTERPRI T))

(DEFINEQ

(DOCTOR [LAMBDA (FLG)
          (COND
            ((NULL FLG)
             (SETQ MEMSTACK NIL)))
        (PROG (KEYSTACK SENTENCE TIMON)
           (SETSPR (QUOTE (%  %
%
 )))
           (SETBRK (QUOTE (%. , ? ! - %( %) ; : #))   )
           (CONTROL T)
           (SETQ FLIPFLIP 0)
           (SETQ TIMON (CLOCK))
           (RECONSTRUCT (QUOTE (TELL ME YOUR PROBLEMS. PLEASE TERMINATE
                                     INPUT WITH A PERIOD OR A QUESTION
                                                            MARK
                                  %.))
                        T)
           (SETNONE)
      A    (PRIN1 (QUOTE "
*"))
           (COND
             (NULL (SETQ SENTENCE (MAKESENTENCE)))
             (GO A)))
           (SETQ KEYSTACK (CDR SENTENCE))
           (SETQ SENTENCE (CAR SENTENCE))
           ([COND (EQUAL SENTENCE (QUOTE GOODBYE)))
           (* Rate computed at $6 a minute, or $1 per 10
           seconds. CLOCK returns value in milliseconds. )
