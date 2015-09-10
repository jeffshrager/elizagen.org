;; eliza-page-19720424-doctorp1-01-of-06.tiff

; <source8>doctor.;2    TUE 13-JUN-72 10:16AM                 PAGE 1:1

;; TRANSCRIBED BY PAUL NATHAN - pnathan@vandals.uidaho.edu

(RETURN
  (RECONSTRUCT
   (APPEND (QUOTE (ITS BEEN MY PLEASURE, THAT'S))
           (CONS (PACK (LIST (QUOTE S)
                             (QUOTIENT (SETQ TIMON
                                             (IDIFFERENCE
                                              (CLOCK)
                                              TIMON))
                                       10000)
                             (QUOTE %.)
                             (QUOTIENT (REMAINDER TIMON
                                                  10000)
                                       100)))
                 (QUOTE (PLEASE %.]     ; not sure if this is a . or ,
                                T]      ; this terminates a prior file
(ANALYZE)
(GO A])

(MAKESENTENCE
 (LAMBDA NIL
   (PROG (FLAG WORD SENTENCE KEYSTACK)
      A1 (SETQ KEYSTACK (CONS))  ; A1 IS A LABEL
      (SETQ SENTENCE (CONS))
      A (SETQ WORD (RATOM))             ; A LABEL
      [COND
      ((NUMBERP WORD)
       (SETQ WORD (PACK (LIST (QUOTE *)
                              WORD]
      [COND
        ((EQ WORD RUBOUT)
         (RETURN (TERPRI)))
        ((MEMB WORD TRMLI8)
         (TERPRI)
         (RETURN (RPLACD SENTENCE KEYSTACK)))
        ((MEMB WORD PCTLI8)
         (COND
           ((NULL (CDR KEYSTACK))
            (GO A1))                      ; A1 IS A LABEL
           ((NULL (SETQ FLAG (MAKESENTENCE)))
            (RETURN))
           ([AND (CDDR FLAG)
                 (NOT IGREATERP (GET (CDR KEYSTACK)
                                     (QUOTE PRIORITY)]
            (RETURN FLAG))
         (T (RETURN  (REPLACD SENTENCE KEYSTACK]
      ;; tconc appears to be at the right place
      (TCONC SENTENCE (COND
               ((GETP WORD (QUOTE TRANSLATION)))
               (WORD)))
