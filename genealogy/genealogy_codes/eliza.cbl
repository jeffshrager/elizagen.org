       IDENTIFICATION DIVISION.

       PROGRAM-ID.             ELIZA.
      *AUTHOR.                 ARNOLD J. TREMBLEY.
      *DATE-WRITTEN.           2017-10-01.
      *SECURITY.               THIS PROGRAM IS PUBLIC DOMAIN FREEWARE.

      ****************************************************************
      *                                                              *
      *    https://en.wikipedia.org/wiki/ELIZA                       *
      *    ELIZA is an early natural language processing program     *
      *    created around 1964 by Joseph Wiezenbaum at MIT.  This    *
      *    version is adapted from ELIZA.BAS which appeared in       *
      *    Creative Computing magazine in 1977, written by Jeff      *
      *    Shrager and adapted for IBM PC in the early 1980's by     *
      *    Patricia Danielson and Paul Hashfield.                    *
      *                                                              *
      *    COBOL translation by Arnold Trembley, 2017-10-01.         *
      *    arnold.trembley@att.net                                   *
      *    Using MinGW GnuCOBOL 2.2 for Windows 7.                   *
      *    This version is public domain freeware.                   *
      *                                                              *
      *    ELIZA simulates a psychotherapist interacting with a      *
      *    human patient. Enter "shut up" to stop the dialog.        *
      *                                                              *
      ****************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  100-PROGRAM-FLAGS.
           05  100-EOF-FLAG                PIC X(01)   VALUE SPACE.
               88  88-100-ALL-DONE                     VALUE "Y".
           05  100-KEYWORD-FLAG            PIC X(01)   VALUE SPACE.
               88  88-100-KEYWORD-FOUND                VALUE "Y".
               88  88-100-KEYWORD-NOT-FOUND            VALUE "N".

       01  200-USER-INPUT                  PIC X(80)   VALUE SPACES.

       01  210-USER-INPUT-LC               PIC X(80)   VALUE SPACES.

       01  220-LAST-USER-INPUT             PIC X(80)   VALUE SPACES.

       01  230-TRANSLATED-INPUT            PIC X(80)   VALUE SPACES.

       01  240-REPLY                       PIC X(79)   VALUE SPACES.

       01  250-SUBSTITUTE-WORK             PIC X(100)  VALUE SPACES.

       01  300-PROGRAM-CONSTANTS.
           05  300-MAX-KEYWORD-ENTRIES     PIC S9(4)   COMP VALUE +36.
           05  300-MAX-SCAN-LEN            PIC S9(4)   COMP VALUE +30.
           05  300-SHUT                    PIC X(04)   VALUE "shut".
           05  300-ASTERISK                PIC X(01)   VALUE "*".

       01  400-PROGRAM-COUNTERS.
           05  400-HOLD-KW-LEN             PIC S9(4)   COMP VALUE ZERO.
           05  400-SCAN-LEN                PIC S9(4)   COMP VALUE ZERO.
           05  400-HOLD-500-K              PIC S9(4)   COMP VALUE +0.
           05  400-HOLD-OFFSET             PIC S9(4)   COMP VALUE +0.
           05  400-OFFSET                  PIC S9(4)   COMP VALUE +0.
           05  400-SUB                     PIC S9(4)   COMP VALUE ZERO.
           05  400-SPACES-COUNT            PIC S9(4)   COMP VALUE ZERO.

       01  500-KEYWORD-TABLE-DATA.
           05  FILLER   PIC X(16)  VALUE "07can you ".
           05  FILLER   PIC X(16)  VALUE "05can i ".
           05  FILLER   PIC X(16)  VALUE "07you are ".
           05  FILLER   PIC X(16)  VALUE "06you're ".
           05  FILLER   PIC X(16)  VALUE "07i don't ".
           05  FILLER   PIC X(16)  VALUE "06i feel  ".
           05  FILLER   PIC X(16)  VALUE "13why don't you ".
           05  FILLER   PIC X(16)  VALUE "11why can't i ".
           05  FILLER   PIC X(16)  VALUE "07are you ".
           05  FILLER   PIC X(16)  VALUE "07i can't ".
           05  FILLER   PIC X(16)  VALUE "04i am ".
           05  FILLER   PIC X(16)  VALUE "03i'm  ".
           05  FILLER   PIC X(16)  VALUE "03you ".
           05  FILLER   PIC X(16)  VALUE "06i want ".
           05  FILLER   PIC X(16)  VALUE "04what ".
           05  FILLER   PIC X(16)  VALUE "03how ".
           05  FILLER   PIC X(16)  VALUE "03who ".
           05  FILLER   PIC X(16)  VALUE "05where ".
           05  FILLER   PIC X(16)  VALUE "04when ".
           05  FILLER   PIC X(16)  VALUE "03why ".
           05  FILLER   PIC X(16)  VALUE "04name ".
           05  FILLER   PIC X(16)  VALUE "05cause ".
           05  FILLER   PIC X(16)  VALUE "05sorry ".
           05  FILLER   PIC X(16)  VALUE "05dream ".
           05  FILLER   PIC X(16)  VALUE "05hello ".
           05  FILLER   PIC X(16)  VALUE "02hi ".
           05  FILLER   PIC X(16)  VALUE "05maybe ".
           05  FILLER   PIC X(16)  VALUE "02no ".
           05  FILLER   PIC X(16)  VALUE "04your ".
           05  FILLER   PIC X(16)  VALUE "06always ".
           05  FILLER   PIC X(16)  VALUE "05think ".
           05  FILLER   PIC X(16)  VALUE "05alike ".
           05  FILLER   PIC X(16)  VALUE "03yes ".
           05  FILLER   PIC X(16)  VALUE "06friend ".
           05  FILLER   PIC X(16)  VALUE "08computer ".
           05  FILLER   PIC X(16)  VALUE "10NOKEYFOUND".

       01  500-KEYWORD-TABLE       REDEFINES 500-KEYWORD-TABLE-DATA.
           05  500-KEYWORD-ENTRY       OCCURS 36 TIMES
                                       INDEXED BY 500-K.
               10  500-KW-LEN              PIC 9(02).
               10  500-KEYWORD             PIC X(14).

       01  520-TRANSLATION-CONSTANTS. 
           05 520-THING-IN                 PIC X(05)   VALUE "thing". 
           05 520-HIGH-IN                  PIC X(04)   VALUE "high". 
           05 520-SHI-IN                   PIC X(03)   VALUE "shi". 
           05 520-CHI-IN                   PIC X(03)   VALUE "chi". 
           05 520-HIT-IN                   PIC X(03)   VALUE "hit". 
           05 520-OUR-IN                   PIC X(03)   VALUE "our".
           05 520-QMARK-IN                 PIC X(02)   VALUE "? ".  
           05 520-XMARK-IN                 PIC X(02)   VALUE "! ".  
           05 520-FSTOP-IN                 PIC X(02)   VALUE ". ".  

           05 520-THING-OUT                PIC X(05)   VALUE "th!ng". 
           05 520-HIGH-OUT                 PIC X(04)   VALUE "h!gh". 
           05 520-SHI-OUT                  PIC X(03)   VALUE "sh!". 
           05 520-CHI-OUT                  PIC X(03)   VALUE "ch!". 
           05 520-HIT-OUT                  PIC X(03)   VALUE "h!t". 
           05 520-OUR-OUT                  PIC X(03)   VALUE "0ur". 
           05 520-QMARK-OUT                PIC X(02)   VALUE "  ".  
           05 520-FSTOP-OUT                PIC X(02)   VALUE "  ".  

           05 520-ARE-IN                   PIC X(05)   VALUE " are ". 
           05 520-WERE-IN                  PIC X(06)   VALUE " were ".
           05 520-YOU-IN                   PIC X(05)   VALUE " you ".
           05 520-YOUR-IN                  PIC X(06)   VALUE " your ".
           05 520-MY-IN                    PIC X(04)   VALUE " my ".
           05 520-IVE-IN                   PIC X(06)   VALUE " i've ".
           05 520-IM-IN                    PIC X(05)   VALUE " i'm ".
           05 520-I-AM-IN                  PIC X(06)   VALUE " i am ".
           05 520-ME-IN                    PIC X(04)   VALUE " me ".
           05 520-I-IN                     PIC X(03)   VALUE " i ".
           05 520-YOURE-IN                 PIC X(08)   VALUE " you're ". 
           05 520-YOU-ARE-IN           PIC X(09)   VALUE " you are ". 
           05 520-YOURSELF-IN          PIC X(10)   VALUE " yourself ". 

           05 520-AM-OUT                   PIC X(04)   VALUE " am ". 
           05 520-WAS-OUT                  PIC X(05)   VALUE " was ".
           05 520-I-FIX                    PIC X(04)   VALUE " i# ".
           05 520-IM-FIX                   PIC X(06)   VALUE " i'm# ".
           05 520-I-AM-FIX                 PIC X(07)   VALUE " i am# ". 
           05 520-MY-FIX                   PIC X(05)   VALUE " my# ".
           05 520-YOUR-FIX                 PIC X(07)   VALUE " your# ".
           05 520-YOUVE-OUT                PIC X(08)   VALUE " you've ".
           05 520-YOURE-OUT                PIC X(08)   VALUE " you're ".
           05 520-YOU-FIX                  PIC X(06)   VALUE " you# ".
           05 520-MYSELF-OUT               PIC X(08)   VALUE " myself ". 

           05 520-I-OUT                    PIC X(03)   VALUE " I ".
           05 520-IM-OUT                   PIC X(05)   VALUE " I'm ".
           05 520-I-AM-OUT                 PIC X(06)   VALUE " I am ".
           05 520-MY-OUT                   PIC X(04)   VALUE " my ".
           05 520-YOUR-OUT                 PIC X(06)   VALUE " your ".
           05 520-YOU-OUT                  PIC X(05)   VALUE " you ".


       01  540-REPLY-TABLE-DATA.
           05  PIC x(60)   VALUE "29Don't you believe that I can*".
           05  PIC X(60)   VALUE "29Perhaps you would like me to*".
           05  PIC x(60)   VALUE "29Do you want me to be able to*".
           05  PIC x(60)   VALUE "26Perhaps you don't want to*".
           05  PIC x(60)   VALUE "26Do you want to be able to*".
           05  PIC x(60)   VALUE "26What makes you think I am*".

           05  PIC X(30)   VALUE "35Does it please you to believ".
           05  PIC X(30)   VALUE "e I am*".

           05  PIC x(60)   VALUE "29Perhaps you would like to be*".

           05  PIC X(30)   VALUE "31Do you sometimes wish you we".
           05  PIC X(30)   VALUE "re*".

           05  PIC x(60)   VALUE "17Don't you really*".
           05  PIC x(60)   VALUE "14Why don't you*".
           05  PIC x(60)   VALUE "26Do you wish to be able to*".
           05  PIC x(60)   VALUE "22Does that trouble you?".
           05  PIC x(60)   VALUE "18Do you often feel*".
           05  PIC x(60)   VALUE "18Do you often feel*".
           05  PIC x(60)   VALUE "21Do you enjoy feeling*".
           05  PIC x(60)   VALUE "30Do you really believe I don't*".
           05  PIC x(60)   VALUE "28Perhaps in good time I will*".
           05  PIC x(60)   VALUE "18Do you want me to*".

           05  PIC X(30)   VALUE "35Do you think you should be a".
           05  PIC X(30)   VALUE "ble to*".

           05  PIC x(60)   VALUE "14Why can't you*".

           05  PIC X(30)   VALUE "46Why are you interested in wh".
           05  PIC X(30)   VALUE "ether or not I am*".

           05  PIC x(60)   VALUE "31Would you prefer if I were not*".
           05  PIC x(60)   VALUE "31Perhaps in your fantasies I am*".
           05  PIC x(60)   VALUE "26How do you know you can't*".
           05  PIC x(60)   VALUE "15Have you tried?".
           05  PIC x(60)   VALUE "20Perhaps you can now*".

           05  PIC X(30)   VALUE "35Did you come to me because y".
           05  PIC X(30)   VALUE "ou are*".

           05  PIC x(60)   VALUE "23How long have you been*".

           05  PIC X(30)   VALUE "34Do you believe it is normal ".
           05  PIC X(30)   VALUE "to be*".

           05  PIC x(60)   VALUE "19Do you enjoy being*".
           05  PIC x(60)   VALUE "31We were discussing you--not me.".
           05  PIC x(60)   VALUE "06Oh, I*".

           05  PIC X(30)   VALUE "44You're not really talking ab".
           05  PIC X(30)   VALUE "out me, are you?".

           05  PIC X(30)   VALUE "37What would it mean to you if".
           05  PIC X(30)   VALUE " you got*".

           05  PIC x(60)   VALUE "16Why do you want*".
           05  PIC x(60)   VALUE "21Suppose you soon got*".
           05  PIC x(60)   VALUE "22What if you never got*".
           05  PIC x(60)   VALUE "22I sometimes also want*".
           05  PIC x(60)   VALUE "15Why do you ask?".
           05  PIC x(60)   VALUE "32Does that question interest you?".

           05  PIC X(30)   VALUE "38What answer would please you".
           05  PIC X(30)   VALUE " the most?".

           05  PIC x(60)   VALUE "18What do you think?".

           05  PIC X(30)   VALUE "38Are such questions on your m".
           05  PIC X(30)   VALUE "ind often?".

           05  PIC X(30)   VALUE "40What is it that you really w".
           05  PIC X(30)   VALUE "ant to know?".

           05  PIC x(60)   VALUE "27Have you asked anyone else?".

           05  PIC X(30)   VALUE "37Have you asked such question".
           05  PIC X(30)   VALUE "s before?".

           05  PIC X(30)   VALUE "42What else comes to mind when".
           05  PIC X(30)   VALUE " you ask that?".

           05  PIC x(60)   VALUE "24Names don't interest me.".

           05  PIC X(30)   VALUE "41I don't care about names -- ".
           05  PIC X(30)   VALUE "Please go on.".

           05  PIC x(60)   VALUE "24Is that the real reason?".

           05  PIC X(30)   VALUE "37Don't any other reasons come".
           05  PIC X(30)   VALUE " to mind?".

           05  PIC X(30)   VALUE "39Does that reason explain any".
           05  PIC X(30)   VALUE "thing else?".

           05  PIC X(30)   VALUE "34What other reasons might the".
           05  PIC X(30)   VALUE "re be?".

           05  PIC x(60)   VALUE "23Please don't apologize!".
           05  PIC x(60)   VALUE "28Apologies are not necessary.".

           05  PIC X(30)   VALUE "45What feelings do you have wh".
           05  PIC X(30)   VALUE "en you apologize?".

           05  PIC x(60)   VALUE "22Don't be so defensive!".

           05  PIC X(30)   VALUE "36What does that dream suggest".
           05  PIC X(30)   VALUE " to you?".

           05  PIC x(60)   VALUE "19Do you dream often?".

           05  PIC X(30)   VALUE "35What persons appear in your ".
           05  PIC X(30)   VALUE "dreams?".

           05  PIC X(30)   VALUE "33Are you disturbed by your dr".
           05  PIC X(30)   VALUE "eams?".

           05  PIC X(30)   VALUE "43How do you do ...Please stat".
           05  PIC X(30)   VALUE "e your problem.".

           05  PIC x(60)   VALUE "29You don't seem quite certain.".
           05  PIC x(60)   VALUE "23Why the uncertain tone?".
           05  PIC x(60)   VALUE "27Can't you be more positive?".
           05  PIC x(60)   VALUE "16You aren't sure?".
           05  PIC x(60)   VALUE "15Don't you know?".

           05  PIC X(30)   VALUE "38Are you saying no just to be".
           05  PIC X(30)   VALUE " negative?".

           05  PIC x(60)   VALUE "29You are being a bit negative.".
           05  PIC x(60)   VALUE "08Why not?".
           05  PIC x(60)   VALUE "13Are you sure?".
           05  PIC x(60)   VALUE "07Why no?".
           05  PIC x(60)   VALUE "31Why are you concerned about my*".
           05  PIC x(60)   VALUE "20What about your own*".

           05  PIC X(30)   VALUE "36Can you think of a specific ".
           05  PIC X(30)   VALUE "example?".

           05  PIC x(60)   VALUE "05When?".
           05  PIC x(60)   VALUE "25What are you thinking of?".
           05  PIC x(60)   VALUE "15Really, always?".
           05  PIC x(60)   VALUE "23Do you really think so?".
           05  PIC x(60)   VALUE "21But you are not sure*".
           05  PIC x(60)   VALUE "13Do you doubt*".
           05  PIC x(60)   VALUE "12In what way?".
           05  PIC x(60)   VALUE "28What resemblance do you see?".

           05  PIC X(30)   VALUE "40What does the similarity sug".
           05  PIC X(30)   VALUE "gest to you?".

           05  PIC X(30)   VALUE "34What other connections do yo".
           05  PIC X(30)   VALUE "u see?".

           05  PIC X(30)   VALUE "38Could there really be some c".
           05  PIC X(30)   VALUE "onnection?".

           05  PIC x(60)   VALUE "04How?".
           05  PIC x(60)   VALUE "24You seem quite positive.".
           05  PIC x(60)   VALUE "13Are you sure?".
           05  PIC x(60)   VALUE "06I see.".
           05  PIC x(60)   VALUE "13I understand.".

           05  PIC X(30)   VALUE "41Why do you bring up the topi".
           05  PIC X(30)   VALUE "c of friends?".

           05  PIC x(60)   VALUE "26Do your friends worry you?".
           05  PIC x(60)   VALUE "28Do your friends pick on you?".

           05  PIC X(30)   VALUE "34Are you sure you have any fr".
           05  PIC X(30)   VALUE "iends?".

           05  PIC x(60)   VALUE "30Do you impose on your friends?".

           05  PIC X(30)   VALUE "42Perhaps your love for friend".
           05  PIC X(30)   VALUE "s worries you.".

           05  PIC x(60)   VALUE "23Do computers worry you?".

           05  PIC X(30)   VALUE "39Are you talking about me in ".
           05  PIC X(30)   VALUE "particular?".

           05  PIC X(30)   VALUE "31Are you frightened by machin".
           05  PIC X(30)   VALUE "es?".

           05  PIC x(60)   VALUE "29Why do you mention computers?".

           05  PIC X(30)   VALUE "56What do you think machines h".
           05  PIC X(30)   VALUE "ave to do with your problem?".

           05  PIC X(30)   VALUE "42Don't you think computers ca".
           05  PIC X(30)   VALUE "n help people?".

           05  PIC X(30)   VALUE "43What is it about machines th".
           05  PIC X(30)   VALUE "at worries you?".

           05  PIC X(30)   VALUE "44Say, do you have any psychol".
           05  PIC X(30)   VALUE "ogical problems?".

           05  PIC x(60)   VALUE "30What does that suggest to you?".
           05  PIC x(60)   VALUE "06I see.".

           05  PIC X(30)   VALUE "36I'm not sure I understand yo".
           05  PIC X(30)   VALUE "u fully.".

           05  PIC X(30)   VALUE "36Come, Come, elucidate your t".
           05  PIC X(30)   VALUE "houghts.".

           05  PIC x(60)   VALUE "26Can you elaborate on that?".
           05  PIC x(60)   VALUE "26That is quite interesting.".

       01  540-REPLY-TABLE         REDEFINES 540-REPLY-TABLE-DATA.
           05  540-REPLY-ENTRY         OCCURS 112 TIMES
                                       INDEXED BY 540-R.
               10  540-REPLY-LENGTH        PIC 9(02).
               10  540-REPLY               PIC X(58).


       01  560-REPLY-LOCATER-DATA.
           05  FILLER      PIC X(12)   VALUE "000100030004".
           05  FILLER      PIC X(12)   VALUE "000400050005".
           05  FILLER      PIC X(12)   VALUE "000600090009".
           05  FILLER      PIC X(12)   VALUE "000600090009".
           05  FILLER      PIC X(12)   VALUE "001000130013".
           05  FILLER      PIC X(12)   VALUE "001400160016".
           05  FILLER      PIC X(12)   VALUE "001700190019".
           05  FILLER      PIC X(12)   VALUE "002000210021".
           05  FILLER      PIC X(12)   VALUE "002200240024".
           05  FILLER      PIC X(12)   VALUE "002500270027".
           05  FILLER      PIC X(12)   VALUE "002800310031".
           05  FILLER      PIC X(12)   VALUE "002800310031".
           05  FILLER      PIC X(12)   VALUE "003200340034".
           05  FILLER      PIC X(12)   VALUE "003500390039".
           05  FILLER      PIC X(12)   VALUE "004000480048".
           05  FILLER      PIC X(12)   VALUE "004000480048".
           05  FILLER      PIC X(12)   VALUE "004000480048".
           05  FILLER      PIC X(12)   VALUE "004000480048".
           05  FILLER      PIC X(12)   VALUE "004000480048".
           05  FILLER      PIC X(12)   VALUE "004000480048".
           05  FILLER      PIC X(12)   VALUE "004900500050".
           05  FILLER      PIC X(12)   VALUE "005100540054".
           05  FILLER      PIC X(12)   VALUE "005500580058".
           05  FILLER      PIC X(12)   VALUE "005900620062".
           05  FILLER      PIC X(12)   VALUE "006300630063".
           05  FILLER      PIC X(12)   VALUE "006300630063".
           05  FILLER      PIC X(12)   VALUE "006400680068".
           05  FILLER      PIC X(12)   VALUE "006900730073".
           05  FILLER      PIC X(12)   VALUE "007400750075".
           05  FILLER      PIC X(12)   VALUE "007600790079".
           05  FILLER      PIC X(12)   VALUE "008000820082".
           05  FILLER      PIC X(12)   VALUE "008300890089".
           05  FILLER      PIC X(12)   VALUE "009000920092".
           05  FILLER      PIC X(12)   VALUE "009300980098".
           05  FILLER      PIC X(12)   VALUE "009901050105".
           05  FILLER      PIC X(12)   VALUE "010601120112".

       01  560-REPLY-LOCATER-TABLE REDEFINES 560-REPLY-LOCATER-DATA.
           05  560-REPLY-LOCATER-ENTRY OCCURS 36 TIMES INDEXED BY 560-L.
               10  560-REPLY-LO            PIC 9(04).
               10  560-REPLY-HI            PIC 9(04).
               10  560-REPLY-LAST-USED     PIC 9(04).

       01  600-PROGRAM-MESSAGES.
           05  600-REPLY-LIST.
               10  FILLER                  PIC X(07)   VALUE 'Reply: '.
               10  600-REPLY-DATA          PIC X(70)   VALUE SPACES.

           05  600-INITIAL-MESSAGE         PIC X(40)   VALUE
               "Hi!  I'm ELIZA.  What's your problem?".

           05  600-GOODBYE-MESSAGE         PIC X(40)   VALUE
               "If that's how you feel--goodbye...".

           05  600-NO-REPEAT-MSG           PIC X(32)   VALUE
               "Please don't repeat yourself!".

       PROCEDURE DIVISION.

      ****************************************************************
      *    0 0 0 0 - M A I N L I N E .                               *
      ****************************************************************
      *    START THE PSYCHOTHERAPIST DIALOG WITH THE USER, ANALYZE   *
      *    THE USER INPUT AND GENERATE THE REPLIES.  THE USER CAN    *
      *    TYPE "SHUT UP" OR SIMPLY "SHUT" TO TERMINATE THE SESSION. *
      ****************************************************************

       0000-MAINLINE.

           DISPLAY SPACE
           MOVE SPACE                  TO 100-EOF-FLAG
           DISPLAY 600-INITIAL-MESSAGE
           PERFORM UNTIL 88-100-ALL-DONE
               ACCEPT 200-USER-INPUT
               MOVE FUNCTION LOWER-CASE (200-USER-INPUT)
                                       TO 210-USER-INPUT-LC
               IF 210-USER-INPUT-LC (1:4) = 300-SHUT
                   SET 88-100-ALL-DONE TO TRUE
                   DISPLAY 600-GOODBYE-MESSAGE
               ELSE
                   IF 210-USER-INPUT-LC = 220-LAST-USER-INPUT
                       DISPLAY 600-NO-REPEAT-MSG
                   ELSE
                       MOVE 210-USER-INPUT-LC 
                                       TO 220-LAST-USER-INPUT
                       PERFORM 1000-SCAN-FOR-KEYWORD
                       IF 400-HOLD-OFFSET > ZERO
                           PERFORM 2000-TRANSLATE-USER-INPUT
                       END-IF
                       PERFORM 3000-BUILD-KEYWORD-REPLY
                   END-IF
               END-IF
           END-PERFORM

           STOP RUN.

      ****************************************************************
      *    1 0 0 0 - S C A N - F O R - K E Y W O R D .               *
      ****************************************************************
      *    SEARCH THE USER INPUT FOR KEYWORDS THAT WILL TRIGGER      *
      *    THE RESPONSES FROM THE REPLY TABLE.                       *
      ****************************************************************

       1000-SCAN-FOR-KEYWORD.

           PERFORM 1100-MASK-STRING-HI                                 

           SET 88-100-KEYWORD-NOT-FOUND TO TRUE
           MOVE ZERO                   TO 400-HOLD-OFFSET
           PERFORM VARYING 400-SUB FROM +1 BY +1
                   UNTIL   400-SUB > 300-MAX-SCAN-LEN
                   OR      88-100-KEYWORD-FOUND
               PERFORM VARYING 500-K FROM +1 BY +1
                       UNTIL   500-K > 300-MAX-KEYWORD-ENTRIES
                       OR      88-100-KEYWORD-FOUND
                   MOVE 500-KW-LEN (500-K)
                                       TO 400-HOLD-KW-LEN
                   IF 210-USER-INPUT-LC (400-SUB:400-HOLD-KW-LEN) =
                           500-KEYWORD (500-K)
                       SET 400-HOLD-500-K TO 500-K
                       SET 88-100-KEYWORD-FOUND TO TRUE
                       COMPUTE 400-HOLD-OFFSET =
                           400-SUB + 400-HOLD-KW-LEN
                       COMPUTE 400-SUB = 400-SCAN-LEN + 1
                   END-IF
               END-PERFORM
           END-PERFORM

           IF 88-100-KEYWORD-NOT-FOUND
               MOVE 300-MAX-KEYWORD-ENTRIES
                                       TO 400-HOLD-500-K
               SET 88-100-KEYWORD-FOUND TO TRUE
           END-IF

           PERFORM 1200-RESTORE-STRING-HI                              
           .

      ****************************************************************
      *    1 1 0 0 - M A S K - S T R I N G - H I .                   *
      ****************************************************************
      *    WORDS LIKE "THING" AND "HIGH" WERE CAUSING A KEYWORD      *
      *    "HI" MATCH THAT TRIGGERED THE HELLO/HI KEYWORD RESPONSES, *
      *    SO THEY ARE MASKED HERE TO PREVENT THAT.                  *
      *    ALSO REMOVE TRAILING "?", "!", AND "." CHARACTERS.        *
      ****************************************************************

       1100-MASK-STRING-HI.          
                                                
           MOVE FUNCTION SUBSTITUTE 
               (210-USER-INPUT-LC, 520-THING-IN, 520-THING-OUT, 
                                   520-HIGH-IN,  520-HIGH-OUT, 
                                   520-SHI-IN,   520-SHI-OUT,  
                                   520-CHI-IN,   520-CHI-OUT,  
                                   520-HIT-IN,   520-HIT-OUT,  
                                   520-OUR-IN,   520-OUR-OUT, 
                                   520-QMARK-IN, 520-QMARK-OUT, 
                                   520-XMARK-IN, 520-QMARK-OUT, 
                                   520-FSTOP-IN, 520-FSTOP-OUT) 
                                       TO 250-SUBSTITUTE-WORK 
           MOVE 250-SUBSTITUTE-WORK    TO 210-USER-INPUT-LC    
      ****************************************************************
      *    REMOVE MULTIPLE TRAILING QUESTION MARKS, EXCLAMATION      *
      *    POINTS, AND PERIODS (FULL STOPS).                         *
      ****************************************************************
           MOVE FUNCTION SUBSTITUTE 
               (210-USER-INPUT-LC, 520-QMARK-IN, 520-QMARK-OUT, 
                                   520-XMARK-IN, 520-QMARK-OUT, 
                                   520-FSTOP-IN, 520-FSTOP-OUT) 
                                       TO 250-SUBSTITUTE-WORK 
           MOVE 250-SUBSTITUTE-WORK    TO 210-USER-INPUT-LC    
           MOVE FUNCTION SUBSTITUTE 
               (210-USER-INPUT-LC, 520-QMARK-IN, 520-QMARK-OUT, 
                                   520-XMARK-IN, 520-QMARK-OUT, 
                                   520-FSTOP-IN, 520-FSTOP-OUT) 
                                       TO 250-SUBSTITUTE-WORK 
           MOVE 250-SUBSTITUTE-WORK    TO 210-USER-INPUT-LC    
           .

      ****************************************************************
      *    1 2 0 0 - R E S T O R E - S T R I N G - H I .             *
      ****************************************************************
      *    AFTER COMPLETING THE KEYWORD SEARCH, RESTORE THE "HI"     *
      *    STRING IN THE USER INPUT.                                 *
      ****************************************************************

       1200-RESTORE-STRING-HI.       
                                                
           MOVE FUNCTION SUBSTITUTE 
               (210-USER-INPUT-LC, 520-THING-OUT, 520-THING-IN, 
                                   520-HIGH-OUT,  520-HIGH-IN, 
                                   520-SHI-OUT,   520-SHI-IN,  
                                   520-CHI-OUT,   520-CHI-IN,  
                                   520-HIT-OUT,   520-HIT-IN,  
                                   520-OUR-OUT,   520-OUR-IN) 
                                       TO 250-SUBSTITUTE-WORK 
           MOVE 250-SUBSTITUTE-WORK    TO 210-USER-INPUT-LC    
           .

      ****************************************************************
      *    2 0 0 0 - T R A N S L A T E - U S E R - I N P U T .       *
      ****************************************************************
      *    PERFORM PRONOUN REPLACEMENT AND CONJUGATION ON THE USER   *
      *    INPUT SO IT WILL SOUND FAIRLY NORMAL WHEN APPENDED TO     *
      *    THE DOCTOR'S REPLY.                                       *
      ****************************************************************

       2000-TRANSLATE-USER-INPUT.
       
           MOVE 210-USER-INPUT-LC (400-HOLD-OFFSET:)
                                       TO 230-TRANSLATED-INPUT.
                                                
           MOVE FUNCTION SUBSTITUTE 
               (230-TRANSLATED-INPUT, 520-ARE-IN,  520-AM-OUT,       
                                      520-WERE-IN, 520-WAS-OUT       
                                      520-YOU-IN,  520-I-FIX,     
                                      520-YOUR-IN, 520-MY-FIX,        
                                      520-MY-IN,   520-YOUR-FIX,        
                                      520-IVE-IN,  520-YOUVE-OUT,       
                                      520-IM-IN,   520-YOURE-OUT,    
                                      520-I-AM-IN, 520-YOURE-OUT,    
                                      520-ME-IN,   520-YOU-FIX,      
                                      520-I-IN,    520-YOU-FIX,      
                                      520-YOURE-IN 520-IM-FIX,      
                                  520-YOU-ARE-IN   520-I-AM-FIX,      
                                  520-YOURSELF-IN, 520-MYSELF-OUT)
                                       TO 250-SUBSTITUTE-WORK.  

           MOVE 250-SUBSTITUTE-WORK TO 230-TRANSLATED-INPUT. 
                              
           MOVE FUNCTION SUBSTITUTE 
               (230-TRANSLATED-INPUT, 520-I-FIX,     520-I-OUT, 
                                      520-IM-FIX,    520-IM-OUT, 
                                      520-I-AM-FIX,  520-I-AM-OUT, 
                                      520-MY-FIX,    520-MY-OUT, 
                                      520-YOUR-FIX,  520-YOUR-OUT, 
                                      520-YOU-FIX,   520-YOU-OUT) 
                                       TO 250-SUBSTITUTE-WORK.
                                                
           MOVE 250-SUBSTITUTE-WORK    TO 230-TRANSLATED-INPUT 
           .

      ****************************************************************
      *    3 0 0 0 - B U I L D - K E Y W O R D - R E P L Y .         *
      ****************************************************************
      *    BUILD THE REPLY BASED ON THE KEYWORD FOUND IN THE USER    *
      *    INPUT.  NOTE THERE ARE A VARIABLE NUMBER OF POSSIBLE      *
      *    REPLIES FOR EACH KEYWORD, AND SOME REPLIES INCLUDE TEXT   *
      *    ECHOED FROM THE USER INPUT.                               *
      ****************************************************************

       3000-BUILD-KEYWORD-REPLY.

           SET 560-L                   TO 400-HOLD-500-K
           ADD +1                      TO 560-REPLY-LAST-USED (560-L)
           IF 560-REPLY-LAST-USED (560-L) > 560-REPLY-HI (560-L)
               MOVE 560-REPLY-LO (560-L) TO 560-REPLY-LAST-USED (560-L)
           END-IF

           SET 540-R                    TO 560-REPLY-LAST-USED (560-L)
           MOVE 540-REPLY (540-R)       TO 240-REPLY
           MOVE 540-REPLY-LENGTH (540-R)    TO 400-SUB
           IF 240-REPLY (400-SUB:1) = 300-ASTERISK
               MOVE SPACE               TO 240-REPLY (400-SUB:1) 
               MOVE 230-TRANSLATED-INPUT                           
                                        TO 240-REPLY (400-SUB:)
               PERFORM 3100-FIX-MORE-BAD-GRAMMAR                 
               MOVE ZERO                TO 400-SPACES-COUNT         
               INSPECT 240-REPLY TALLYING 400-SPACES-COUNT         
                   FOR TRAILING SPACES                                   
      ****************************************************************
      *        MERGE USER INPUT INTO THE REPLY AND THEN CORRECT      *
      *        ENDING PUNCTUATION FOR "?" OR "." (FULL-STOP).        *
      ****************************************************************
               IF  400-SPACES-COUNT > ZERO                          
               AND 400-SPACES-COUNT < (LENGTH OF 240-REPLY) - 1                      
                   COMPUTE 400-OFFSET =                                
                       (LENGTH OF 240-REPLY) - 400-SPACES-COUNT + 1    
                   END-COMPUTE                                         
                   IF 560-REPLY-LAST-USED (560-L) = 02 OR 04 OR 05   
                   OR 08 OR 18 OR 24 OR 33 OR 39 OR 81                    
                       MOVE "."         TO 240-REPLY (400-OFFSET:1) 
                   ELSE   
                       MOVE "?"         TO 240-REPLY (400-OFFSET:1) 
                   END-IF
               END-IF                                                
           END-IF

           DISPLAY 240-REPLY                   
           .

      ****************************************************************
      *    3 1 0 0 - F I X - M O R E - B A D - G R A M M A R .       *
      ****************************************************************
      *    HERE ARE SOME MORE FIXUPS FOR GRAMMAR PROBLEMS.  BUT IT   *
      *    DOESN'T SOLVE ALL OF THEM.                                *
      ****************************************************************

       3100-FIX-MORE-BAD-GRAMMAR. 

           MOVE FUNCTION SUBSTITUTE (240-REPLY, 
               " you want I ",            " you want me ",            
               " you got I ",             " you got me ",            
               " to make I ",             " to make me ",            
               " you been I ",            " you been me ",            
               " you be I ",              " you be me ",            
               " to be I ",               " to be me ",            
               " soon got I ",            " soon got me ", 
               " never got I ",           " never got me ",       
               " sometimes also want I ", " sometimes also want me ", 
               " normal to be I ",        " normal to be me ", 
               " enjoy being I ",         " enjoy being me ", 
               " can't make I ",          " can't make me ", 
               " can now make I ",        " can now make me ", 
               " I are ",                 " I am ",            
               " you am ",                " you are ",       
               " with I ",                " with me")       
                                       TO 250-SUBSTITUTE-WORK.  

           MOVE 250-SUBSTITUTE-WORK TO 240-REPLY.              

       END PROGRAM ELIZA.
