Welcome to GnuCOBOL
====================

ELIZA in GnuCOBOL.  

ELIZA is based on an early natural language processing program created by Joseph 
Wiezenbaum at MIT around 1964.  This version is adapted from ELIZA.BAS which 
appeared in Creative Computing magazine in 1977, written by Jeff Shrager and 
adapted for IBM PC in the early 1980's by Patricia Danielson and Paul Hashfield.  

This COBOL transalation was written by Arnold Trembley on 01 October 2017 
(arnold.trembley@att.net) using MinGW GnuCOBOL 2.2 on Windows 7.  

ELIZA simulates a psychotherapist interacting with a  
human patient. Enter "shut up" to stop the dialog.

ELIZA.BAS can be found at:
https://www.jesperjuul.net/eliza/ELIZA.BAS

For more information on ELIZA:
https://en.wikipedia.org/wiki/ELIZA 
    
Compiling
=========

To compile ELIZA using GnuCOBOL 2.2 with default.conf: 

    cobc -x eliza.cbl 


To run Eliza, simply type ELIZA in the command line.

Here is a sample dialog:


Hi!  I'm ELIZA.  What's your problem?
what's YOUR problem?
Why do you ask?
who wants to know?
Why do you ask?
are you making fun of me?
why are you interested in whether or not I am making fun of you?
because I don't trust you
Is that the real reason?
there is no reason
Are you saying no just to be negative?
yes
Are you sure?
I'm not sure
Did you come to me because you are not sure?
I thought you could help me
We were discussing you--not me.
are you going to help me?
Would you prefer if I were not going to help you?
go to hell
Say, do you have any psychological problems?
shut up
If that's how you feel--goodbye...

=========
Cheers from the GnuCOBOL contributors.
