package Eliza;
import java.util.*;

/**
Eliza in Java.
Adapted from a BASIC program I found floating on the net.
Eliza was originally written by Joseph Weizenbaum.
This version is an adaption of the program
as it appeared in the memorable magazine Create Computing 
around 1981.
<br>
Jesper Juul - jj@pobox.com.
Copenhagen, February 24th, 1999.
*/

public class ElizaParse
{

public ElizaParse()
{
init();
}

private static String intromsg[]=
{
"**************************",
"ELIZA",
"CREATIVE COMPUTING",
"MORRISTOWN, NEW JERSEY",
"",
"ADAPTED FOR IBM PC BY",
"PATRICIA DANIELSON AND PAUL HASHFIELD",
//"PLEASE DON'T USE COMMAS OR PERIODS IN YOUR INPUTS",
"",
"Java version February 24th, 1999",
"By Jesper Juul - jj@pobox.com.","",
"**************************",
};

String[] getIntroMsg()
{
return intromsg;
}

/**
Vector holding strings that have been added using PRINT.
*/
public Vector msg=new Vector();

/**
Cute hack to make it look like BASIC.
This adds a String to the msg Vector.
*/
public void PRINT(String s)
{
msg.addElement(s);
}

int N1=36,N2=14,N3=112;
int WORDINOUT=7;

int S[]=new int[N1+1];
int R[]=new int[N1+1];
int N[]=new int[N1+1];
String WORDIN[]=new String[WORDINOUT];
String WORDOUT[]=new String[WORDINOUT];


void init()
{
int i;

for (i=0;i<WORDINOUT;i++)
{
 WORDIN[i]=wordinout[i][0];
 WORDOUT[i]=wordinout[i][1];
}

for (int X=1;X<=N1;X++)
{
 S[X]=rightreplies[X*2-2];
 int L=rightreplies[X*2-1];
 R[X]=S[X];
 N[X]=S[X]+L-1;
}



PRINT("HI! I'M ELIZA. WHAT'S YOUR PROBLEM?");

}


public boolean exit=false;

public void handleLine(String I)
{
I="  "+I.toUpperCase()+"  ";

//Remove apostrophes = Line 210-210
I=removeChar(I,'\'');

if (I.indexOf("SHUT")>=0)
{
PRINT ("O.K. IF YOU FEEL THAT WAY I'LL SHUT UP....");
 exit=true;
 return;
}

if (I.equals(lastline))
{
PRINT ("PLEASE DON'T REPEAT YOURSELF!");
return;
}

lastline=I;

int pos=0;
String C="error";
int K;
String F="";
boolean found=false;

for (K=0;K<N1;K++)
{
pos=I.indexOf(KEYWORD[K]);
if (pos>=0)
{
if (K==13)
{
//Should use regionmatches
if (I.indexOf(KEYWORD[29])>=0) K=29;
}
F=KEYWORD[K];
found=true;
break;
}
}

if (found)
{
C=I.substring(pos+F.length()-1);

//Swap strings = Line 430-560
for (int i=0;i<WORDINOUT;i++)
{
 C=replaceString(C,WORDIN[i],WORDOUT[i]);
}
//Remove extra spaces
 C=replaceString(C,"  "," ");
}
else
{
 K=35;
}

//600 F$ = REPLIES$(R(K))
F=REPLIES[R[K+1]];

//610 R(K)=R(K)+1:IF R(K)>N(K) THEN R(K)=S(K)
if (++R[K+1]>N[K+1]) R[K+1]=S[K+1];

//620 IF RIGHT$(F$,1)<>"*" THEN PRINT F$:P$=I$:GOTO 170
if (F.charAt(F.length()-1)!='*')
{
 PRINT (F);
}
else
{
//625 IF C$<>"   " THEN 630
 if (C.equals("   "))
 {
//626 PRINT "YOU WILL HAVE TO ELABORATE MORE FOR ME TO HELP YOU"
PRINT ("YOU WILL HAVE TO ELABORATE MORE FOR ME TO HELP YOU");
//627 GOTO 170
 }
 else
 {
//630 PRINT LEFT$(F$,LEN(F$)-1);C$
//640 P$=I$:GOTO 170
  PRINT(F.substring(0,F.length()-1)+C);
 }
}
}

/**
Remembers the last line typed.
*/
String lastline="-";

/**
Line 1000
*/
static String KEYWORD[]=
{ "CAN YOU ","CAN I ","YOU ARE ","YOU'RE ","I DON'T ","I FEEL ",
 "WHY DON'T YOU ","WHY CAN'T I ","ARE YOU ","I CAN'T ","I AM ","I'M ",
 "YOU ","I WANT ","WHAT ","HOW ","WHO ","WHERE ","WHEN ","WHY ",
 "NAME ","CAUSE ","SORRY ","DREAM ","HELLO ","HI ","MAYBE ",
 "NO","YOUR ","ALWAYS ","THINK ","ALIKE ","YES ","FRIEND ",
 "COMPUTER", "NOKEYFOUND"};

/**
Line 1200
*/
static String wordinout[][]=
{{" ARE "," AM "},{" WERE "," WAS "},{" YOU "," I "},{" YOUR"," MY "},
{" I'VE "," YOU'VE "},{" I'M "," YOU'RE "},
{" ME "," YOU "}};

/**
Line 1300
*/
static String REPLIES[]=
{"DON'T YOU BELIEVE THAT I CAN*",
"PERHAPS YOU WOULD LIKE TO BE LIKE ME*",
"YOU WANT ME TO BE ABLE TO*",
"PERHAPS YOU DON'T WANT TO*",
"DO YOU WANT TO BE ABLE TO*",
"WHAT MAKES YOU THINK I AM*",
"DOES IT PLEASE YOU TO BELIEVE I AM*",
"PERHAPS YOU WOULD LIKE TO BE*",
"DO YOU SOMETIMES WISH YOU WERE*",
"DON'T YOU REALLY*",
"WHY DON'T YOU*",
"DO YOU WISH TO BE ABLE TO*",
"DOES THAT TROUBLE YOU*",
"DO YOU OFTEN FEEL*",
"DO YOU OFTEN FEEL*",
"DO YOU ENJOY FEELING*",
"DO YOU REALLY BELIEVE I DON'T*",
"PERHAPS IN GOOD TIME I WILL*",
"DO YOU WANT ME TO*",
"DO YOU THINK YOU SHOULD BE ABLE TO*",
"WHY CAN'T YOU*",
"WHY ARE YOU INTERESTED IN WHETHER OR NOT I AM*",
"WOULD YOU PREFER IF I WERE NOT*",
"PERHAPS IN YOUR FANTASIES I AM*",
"HOW DO YOU KNOW YOU CAN'T*",
"HAVE YOU TRIED?",
"PERHAPS YOU CAN NOW*",
"DID YOU COME TO ME BECAUSE YOU ARE*",
"HOW LONG HAVE YOU BEEN*",
"DO YOU BELIEVE IT IS NORMAL TO BE*",
"DO YOU ENJOY BEING*",
"WE WERE DISCUSSING YOU--NOT ME.",
"OH, I*",
"YOU'RE NOT REALLY TALKING ABOUT ME, ARE YOU?",
"WHAT WOULD IT MEAN TO YOU IF YOU GOT*",
"WHY DO YOU WANT*",
"SUPPOSE YOU SOON GOT*",
"WHAT IF YOU NEVER GOT*",
"I SOMETIMES ALSO WANT*",
"WHY DO YOU ASK?",
"DOES THAT QUESTION INTEREST YOU?",
"WHAT ANSWER WOULD PLEASE YOU THE MOST?",
"WHAT DO YOU THINK?",
"ARE SUCH QUESTIONS ON YOUR MIND OFTEN?",
"WHAT IS IT THAT YOU REALLY WANT TO KNOW?",
"HAVE YOU ASKED ANYONE ELSE?",
"HAVE YOU ASKED SUCH QUESTIONS BEFORE?",
"WHAT ELSE COMES TO MIND WHEN YOU ASK THAT?",
"NAMES DON'T INTEREST ME.",
"I DON'T CARE ABOUT NAMES --PLEASE GO ON.",
"IS THAT THE REAL REASON?",
"DON'T ANY OTHER REASONS COME TO MIND?",
"DOES THAT REASON EXPLAIN ANYTHING ELSE?",
"WHAT OTHER REASONS MIGHT THERE BE?",
"PLEASE DON'T APOLOGIZE!",
"APOLOGIES ARE NOT NECESSARY.",
"WHAT FEELINGS DO YOU HAVE WHEN YOU APOLOGIZE?",
"DON'T BE SO DEFENSIVE!",
"WHAT DOES THAT DREAM SUGGEST TO YOU?",
"DO YOU DREAM OFTEN?",
"WHAT PERSONS APPEAR IN YOUR DREAMS?",
"ARE YOU DISTURBED BY YOUR DREAMS?",
"HOW DO YOU DO ...PLEASE STATE YOUR PROBLEM.",
"YOU DON'T SEEM QUITE CERTAIN.",
"WHY THE UNCERTAIN TONE?",
"CAN'T YOU BE MORE POSITIVE?",
"YOU AREN'T SURE?",
"DON'T YOU KNOW?",
"ARE YOU SAYING NO JUST TO BE NEGATIVE?",
"YOU ARE BEING A BIT NEGATIVE.",
"WHY NOT?",
"ARE YOU SURE?",
"WHY NO?",
"WHY ARE YOU CONCERNED ABOUT MY*",
"WHAT ABOUT YOUR OWN*",
"CAN YOU THINK OF A SPECIFIC EXAMPLE?",
"WHEN?",
"WHAT ARE YOU THINKING OF?",
"REALLY, ALWAYS?",
"DO YOU REALLY THINK SO?",
"BUT YOU ARE NOT SURE YOU*",
"DO YOU DOUBT YOU*",
"IN WHAT WAY?",
"WHAT RESEMBLANCE DO YOU SEE?",
"WHAT DOES THE SIMILARITY SUGGEST TO YOU?",
"WHAT OTHER CONNECTIONS DO YOU SEE?",
"COULD THERE REALLY BE SOME CONNECTION?",
"HOW?",
"YOU SEEM QUITE POSITIVE.",
"ARE YOU SURE?",
"I SEE.",
"I UNDERSTAND.",
"WHY DO YOU BRING UP THE TOPIC OF FRIENDS?",
"DO YOUR FRIENDS WORRY YOU?",
"DO YOUR FRIENDS PICK ON YOU?",
"ARE YOU SURE YOU HAVE ANY FRIENDS?",
"DO YOU IMPOSE ON YOUR FRIENDS?",
"PERHAPS YOUR LOVE FOR FRIENDS WORRIES YOU.",
"DO COMPUTERS WORRY YOU?",
"ARE YOU TALKING ABOUT ME IN PARTICULAR?",
"ARE YOU FRIGHTENED BY MACHINES?",
"WHY DO YOU MENTION COMPUTERS?",
"WHAT DO YOU THINK MACHINES HAVE TO DO WITH YOUR PROBLEM?",
"DON'T YOU THINK COMPUTERS CAN HELP PEOPLE?",
"WHAT IS IT ABOUT MACHINES THAT WORRIES YOU?",
"SAY, DO YOU HAVE ANY PSYCHOLOGICAL PROBLEMS?",
"WHAT DOES THAT SUGGEST TO YOU?",
"I SEE.",
"I'M NOT SURE I UNDERSTAND YOU FULLY.",
"COME COME ELUCIDATE YOUR THOUGHTS.",
"CAN YOU ELABORATE ON THAT?",
"THAT IS QUITE INTERESTING.",
};

/**
Line 2500. These are the mysterious numbers that keep
track of which replies have been used and so one.
Clever but hard to read.
*/
final static int rightreplies[]=
{
 1,3,4,2,6,4,6,4,10,4,14,3,17,3,20,2,22,3,25,3,
 28,4,28,4,32,3,35,5,40,9,40,9,40,9,40,9,40,9,40,9,
 49,2,51,4,55,4,59,4,63,1,63,1,64,5,69,5,74,2,76,4,
80,3,83,7,90,3,93,6,99,7,106,6};


/**
Utility function that removes a char from a String.
*/
public static String removeChar(String s,char c)
{
if (s==null) return s;
int p;
while((p=s.indexOf(c))>=0)
{
s=s.substring(0,p-1)+s.substring(p+1);
}
return s;
}

/**
Utility function that replaces all occurences of a specific string
with another string.
*/
public static String replaceString(String s,String oldstring,String newstring)
{
int pos;
while ((pos=s.indexOf(oldstring))>=0)
{
 s=s.substring(0,pos)+newstring+s.substring(pos+oldstring.length());
}
return s;
}
}
