/* 
   
   Filer -- Pattern Matching File Management Utility
   Copyright (c) 1976-2020 by Jeff Shrager

;;; +=========================================================================+
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

---------------------------------------------------------------

Filer is a filename pattern matcher and rebuilder (see -? notes,
below). The matchpattern is compared with the files in the current
directory (see -d option) and output to standard out, as described
below.  Under most circumstances, you'll want to quote the patterns
because filers uses special characters (* ' and ?), which will do the
wrong thing in a command line if you don't quote them.

***WARNING*** Filer is both very useful and very dangerous.  Before
you either pipe the produced commands to a shell for execution, be
absolutely sure that it's going do what you want, or you might end up
making a mess of your directory.  I take no responsibility for messes
made by the program!  (See disclaimer, above)

*/

#include <stdlib.h>
#include <iostream.h>
#include <string.h>
#include <dirent.h>
#include <time.h>

#define FNLEN 300 // max lenth of a filename or rebuilt name

void clrtbln(char table[FNLEN]);
int match(char fn[], char p[], int fp, int pp, int tp);
void rebuild();
void rbquote(int &l);
void rbdate(int &l);

// Tells us whether to ignore files that start with . (-a flag)

int include_dots = 0;

// The table holds the match results.  Each entry begins with a type
// char (either * or ?, or zero for empty), and then up to fifty chars
// of content which go with that item.

char table[10][FNLEN];

char nn[FNLEN]; // holds the resulting name from rebuilding
int nnp; // where in the new name are we?
char r[FNLEN]; // the rebuilding pattern (ought to be more localized)
char p[FNLEN]; // the match pattern
char *cmd = ""; // the command, if any.

// time structs

time_t* tp;
tm* tn;

main (int argc, char *argv[])
{
  // Time only needs to be computed once.

  tp = (time_t *) malloc(sizeof(time_t));
  tn = (tm *) malloc(sizeof(tm));
  time(tp);
  tn = localtime(tp);

  // option processing

  char* dirspec = ".";
  int errflg = 0;
  extern char *optarg;
  extern int optind, optopt;
  int c;
  while ((c = getopt(argc, argv, ":c:d:m:r:a?h")) != -1)
    switch (c) {
    case 'h':
      cout << "
usage: [-c command] [-m match-pattern] [-r rebuilding-pattern]
       (see other options, below)

Filer builds lists of files, or lists of shell commands with file agrs.
The files are pattern-matched from the current directory.

In the match, * matches an arbitrary string, ? matches an arbitrary char.
In rebuilding: * or ? may be followed by 'n (n=1-9) to indicate which
               wild card char to replace.
   (double quoted patterns are recommended because of the use of * ? ', etc.)
                'd[yYmdHM] inserts the indicated time:
                  'dY - insert the full year (1995)
                    y - short year (95)
                    m - month (01 thru 12)
                    d - day after first of the month (01 thru 31)
                    s - standard (yyyymmdd as: 19950922)
                   
                    H - hours (00 thru 23)
                    M - minutes (00 thru 59)
                    t - time (hhmm)

Example: filer -c cp -m \"*b*\" -r \"*'2b*'1\"
Will turn: 'abc' into 'cba', etc.

Actually, Filer just creates the cp commands to do that, you'll have to pipe
the commands to shell in order to get them to really happen.  You can do this
either by direct pipe to your favorite shell, or by collecting them (e.g., by >)
into a file and then running them in batch via source, submit, etc.

Options summary:

   -m <pattern>       matching pattern
   -r <pattern>       rebuilding pattern
   -c <command>       a command to prefix to resulting matches and rebuilds
   -d <path>          directory in which to do matches
                      (gets prepended to match patterns in built commands)
   -a                 include files that begin with a period (.)

Filer is Copyright (c) 1976-2006 by Jeff Shrager

;;; +=========================================================================+
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | Software), to deal in the Software without restriction, including       |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED >AS IS<, WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

";
      exit(2);
    case 'a':
      include_dots = 1;
      break;
    case 'c':
      cmd = optarg;
      break;
    case 'd':
      dirspec = optarg;
      break;
    case 'm':
      strcpy(&p[0], optarg);
      break;
    case 'r':
      strcpy(&r[0], optarg);
      break;
    case ':':	    // missing arg.
      cout << "Filer: Option -" << optopt << " requires an argument\n";
      exit(2);
    }
  
  // Okay, here's the good work.

  DIR *dirp;
  struct dirent *dp;

  // Get the filenames from the directory.

  dirp = opendir(dirspec);
  
  // Match for each file in the master list.  The matcher will put a \0 in
  // the first char of each file that fails to match.

  while ((dp = readdir(dirp)) != NULL) 
    {
      // Clear out the table.
      
      for (int k=0;k<10;k++)
	for (int l=0;l<FNLEN;l++)
	  table[k][l]=0;

      if(match(dp->d_name, p, 0, 0, 0))
	{
	 rebuild();
	 cout << cmd << " " << dirspec << "/" << dp->d_name << " " << nn << '\n';
       }
    }

  (void) closedir(dirp);
}

// The smart matcher uses * (any chars) and ? (any one char).
// Returns 1 for a good match, 0 for a bad one.  If the match is
// bad, the table is invalid.

int match(char fn[], char p[], int fp, int pp, int tp)
{
  if (fn[0] == '.' && include_dots == 0) return (0);
  while(1)
    {

      // If we're at the end of the filename, and at the end of the
      // pattern simulataneously, then we win!

      if (fn[fp] == 0 && fn[fp] == p[pp]) 
	return 1;
      else

	// If we're out of one or the other, fail!

	if (fn[fp] == 0 || p[pp] == 0)
	  return 0;
	else

	  // if they're the same letter, still okay, move on thru recursion.

	  if (fn[fp] == p[pp])
	    return match(fn, p, fp + 1, pp + 1, tp);
	  else

	    // See if the one is a ? -- still okay, but save it!

	    if(p[pp] == '?')
	      {
		clrtbln(table[tp]); // Clear the line of the table
		table[tp][0] = '?';
		table[tp][1] = fn[fp];
		return match(fn, p, fp + 1, pp + 1, tp + 1);
	      }

	    else

	      // See if the one is a * -- still okay, also save it and recurse
	      // The way that this is written, * must match at least
	      // one char.  I'm not sure that I like it that way.

	      if(p[pp] == '*')
		{
		  int sp = 1; // set up a local star pointer

		  clrtbln(table[tp]); // Clear the line of the table
		  table[tp][0] = '*'; // Insert marker for *

		  table[tp][sp] = fn[fp];
		  if (match(fn, p, fp + 1, pp + 1, tp + 1))
		    return 1;
		  else

		    // If we got here then we want to expand the * entry by
		    // one, and if we hit the end, then fail this *.

		    while(1)
		      {
			table[tp][++sp] = fn[++fp];
			if (fn[fp] == 0)
			  {
			    if (p[pp + 1] == 0)
			      return 1;
			    else
			      return 0;
			  }
			else
			  if (match(fn, p, fp + 1, pp + 1, tp + 1))
			    return 1;
		      }
		}
	      else
		return 0;
    }
}

void clrtbln(char table[FNLEN])
{
  for (int k = 0; k < FNLEN; k++) table[k] = 0;
}

// Here's the rebuilding code.  Uses the table as a global.  The spec
// is that the rebuilding pattern can contain chars or "*" or "?", and
// that the pattern chars (*?) can be followed by an apostrophe (') and
// a single digit, indicating the nth * ot ? should be used.  If no
// 'n is indicated, the NEXT *? is used (from 0 or the last 'n).

void rbstar(int &l);
void rbqmark(int &l);

int stk = 0; // last *
int qmk = 0; // last ?

void rebuild()
{
  stk = 0;
  qmk = 0;

  nnp=0; // this ought to be local, and global only to our fns.  Next lifetime.

  for (int k = 0; k < FNLEN; k++) nn[k]=0;

  for (int l = 0; r[l] != 0;)
    {
      if (r[l] == '*')
	rbstar(l);
      else
	if (r[l] == '?')
	  rbqmark(l);
	else
	  if (r[l] == '\'')
	    rbquote(l);
	  else
	    nn[nnp++] = r[l++];
    }
}

void rbcpy(int where);
int getdigit(int &l);

void rbstar(int &l)
{
  // find out if there's a 'n, and if not, figure out where we are.

  l++;

  int dg = getdigit(l);
  
  if (dg == 99) cout << "Filer: Pattern index must be 1-9!\n"; // user gave a bad character

  if (dg == 0) dg = ++stk;
  else stk = dg;

  int where;

  for (where = 0; dg != 0; where++)
    {
      if (table[where][0] == '*')
	dg--;
      if (where > 9) dg = 0;
    }

  if (table[where - 1][0] == 0) cout << "Can't find indexed pattern item.\n";

  // pipe it into the output
  
  rbcpy(where - 1);
}

void rbquote(int &l)
{
  switch(r[++l])
    {
    case 'd': 
      rbdate(l);
      break;
    default:
      cout << "Filer: Invalid '_ quote spec!\n";
      exit(2);
    }
}

void rbdate(int &l)
{
  switch(r[++l])
    {
    case 'y':
      strftime(nn+nnp, 3, "%y", tn);
      nnp = nnp+2; l++;
      break;
    case 'Y': 
      strftime(nn+nnp, 5, "%Y", tn);
      nnp = nnp+4; l++;
      break;
    case 'm': 
      strftime(nn+nnp, 3, "%m", tn);
      nnp = nnp+2; l++;
      break;
    case 'd': 
      strftime(nn+nnp, 3, "%d", tn);
      nnp = nnp+2; l++;
      break;
    case 'M': 
      strftime(nn+nnp, 3, "%M", tn);
      nnp = nnp+2; l++;
      break;
    case 'H': 
      strftime(nn+nnp, 3, "%H", tn);
      nnp = nnp+2; l++;
      break;
    case 's': 
      strftime(nn+nnp, 9, "%Y%m%d", tn);
      nnp = nnp+8; l++;
      break;
    case 't': 
      strftime(nn+nnp, 5, "%H%M", tn);
      nnp = nnp+4; l++;
      break;
    default:
      cout << "Filer: Invalid 'd_ date spec!\n";
      exit(2);
    }
}
  

void rbqmark(int &l)
{
  // find out if there's a 'n, and if not, figure out where we are.

  l++;

  int dg = getdigit(l);
  
  if (dg == 99) cout << "Filer: Pattern index must be 1-9!\n"; // user gave a bad character

  if (dg == 0) dg = ++qmk;
  else qmk = dg;

  int where;

  for (where = 0; dg != 0; where++)
    {
      if (table[where][0] == '?')
	dg--;
      if (where > 9) dg = 0;
    }

  if (table[where - 1][0] == 0) cout << "Can't find indexed pattern item.\n";

  // pipe it into the output
  
  rbcpy(where - 1);
}

// Copy the table entry into the end of the rebuilt name.

void rbcpy(int where)
{
  int tk = 1; // where in the table entry we are
  while (table[where][tk] != 0)
    {
      nn[nnp++] = table[where][tk++];
    }
}

// Getdigit sees if there's an 'n after a special mark.  If not, it
// returns 0, else it returns the digit and updates the incoming
// rbpat place pointed two ahead.

int getdigit(int &l)
{
  int d;

  if (r[l] != '\'')
    return 0;
  else
    {
      l++;
      d = r[l++] - 48; // get a digit from its char rep, I think....??
      if (d >= 1 && d <= 10)
	return d;
      else
	return 99;
    }
}


/*

Things that need to be done:

-g, -e, -v options: -e means "eval", that is, do the commands and
don't bother piping them to cout (unless -v is given).  If -e is given
without -g ("go"), then the user is prompted (yes, no, go) for each
command.  -v ("verify") is given with -e, then the commands are both
executed *and* sent to cout.

-i read from cin instead of getting the current dir's files.

Mod: I don't think that * requiring at least one char is right.  Maybe
replace that with + and make * be zero-min.

Mod: Think about replaces -c -m and -r with just one, two, or three
args (second and third optional).

Mike Worden wants arbitrary math.  This is a good idea.  Maybe add a
"'#" match char which, on the other end you can do simple things to,
like '#+  or  '#- to add one or subtract one (if I really wanted to
get fancy I could permit things like '#(+5/(#-4))...  Next lifetime.

*/

