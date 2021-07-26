#!/usr/local/bin/perl
#
#	Copyright  2014  James A. Markevitch
#		ALL RIGHTS RESERVED
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
#
#
# This is a Perl hack to implement the 1966 version of BBN-LISP for the PDP-1
# computer.  This was written primarly to run the 1966 LISP version of the
# "doctor" program (aka Eliza) written by Bernie Cosell.
#
# The intent is to be compatible with the version of LISP described in
# The BBN-LISP System, Daniel G. Bobrow et al, February, 1966, AFCRL-66-180
# [BBN66].  However, because many of the quirks of that version of LISP
# are not documented, The BBN-LISP System Reference Manual April 1969, D. G.
# Bobrow et al [BBN69] was used as a reference.  Finally, LISP 1.5 Programmer's
# Manual, John McCarthy et al [LISP1.5] was also used as a reference.
#
# N.B. The 1966 version of BBN-LISP has differences from later versions and
# this interpreter will not properly execute programs written for those later
# versions.
#
# This program uses the Perl stack, rather than relying on the pushdown list
# for recursion; it uses the Perl eval/die/$@ mechanism for unwinding the
# stack for "go" and "return" as well as for errors; the interactive REPL only
# operates at the top level, meaning that any errors unwind the stack.
#
# The eval engine follows the description and diagram in Appendix 2 of [BBN69].
#
# Todo
#	Add an option to be compatible with the 1969 version
#	Improve funcstack
#
# Behavior of BBN-LISP that may differ from more familiar LISP dialects
#	car of a symbol is the value
#	cdr of a symbol is its property list
#	car of a number is the number [BBN69]
#	No character or string type, so even " " is a symbol [BBN69]
#
# Unimplemented functions
#	(apply fn args a) Apply function using symbol list
#	(character n)	Prints character with code n
#	(disp x y)	Display a point on the CRT
#	(displis l)	Display multiple points on the CRT
#	(evala x a)	Evaluate using symbol list
#	(field n)	Call field n from the drum
#
# Possible incompatibilities with true BBN-LISP
#	(clearbuf x) may have subtle differences in effect
#	(gcgag x) has no effect
#	(go x) does not need to be at the top level in a prog
#	(oblist) may include extra symbols
#	(readin x) has no effect
#	(remob x) only removes the object from the symbol table
#	(reclaim) has no effect and returns a fixed value
#	(return x) does not need to be at the top level in a prog
#	Arity is always checked for built-in functions
#	Q does not specify octal numeric constants
#	APVALs are just normal symbols and can be overwritten
#	Left arrow (xarr) is represented as "<-" instead of a single character
#	Arity is checked on function calls ([BBN69] fills with nil or discards)
#	Error checking may be more strict or more lenient in some cases
#	faulteval is not invoked on errors
#
# Implementation characeristics undocumented in [BBN66] that may be incompatible
#	(eq x y) compares number values, not identical value cells
#	(feed x) returns nil
#	(gcgag) returns previous setting
#	(list x) requires at least one argument
#	(member x y) uses equal for comparison [LISP1.5]
#	(nconc x y) returns y if x is nil [LISP1.5]
#	(prog x) requires at least two arguments
#	(prog x) initializes variable values, property lists and funcdefs to nil
#	(prog x) restores values, property lists and funcdefs
#	(progn x) returns nil if there are no arguments
#	(quote x) requires at least one argument
#	(remob x) returns nil
#	(setn x y) returns the new value of x
#	(setq x y) sets the local value, not the top-level value
#	lambda, nlamda, and prog variables are not captured; use gensym instead
#	The pushdown list also includes function definitions and property lists
#
# Additions that were not described in [BBN66]
#	Semi-colon introduces a comment to the end of the line
#	(clock) returns the wall clock time in seconds
#	(control x) has no effect
#	(loadfile x) added for convenience
#
# Hacks to make the "doctor" program work (when -doctor66 is specified)
#	(set word (ratom)) is executed as (setq word (ratom))
#	(greaterp nil nil) returns nil
#	(greaterp num nil) returns nil
#	(ratom) returns "L14" for new-line

$utilname = "bbnlisp.pl";
$utilvers = "1.0 12/31/2014";

#
# Objects are represented as array references.  The first value in the array
# specifies the object type and the others are shown in the following table:
#
# [0]		[1]	[2]	[3]	[4]
#
# O_SYMBOL	value	proplist name	funcdef		Symbol
# O_PAIR	car	cdr				Dotted pair (cons)
# O_NUM		value	proplist			Number
# O_T		?	?				T
# O_NIL		?	?				NIL
# O_SUBR	funcptr	?				Built-in function
# O_FSUBR	funcptr	?				Special form

$O_ATOM   = 0x10000;

$O_SYMBOL = 0x00001 | $O_ATOM;
$O_PAIR   = 0x00002;
$O_NUM    = 0x00004 | $O_ATOM;
$O_T      = 0x00008 | $O_ATOM;
$O_NIL    = 0x00010 | $O_ATOM;
$O_SUBR   = 0x00020 | $O_ATOM;
$O_FSUBR  = 0x00040 | $O_ATOM;

#
# Built-in SUBR and FSUBR to be installed in the symbol table at initialization
# time.
#
#	symbol		function		type		arity
#
%builtins = (
	"and",		[ \&fsubr_and,		$O_FSUBR,	-1, ],
	"atom",		[ \&subr_atom,		$O_SUBR,	1, ],
	"car",		[ \&subr_car,		$O_SUBR,	1, ],
	"cdr",		[ \&subr_cdr,		$O_SUBR,	1, ],

	"caar",		[ \&subr_caar,		$O_SUBR,	1, ],
	"cadr",		[ \&subr_cadr,		$O_SUBR,	1, ],
	"cdar",		[ \&subr_cdar,		$O_SUBR,	1, ],
	"cddr",		[ \&subr_cddr,		$O_SUBR,	1, ],
	"caaar",	[ \&subr_caaar,		$O_SUBR,	1, ],
	"caadr",	[ \&subr_caadr,		$O_SUBR,	1, ],
	"cadar",	[ \&subr_cadar,		$O_SUBR,	1, ],
	"caddr",	[ \&subr_caddr,		$O_SUBR,	1, ],
	"cdaar",	[ \&subr_cdaar,		$O_SUBR,	1, ],
	"cdadr",	[ \&subr_cdadr,		$O_SUBR,	1, ],
	"cddar",	[ \&subr_cddar,		$O_SUBR,	1, ],
	"cdddr",	[ \&subr_cdddr,		$O_SUBR,	1, ],
	"caaaar",	[ \&subr_caaaar,	$O_SUBR,	1, ],
	"caaadr",	[ \&subr_caaadr,	$O_SUBR,	1, ],
	"caadar",	[ \&subr_caadar,	$O_SUBR,	1, ],
	"caaddr",	[ \&subr_caaddr,	$O_SUBR,	1, ],
	"cadaar",	[ \&subr_cadaar,	$O_SUBR,	1, ],
	"cadadr",	[ \&subr_cadadr,	$O_SUBR,	1, ],
	"caddar",	[ \&subr_caddar,	$O_SUBR,	1, ],
	"cadddr",	[ \&subr_cadddr,	$O_SUBR,	1, ],
	"cdaaar",	[ \&subr_cdaaar,	$O_SUBR,	1, ],
	"cdaadr",	[ \&subr_cdaadr,	$O_SUBR,	1, ],
	"cdadar",	[ \&subr_cdadar,	$O_SUBR,	1, ],
	"cdaddr",	[ \&subr_cdaddr,	$O_SUBR,	1, ],
	"cddaar",	[ \&subr_cddaar,	$O_SUBR,	1, ],
	"cddadr",	[ \&subr_cddadr,	$O_SUBR,	1, ],
	"cdddar",	[ \&subr_cdddar,	$O_SUBR,	1, ],
	"cddddr",	[ \&subr_cddddr,	$O_SUBR,	1, ],

	"clearbuf",	[ \&subr_clearbuf,	$O_SUBR,	0, ],
	"clock",	[ \&subr_clock,		$O_SUBR,	0, ],
	"cond",		[ \&fsubr_cond,		$O_FSUBR,	-1, ],
	"cons",		[ \&subr_cons,		$O_SUBR,	2, ],
	"control",	[ \&subr_control,	$O_SUBR,	1, ],
	"divide",	[ \&subr_divide,	$O_SUBR,	2, ],
	"eq",		[ \&subr_eq,		$O_SUBR,	2, ],
	"equal",	[ \&subr_equal,		$O_SUBR,	2, ],
	"error",	[ \&subr_error,		$O_SUBR,	1, ],
	"errorset",	[ \&subr_errorset,	$O_SUBR,	2, ],
	"eval",		[ \&subr_eval,		$O_SUBR,	1, ],
	"feed",		[ \&subr_feed,		$O_SUBR,	1, ],
	"fntyp",	[ \&subr_fntyp,		$O_SUBR,	1, ],
	"gcgag",	[ \&subr_gcgag,		$O_SUBR,	1, ],
	"gensym",	[ \&subr_gensym,	$O_SUBR,	0, ],
	"getd",		[ \&subr_getd,		$O_SUBR,	1, ],
	"go",		[ \&fsubr_go,		$O_FSUBR,	1, ],
	"greaterp",	[ \&subr_greaterp,	$O_SUBR,	2, ],
	"lambda",	[ \&fsubr_lambda,	$O_FSUBR,	2, ],
	"list",		[ \&fsubr_list,		$O_FSUBR,	-1, ],
	"loadfile",	[ \&fsubr_loadfile,	$O_FSUBR,	1, ],
	"logand",	[ \&fsubr_logand,	$O_FSUBR,	-1, ],
	"logor",	[ \&fsubr_logor,	$O_FSUBR,	-1, ],
	"member",	[ \&subr_member,	$O_SUBR,	2, ],
	"minus",	[ \&subr_minus,		$O_SUBR,	1, ],
	"nconc",	[ \&subr_nconc,		$O_SUBR,	2, ],
	"nlamda",	[ \&fsubr_nlamda,	$O_FSUBR,	2, ],
	"nnconc",	[ \&subr_nconc,		$O_SUBR,	2, ],	# nconc
	"null",		[ \&subr_null,		$O_SUBR,	1, ],
	"numberp",	[ \&subr_numberp,	$O_SUBR,	1, ],
	"oblist",	[ \&subr_oblist,	$O_SUBR,	0, ],
	"or",		[ \&fsubr_or,		$O_FSUBR,	-1, ],
	"pack",		[ \&subr_pack,		$O_SUBR,	1, ],
	"plus",		[ \&fsubr_plus,		$O_FSUBR,	-1, ],
	"prin1",	[ \&subr_prin1,		$O_SUBR,	1, ],
	"print",	[ \&subr_print,		$O_SUBR,	1, ],
	"punchon",	[ \&subr_punchon,	$O_SUBR,	1, ],
	"prog",		[ \&fsubr_prog,		$O_FSUBR,	-1, ],
	"prog1",	[ \&subr_prog1,		$O_SUBR,	2, ],
	"prog2",	[ \&subr_prog2,		$O_SUBR,	2, ],
	"progn",	[ \&fsubr_progn,	$O_FSUBR,	-1, ],
	"putd",		[ \&subr_putd,		$O_SUBR,	2, ],
	"quit",		[ \&subr_quit,		$O_SUBR,	0, ],
	"quote",	[ \&fsubr_quote,	$O_FSUBR,	-1, ],
	"quotient",	[ \&subr_quotient,	$O_SUBR,	2, ],
	"ratom",	[ \&subr_ratom,		$O_SUBR,	0, ],
	"read",		[ \&subr_read,		$O_SUBR,	0, ],
	"readin",	[ \&subr_readin,	$O_SUBR,	1, ],
	"reclaim",	[ \&subr_reclaim,	$O_SUBR,	0, ],
	"remob",	[ \&subr_remob,		$O_SUBR,	1, ],
	"return",	[ \&subr_return,	$O_SUBR,	1, ],
	"rplaca",	[ \&subr_rplaca,	$O_SUBR,	2, ],
	"rplacd",	[ \&subr_rplacd,	$O_SUBR,	2, ],
	"select",	[ \&fsubr_select,	$O_FSUBR,	-1, ],
	"selectq",	[ \&fsubr_selectq,	$O_FSUBR,	-1, ],
	"set",		[ \&subr_set,		$O_SUBR,	2, ],
	"setbrk",	[ \&fsubr_setbrk,	$O_FSUBR,	-1, ],
	"setn",		[ \&subr_setn,		$O_SUBR,	2, ],
	"setq",		[ \&fsubr_setq,		$O_FSUBR,	2, ],
	"setsepr",	[ \&fsubr_setsepr,	$O_FSUBR,	-1, ],
	"terpri",	[ \&subr_terpri,	$O_SUBR,	0, ],
	"times",	[ \&fsubr_times,	$O_FSUBR,	-1, ],
	"typein",	[ \&subr_typein,	$O_SUBR,	1, ],
	"typeout",	[ \&subr_typeout,	$O_SUBR,	1, ],
	"unpack",	[ \&subr_unpack,	$O_SUBR,	1, ],
);

%apvals = (
	"blank",	" ",
	"space",	" ",
	"tab",		"\t",
	"comma",	",",
	"eqsign",	"=",
	"xeqs",		"=",
	"period",	".",
	"plus",		"+",
	"lpar",		"(",
	"rpar",		")",
	"slash",	"/",
	"qmark",	"?",
	"xdol",		"\$",
	"xsqu",		"'",
	"xdqu",		"\"",
	"xlbr",		"[",
	"xrbr",		"]",
	"xarr",		"<-",	# should be left arrow
	"uparr",	"^",
	"colon",	":",
	"xgreater",	">",
	"xlesser",	"<",
	"xnum",		"#",
	"xper",		"%",
	"xamp",		"&",
	"xat",		"\@",
);

while (@ARGV > 0 && $ARGV[0] =~ /^\-/) {
	$arg = shift(@ARGV);
	$f_doctor66 = 1, next				if ($arg eq "-doctor66");
	&prusage(0)					if ($arg eq "-help");
	&prversion()					if ($arg eq "-version");
	&prusage(1) if (@ARGV < 1);
	push(@g_loadfiles, shift(@ARGV)), next		if ($arg eq "-load");
	push(@g_punchinfiles, shift(@ARGV)), next	if ($arg eq "-punchin");
	$g_punchoutfile = shift(@ARGV), next		if ($arg eq "-punchout");
	&prusage(1);
}

if (defined($g_punchoutfile)) {
	open(PUNCHFILE, ">$g_punchoutfile") || die "Error: cannot create $g_punchoutfile";
}

&init_syms();
&repl();

sub prusage {
	my($code) = @_;

	print "Usage: $utilname  [ options ... ]\n";
	print "    -doctor66        Hack behavior to make 1966 version of doctor run\n";
	print "    -help            Display this message\n";
	print "    -load file       Load file before entering repl\n";
	print "    -punchin file    Punch input file\n";
	print "    -punchout file   Send punch output to file\n";
	print "    -version         Print version number\n";

	exit($code);
}

sub prversion {
	print "$utilname version $utilvers\n";
	exit(0);
}

sub init_syms {
	my($key, $obj);

	$g_nobind_obj = undef;
	$g_nil_obj = &new_nil();
	$g_t_obj = &new_t();
	&obj_set_value(&lookup_symbol("nil"), $g_nil_obj);
	&obj_set_value(&lookup_symbol("t"), $g_t_obj);

	# APVALs
	&obj_set_value(&lookup_symbol("f", $g_nil_obj));
	&obj_set_value(&lookup_symbol("*t*", $g_t_obj));
	foreach $key (keys %apvals) {
		&obj_set_value(&lookup_symbol($key), &new_symbol($apvals{$key}, $g_nobind_obj));
	}

	foreach $key (keys %builtins) {
		if ($builtins{$key}->[1] == $O_SUBR) {
			$obj = &new_subr($key, @{$builtins{$key}});
		} else {
			$obj = &new_fsubr($key, @{$builtins{$key}});
		}
		&obj_set_funcdef(&lookup_symbol($key), $obj);
	}

	$g_lambda_sym = &lookup_symbol("lambda");
	$g_nlamda_sym = &lookup_symbol("nlamda");

	$g_punchon_obj = $g_nil_obj;
	$g_typeout_obj = $g_t_obj;
	$g_typein_obj = $g_t_obj;
	$g_readin_obj = $g_nil_obj;
	$g_gcgag_obj = $g_nil_obj;
	$g_errorset_obj = $g_t_obj;

	# Chosen to be a large fraction of the real value on the PDP-1 (25000)
	$g_reclaim_value = 20000;

	$g_seprchars{" "} = 1;
	$g_seprchars{"\t"} = 1;
	$g_seprchars{"\n"} = 1;
	$g_brkchars{"("} = 1;
	$g_brkchars{")"} = 1;

	# The following hack is to make the 1966 version of doctor run
	$g_set_sym = &lookup_symbol("set");
	$g_setq_sym = &lookup_symbol("setq");
	# (set word (ratom))
	$g_doctorhack_obj = &new_pair($g_set_sym,
			    &new_pair(&lookup_symbol("word"),
			    &new_pair(&new_pair(&lookup_symbol("ratom"),
				      $g_nil_obj),
			    $g_nil_obj)));
}

sub repl {
	for (;;) {
		print "> ";
		eval { &print_obj(STDOUT, &eval_obj(&read_obj())); };
		if ($@) {
			print "throw was $@\n";
		}
		print "\n";
	}
}

sub read_obj {
	my($token, $listobj);
	my(@objs);

	$token = &read_token();
	if ($token eq "(") {
		for (;;) {
			$token = &read_token();
			if ($token eq ")") {
				$listobj = $g_nil_obj;
				while (@objs) {
					$listobj = &new_pair(pop(@objs), $listobj);
				}
				return $listobj;
			} elsif ($token eq ".") {
				$listobj = &read_obj();
				$token = &read_token();
				if ($token ne ")") {
					print "Error: missing close paren after dotted pair\n";
				}
				while (@objs) {
					$listobj = &new_pair(pop(@objs), $listobj);
				}
				return $listobj;
			}
			&untoken($token);
			push(@objs, &read_obj());
		}
	} elsif ($token =~ /^\-?\d+$/) {
		return &new_num($token);
	} elsif ($token eq "t") {
		return $g_t_obj;
	} elsif ($token eq "nil") {
		return $g_nil_obj;
	} else {
		$token =~ s/\"(.[^\"]*)\"/$1/sg;
		return &lookup_symbol($token);
	}
}

sub read_token {
	my($token);

	return pop(@g_tokens) if (@g_tokens > 0);

	for (;;) {
		# Strip white space
		$g_inbuf =~ s/^[\s\n]+//s;

		# Semi-colon to end of line is a comment
		if ($g_inbuf =~ /^\;[^\n]*\n(.*)$/s) {
			$g_inbuf = $1;
			next;
		}

		# ( ) are tokens
		if ($g_inbuf =~ /^([\(\)])[\s\n]*(.*)$/s) {
			($token, $g_inbuf) = ($1, $2);
			return $token;
		}

		# Any other sequence of characters, including quoted
		# sub-strings is a token.  Any quotes remain in the token
		# and must be dealt with by the caller.
		#
		if ($g_inbuf =~ /^(([^\"\(\)\s\n]+|\".[^\"]*\")+)[\s\n]*(.*)$/s) {
			($token, $g_inbuf) = ($1, $3);
			return $token;
		}

		for (;;) {
			&fill_inbuf();
			# Keep reading if there is an unterminated quote
			last unless ($g_inbuf =~ /^([^\"]|\".[^\"]*\")*\"[^\"]*$/s);
		}
	}
}

sub untoken {
	my($token) = @_;

	push(@g_tokens, $token);
}

sub fill_inbuf {
	if ($g_typein_obj == $g_nil_obj) {
		while (defined($g_punchinfile) || @g_punchinfiles > 0) {
			if (!defined($g_punchinfile)) {
				$g_punchinfile = shift(@g_punchinfiles);
				open(PUNCHINFILE, $g_punchinfile) || die "Error: cannot open $g_punchinfile\n";
			}
			while (<PUNCHINFILE>) {
				$g_inbuf .= $_;
				return;
			}
			close(LOADFILE);
			$g_punchinfile = undef;
		}
		$g_typein_obj = $g_t_obj;
		&error("ran off end of punchin file; typein set to t");
	}

	while (defined($g_loadfile) || @g_loadfiles > 0) {
		if (!defined($g_loadfile)) {
			$g_loadfile = shift(@g_loadfiles);
			open(LOADFILE, $g_loadfile) || die "Error: cannot open $g_loadfile\n";
		}
		while (<LOADFILE>) {
			$g_inbuf .= $_;
			return;
		}
		close(LOADFILE);
		$g_loadfile = undef;
	}
	while (<>) {
		$g_inbuf .= $_;
		return;
	}
	exit 0;
}

#
# Implementation of object evaluation based on Appendix 2, The BBN LISP
# Interpreter, on pages 23.7 and 23.8 of [BBN69].
#
sub eval_obj {
	my($obj) = @_;
	my($cobj, $dobj, $robj, $vars, $varp, $nvars, $entry);
	my(@args);

	if (&obj_is_symbol($obj)) {
		if (!defined(&obj_value($obj))) {
			&error("unbound symbol", $obj);
		}
		return &obj_value($obj);
	} elsif (&obj_is_pair($obj)) {
		$cobj = &obj_car($obj);
		if ($f_doctor66 &&
		    $cobj == $g_set_sym &&
		    &subr_equal($obj, $g_doctorhack_obj) == $g_t_obj) {
			$cobj = $g_setq_sym;
		}
		for (;;) {
			if (&obj_is_symbol($cobj)) {
				$dobj = &obj_funcdef($cobj);
				if (&obj_is_subr($dobj)) {
					@args = &evalargs(&obj_cdr($obj));
					if ($dobj->[1]->[3] >= 0 &&
					    $dobj->[1]->[3] != @args) {
						&error("incorrect number of args", $dobj->[1]);
					}
					return &{$dobj->[1]->[1]}(@args);
				} elsif (&obj_is_fsubr($dobj)) {
					@args = &unevalargs(&obj_cdr($obj));
					if ($dobj->[1]->[3] >= 0 &&
					    $dobj->[1]->[3] != @args) {
						&error("incorrect number of args", $dobj->[1]);
					}
					return &{$dobj->[1]->[1]}(@args);
				} elsif ($dobj == $g_nil_obj) {
					$dobj = &obj_value($cobj);
					if ($dobj == $g_nil_obj ||
					    $dobj == $g_t_obj ||
					    $dobj == $g_nobind_obj) {
						&error("unevaluatable binding", $cobj);
					}
				}
				$cobj = $dobj;
				next;
			} elsif (!&obj_is_pair($cobj)) {
				&error("invalid function", $cobj);
			}
			if (&obj_car($cobj) != $g_lambda_sym &&
			    &obj_car($cobj) != $g_nlamda_sym) {
				$cobj = &eval_obj($cobj);
				next;
			}
			if (&obj_car($cobj) == $g_nlamda_sym) {
				push(@args, &obj_cdr($obj));
			} else {
				@args = &evalargs(&obj_cdr($obj));
			}
			$vars = &subr_cadr($cobj);
			$nvars = 0;
			while (@args) {
				# [BBN69] last if (&obj_is_nil($vars));
				&error("non-pair in lambda vars", $vars) if (!&obj_is_pair($vars));
				$varp = &obj_car($vars);
				&error("non-symbol in lambda vars", $varp) if (!&obj_is_symbol($varp));
				push(@g_pushdown, [ $varp, &obj_value($varp), &obj_proplist($varp), &obj_funcdef($varp) ]);
				&obj_set_value($varp, shift(@args));
				&obj_set_proplist($varp, $g_nil_obj);
				&obj_set_funcdef($varp, $g_nil_obj);
				$vars = &obj_cdr($vars);
				++$nvars;
			}
			# [BBN69] set remaining variables to nil here
			&error("insufficient arguments to function", $obj) if (!&obj_is_nil($vars));
			#unshift(@g_funcstack, $cobj);
			unshift(@g_funcstack, $obj);
			eval { $robj = &eval_obj(&subr_caddr($cobj)); };
			shift(@g_funcstack);
			while ($nvars > 0) {
				$entry = pop(@g_pushdown);
				$varp = $entry->[0];
				&obj_set_value($varp, $entry->[1]);
				&obj_set_proplist($varp, $entry->[2]);
				&obj_set_funcdef($varp, $entry->[3]);
				--$nvars;
			}
			if ($@) {
				die $@;
			}
			return $robj;
		}
	} elsif (&obj_is_num($obj) ||
		 &obj_is_t($obj) ||
		 &obj_is_nil($obj)) {
		return $obj;
	} else {
		&error("cannot evaluate", $obj);
	}
}

sub evalargs {
	my($pair) = @_;
	my(@args);

	while (&obj_is_pair($pair)) {
		push(@args, &eval_obj(&obj_car($pair)));
		$pair = &obj_cdr($pair);
	}
	return @args;
}

sub unevalargs {
	my($pair) = @_;
	my(@args);

	while (&obj_is_pair($pair)) {
		push(@args, &obj_car($pair));
		$pair = &obj_cdr($pair);
	}
	return @args;
}

sub fsubr_and {
	my(@args) = @_;
	my($arg);

	foreach $arg (@args) {
		return $g_nil_obj if (&obj_is_nil(&eval_obj($arg)));
	}
	return $g_t_obj;
}

sub subr_atom {
	my($x) = @_;

	return $g_t_obj if (&obj_is_atom($x));
	return $g_nil_obj;
}

sub subr_car {
	my($x) = @_;

	return &obj_value($x) if (&obj_is_symbol($x));
	return $x if (&obj_is_num($x));
	return $g_nil_obj if (&obj_is_nil($x));
	return &obj_car($x) if (&obj_is_pair($x));

	&error("car on illegal object", $x);
}

sub subr_cdr {
	my($x) = @_;

	return &obj_proplist($x) if (&obj_is_symbol($x));
	return $g_nil_obj if (&obj_is_nil($x));
	return &obj_cdr($x) if (&obj_is_pair($x));

	&error("cdr on illegal object", $x);
}

sub subr_caar { &subr_car(&subr_car($_[0])); }
sub subr_cadr { &subr_car(&subr_cdr($_[0])); }
sub subr_cdar { &subr_cdr(&subr_car($_[0])); }
sub subr_cddr { &subr_cdr(&subr_cdr($_[0])); }

sub subr_caaar { &subr_car(&subr_car(&subr_car($_[0]))); }
sub subr_caadr { &subr_car(&subr_car(&subr_cdr($_[0]))); }
sub subr_cadar { &subr_car(&subr_cdr(&subr_car($_[0]))); }
sub subr_caddr { &subr_car(&subr_cdr(&subr_cdr($_[0]))); }
sub subr_cdaar { &subr_cdr(&subr_car(&subr_car($_[0]))); }
sub subr_cdadr { &subr_cdr(&subr_car(&subr_cdr($_[0]))); }
sub subr_cddar { &subr_cdr(&subr_cdr(&subr_car($_[0]))); }
sub subr_cdddr { &subr_cdr(&subr_cdr(&subr_cdr($_[0]))); }

sub subr_caaaar { &subr_car(&subr_car(&subr_car(&subr_car($_[0])))); }
sub subr_caaadr { &subr_car(&subr_car(&subr_car(&subr_cdr($_[0])))); }
sub subr_caadar { &subr_car(&subr_car(&subr_cdr(&subr_car($_[0])))); }
sub subr_caaddr { &subr_car(&subr_car(&subr_cdr(&subr_cdr($_[0])))); }
sub subr_cadaar { &subr_car(&subr_cdr(&subr_car(&subr_car($_[0])))); }
sub subr_cadadr { &subr_car(&subr_cdr(&subr_car(&subr_cdr($_[0])))); }
sub subr_caddar { &subr_car(&subr_cdr(&subr_cdr(&subr_car($_[0])))); }
sub subr_cadddr { &subr_car(&subr_cdr(&subr_cdr(&subr_cdr($_[0])))); }
sub subr_cdaaar { &subr_cdr(&subr_car(&subr_car(&subr_car($_[0])))); }
sub subr_cdaadr { &subr_cdr(&subr_car(&subr_car(&subr_cdr($_[0])))); }
sub subr_cdadar { &subr_cdr(&subr_car(&subr_cdr(&subr_car($_[0])))); }
sub subr_cdaddr { &subr_cdr(&subr_car(&subr_cdr(&subr_cdr($_[0])))); }
sub subr_cddaar { &subr_cdr(&subr_cdr(&subr_car(&subr_car($_[0])))); }
sub subr_cddadr { &subr_cdr(&subr_cdr(&subr_car(&subr_cdr($_[0])))); }
sub subr_cdddar { &subr_cdr(&subr_cdr(&subr_cdr(&subr_car($_[0])))); }
sub subr_cddddr { &subr_cdr(&subr_cdr(&subr_cdr(&subr_cdr($_[0])))); }

sub subr_clearbuf {
	$g_inbuf = "";
	@g_tokens = ();
}

sub subr_clock {
	return &new_num(time);
}

sub fsubr_cond {
	my(@args) = @_;
	my($arg, $result, $expr);

	foreach $arg (@args) {
		&error("cond element not pair", $arg) if (!&obj_is_pair($arg));
		$result = &eval_obj(&obj_car($arg));
		next if (&obj_is_nil($result));
		$expr = &obj_cdr($arg);
		for (;;) {
			last if (&obj_is_nil($expr));
			&error("cond element not list", $expr) if (!&obj_is_pair($expr));
			$result = &eval_obj(&obj_car($expr));
			$expr = &obj_cdr($expr);
		}
		return $result;
	}
	return $g_nil_obj;
}

sub subr_cons {
	my($x, $y) = @_;

	return &new_pair($x, $y);
}

sub subr_control {
	my($x) = @_;

	return $g_nil_obj;
}

sub subr_divide {
	my($x, $y) = @_;
	my($q, $r);

	&error("non-numeric argument of divide", $x) if (!&obj_is_num($x));
	&error("non-numeric argument of divide", $y) if (!&obj_is_num($y));
	&error("divide by zero") if (&obj_value($y) == 0);
	$q = int(&obj_value($x) / &obj_value($y));
	$r = &obj_value($x) - $q * &obj_value($y);
	return &new_pair(&new_num($q), &new_num($r));
}

sub subr_eq {
	my($x, $y) = @_;

	return $g_t_obj if (&obj_is_num($x) &&
			    &obj_is_num($y) &&
			    &obj_value($x) == &obj_value($y));
	return $g_t_obj if ($x == $y);
	return $g_nil_obj;
}

sub subr_equal {
	my($x, $y) = @_;

	return $g_t_obj if (&obj_is_t(&subr_eq($x, $y)));
	return $g_t_obj if (&obj_is_pair($x) &&
			    &obj_is_pair($y) &&
			    &obj_is_t(&subr_equal(&obj_car($x), &obj_car($y))) &&
			    &obj_is_t(&subr_equal(&obj_cdr($x), &obj_cdr($y))));
	return $g_nil_obj;
}

sub subr_error {
	my($x) = @_;

	&error("error", $x);
}

sub subr_errorset {
	my($form, $arg) = @_;
	my($save, $robj);

	&error("errorset argument is neither t or nil", $arg) if (!&obj_is_t($arg) &&
								  !&obj_is_nil($arg));
	$save = $g_errorset_obj;
	$g_errorset_obj = $arg;
	eval { $robj = &eval_obj($form); };
	$g_errorset_obj = $save;
	return &new_pair($robj, $g_nil_obj) if (!$@);
	$@ = undef;
	return $g_nil_obj;
}

sub subr_eval {
	my($x) = @_;

	return &eval_obj($x);
}

sub subr_feed {
	my($x) = @_;

	&error("feed on non-number", $x) if (!&obj_is_num($x));
	print "\n" x &obj_value($x);
	return $g_nil_obj;
}

sub subr_fntyp {
	my($x) = @_;
	my($obj);

	&error("fntyp on non-symbol", $x) if (!&obj_is_symbol($x));
	$obj = &obj_funcdef($x);
	return &lookup_symbol("fsubr") if (&obj_is_fsubr($obj));
	return &lookup_symbol("subr") if (&obj_is_subr($obj));
	return $g_nil_obj if (!&obj_is_pair($obj));
	$obj = &obj_car($obj);
	return &lookup_symbol("expr") if ($obj == $g_lambda_sym);
	return &lookup_symbol("fexpr") if ($obj == $g_nlamda_sym);
	return $g_nil_obj;
}

sub subr_gensym {
	return &lookup_symbol(sprintf("A%04d", ++$g_gensymnum));
}

sub subr_getd {
	my($x) = @_;

	&error("getd to non-symbol", $x) if (!&obj_is_symbol($x));
	return &obj_funcdef($x);
}

sub subr_gcgag {
	my($x) = @_;
	my($oldval);

	&error("gcgag argument is neither t or nil", $x) if (!&obj_is_t($x) &&
							     !&obj_is_nil($x));
	$oldval = $g_gcgag_obj;
	$g_gcgag_obj = $x;
	return $oldval;
}

sub fsubr_go {
	my($x) = @_;

	$g_golabel = $x;
	die "go";
}

sub subr_greaterp {
	my($x, $y) = @_;

	return $g_nil_obj if ($f_doctor66 && &obj_is_nil($x) && &obj_is_nil($y));
	return $g_nil_obj if ($f_doctor66 && &obj_is_num($x) && &obj_is_nil($y));

	&error("non-numeric argument of greaterp", $x) if (!&obj_is_num($x));
	&error("non-numeric argument of greaterp", $y) if (!&obj_is_num($y));
	return (&obj_value($x) > &obj_value($y)) ? $g_t_obj : $g_nil_obj;
}

sub fsubr_lambda {
	my(@args) = @_;

	&error("lambda cannot be executed");
}

sub fsubr_list {
	my(@args) = @_;
	my($list, $tail);

	&error("list without any args") if (@args == 0);
	$list = &new_pair(&eval_obj(shift(@args)), $g_nil_obj);
	$tail = $list;
	while (@args) {
		&obj_set_cdr($tail, &new_pair(&eval_obj(shift(@args)), $g_nil_obj));
		$tail = &obj_cdr($tail);
	}
	return $list;
}

sub fsubr_loadfile {
	my($arg) = @_;

	&error("non-symbol for loadfile", $arg) if (!&obj_is_symbol($arg));
	$g_loadfile = &obj_pname($arg);
	if (!open(LOADFILE, $g_loadfile)) {
		&error("cannot open file", $arg);
		$g_loadfile = undef;
	}
	return $g_nil_obj;
}

sub fsubr_logand {
	my(@args) = @_;
	my($arg, $val);

	$val = ~0;
	while (@args) {
		$arg = &eval_obj(shift(@args));
		&error("non-numeric argument of logand", $arg) if (!&obj_is_num($arg));
		$val &= &obj_value($arg);
	}
	return &new_num($val);
}

sub fsubr_logor {
	my(@args) = @_;
	my($arg, $val);

	$val = 0;
	while (@args) {
		$arg = &eval_obj(shift(@args));
		&error("non-numeric argument of logor", $arg) if (!&obj_is_num($arg));
		$val |= &obj_value($arg);
	}
	return &new_num($val);
}

sub subr_member {
	my($x, $y) = @_;

	while (&obj_is_pair($y)) {
		return $g_t_obj if (&obj_is_t(&subr_equal($x, &obj_car($y))));
		$y = &obj_cdr($y);
	}
	&error("improper list to member", $y) if (!&obj_is_nil($y));
	return $g_nil_obj;
}

sub subr_minus {
	my($x) = @_;

	&error("non-numeric argument of minus", $x) if (!&obj_is_num($x));
	return &new_num(- &obj_value($x));
}

sub subr_nconc {
	my($x, $y) = @_;
	my($obj);

	return $y if (&obj_is_nil($x));

	&error("non-pair to nconc", $x) if (!&obj_is_pair($x));
	$obj = $x;
	while (&obj_is_pair(&obj_cdr($obj))) {
		$obj = &obj_cdr($obj);
	}
	&error("improper list to nconc", $x) if (!&obj_is_nil(&obj_cdr($obj)));
	&obj_set_cdr($obj, $y);
	return $x;
}

sub fsubr_nlamda {
	my(@args) = @_;

	&error("nlamda cannot be executed");
}

sub subr_null {
	my($x) = @_;

	return &obj_is_nil($x) ? $g_t_obj : $g_nil_obj;
}

sub subr_numberp {
	my($x) = @_;

	return &obj_is_num($x) ? $g_t_obj : $g_nil_obj;
}

sub subr_oblist {
	my($obj, $key);

	$obj = $g_nil_obj;
	foreach $key (keys %g_symtab) {
		next unless (defined ($g_symtab{$key}));
		$obj = &new_pair($g_symtab{$key}, $obj);
	}
	return $obj;
}

sub fsubr_or {
	my(@args) = @_;
	my($arg);

	foreach $arg (@args) {
		return $g_t_obj if (!&obj_is_nil(&eval_obj($arg)));
	}
	return $g_nil_obj;
}

sub subr_pack {
	my($x) = @_;
	my($item, $str);

	&error("pack of non-pair", $x) if (!&obj_is_pair($x));
	while (&obj_is_pair($x)) {
		$item = &obj_car($x);
		&error("pack item not a symbol", $item) if (!&obj_is_symbol($item));
		$str .= &obj_pname($item);
		$x = &obj_cdr($x);
	}
	&error("pack list not nil terminated", $x) if (!&obj_is_nil($x));
	return &lookup_symbol($str);
}

sub fsubr_plus {
	my(@args) = @_;
	my($arg, $val);

	$val = 0;
	while (@args) {
		$arg = &eval_obj(shift(@args));
		&error("non-numeric argument of plus", $arg) if (!&obj_is_num($arg));
		$val += &obj_value($arg);
	}
	return &new_num($val);
}

sub subr_prin1 {
	my($x) = @_;

	&print_obj(STDOUT, $x) if ($g_typeout_obj == $g_t_obj);
	&print_obj(PUNCHFILE, $x) if ($g_punchon_obj == $g_t_obj &&
				      defined($g_punchoutfile));
	return $x;
}

sub subr_print {
	my($x) = @_;

	if ($g_typeout_obj == $g_t_obj) {
		&print_obj(STDOUT, $x);
		print "\n";
	}
	if ($g_punchon_obj == $g_t_obj && defined($g_punchoutfile)) {
		&print_obj(PUNCHFILE, $x);
		print PUNCHFILE "\n";
	}
	return $x;
}

sub fsubr_prog {
	my(@args) = @_;
	my($vars, $varp, $nvars, $expr, $entry, $rval);
	my(@exprs);

	&error("prog requires two arguments") if (@args < 2);
	$vars = shift(@args);
	$nvars = 0;
	while (&obj_is_pair($vars)) {
		$varp = &obj_car($vars);
		&error("non-symbol in prog vars", $varp) if (!&obj_is_symbol($varp));
		push(@g_pushdown, [ $varp, &obj_value($varp), &obj_proplist($varp), &obj_funcdef($varp) ]);
		&obj_set_value($varp, $g_nil_obj);
		&obj_set_proplist($varp, $g_nil_obj);
		&obj_set_funcdef($varp, $g_nil_obj);
		$vars = &obj_cdr($vars);
		++$nvars;
	}
	&error("prog var ends in improper list", $vars) if (!&obj_is_nil($vars));
	$rval = $g_nil_obj;
	@exprs = @args;
	for (;;) {
		eval {
			foreach $expr (@exprs) {
				next if (!&obj_is_pair($expr));   # Skips labels
				&eval_obj($expr);
			}
		};
		last unless ($@);
		if ($@ =~ /^go /) {
			@exprs = @args;
			while (@exprs) {
				$label = shift(@exprs);
				last if ($label == $g_golabel);
			}
			next if (@exprs > 0);
			# The following eval intentionally throws an error
			eval { &error("label not found", $g_golabel); };
		} elsif ($@ =~ /^return /) {
			$rval = $g_returnobj;
			$@ = undef;
		}
		last;
	}
	while ($nvars > 0) {
		$entry = pop(@g_pushdown);
		$varp = $entry->[0];
		&obj_set_value($varp, $entry->[1]);
		&obj_set_proplist($varp, $entry->[2]);
		&obj_set_funcdef($varp, $entry->[3]);
		--$nvars;
	}
	die $@ if ($@);
	return $rval;
}

sub subr_prog1 {
	my($x, $y) = @_;

	return $x;
}

sub subr_prog2 {
	my($x, $y) = @_;

	return $y;
}

sub fsubr_progn {
	my(@args) = @_;
	my($obj);

	$obj = $g_nil_obj;
	while (@args) {
		$obj = &eval_obj(shift(@args));
	}
	return $obj;
}

sub subr_punchon {
	my($x) = @_;
	my($oldval);

	&error("punchon argument is neither t or nil", $x) if (!&obj_is_t($x) &&
							       !&obj_is_nil($x));
	$oldval = $g_punchon_obj;
	$g_punchon_obj = $x;
	return $oldval;
}

sub subr_putd {
	my($x, $y) = @_;

	&error("putd to non-symbol", $x) if (!&obj_is_symbol($x));
	&obj_set_funcdef($x, $y);
	return $y;
}

sub subr_quit {
	die "quit called";
}

sub fsubr_quote {
	my(@args) = @_;
	my($obj);

	&error("quote without any args") if (@args == 0);
	$obj = pop(@args);
	while (@args) {
		$obj = &new_pair(pop(@args), $obj);
	}
	return $obj;
}

sub subr_quotient {
	my($x, $y) = @_;

	&error("non-numeric argument of quotient", $x) if (!&obj_is_num($x));
	&error("non-numeric argument of quotient", $y) if (!&obj_is_num($y));
	&error("divide by zero") if (&obj_value($y) == 0);
	return &new_num(int(&obj_value($x) / &obj_value($y)));
}

sub subr_ratom {
	my($c, $str);

	$str = "";
	for (;;) {
		&fill_inbuf() if ($g_inbuf eq "");
		$g_inbuf =~ /^(.)(.*)$/s;
		($c, $g_inbuf) = ($1, $2);

		if ($f_doctor66 && $c eq "\n") {
			return &lookup_symbol("L14") if ($str eq "");
			$g_inbuf = $c . $g_inbuf;
			return &lookup_symbol($str);
		}

		if ($g_seprchars{$c}) {
			next if ($str eq "");
			return &lookup_symbol($str);
		}

		if ($g_brkchars{$c}) {
			return &lookup_symbol($c) if ($str eq "");
			$g_inbuf = $c . $g_inbuf;
			return &lookup_symbol($str);
		}
		$str .= $c;
	}
}

sub subr_read {
	return &read_obj();
}

sub subr_readin {
	my($x) = @_;
	my($oldval);

	&error("readin argument is neither t or nil", $x) if (!&obj_is_t($x) &&
							      !&obj_is_nil($x));
	$oldval = $g_readin_obj;
	$g_readin_obj = $x;
	return $oldval;
}

sub subr_reclaim {
	return &new_num($g_reclaim_value);
}

sub subr_remob {
	my($x) = @_;

	&error("remob on non-symbol", $x) if (!&obj_is_symbol($x));
	delete $g_symtab{&obj_pname($x)};
	return $g_nil_obj;
}

sub subr_return {
	my($x) = @_;

	$g_returnobj = $x;
	die "return";
}

sub subr_rplaca {
	my($x, $y) = @_;

	&error("cannot rplaca on object", $x) if (!&obj_is_pair($x));
	&obj_set_car($x, $y);
	return $x;
}

sub subr_rplacd {
	my($x, $y) = @_;

	if (&obj_is_symbol($x)) {
		&obj_set_cdr($x, $y);
	} elsif (&obj_is_pair($x)) {
		&obj_set_cdr($x, $y);
	} else {
		&error("cannot rplacd on object", $x);
	}
	return $x;
}

sub fsubr_select {
	my(@args) = @_;
	my($q, $y);

	&error("too few arguments to select") if (@args < 2);
	$q = &eval_obj(shift(@args));
	while (@args > 1) {
		$y = shift(@args);
		&error("non-pair for select", $y) if (!&obj_is_pair($y));
		return &eval_obj(&subr_cadr($y)) if (&subr_eq($q, &eval_obj(&obj_car($y))) == $g_t_obj);
	}
	return &eval_obj(shift(@args));
}

sub fsubr_selectq {
	my(@args) = @_;
	my($q, $y);

	&error("too few arguments to select") if (@args < 2);
	$q = &eval_obj(shift(@args));
	while (@args > 1) {
		$y = shift(@args);
		&error("non-pair for select", $y) if (!&obj_is_pair($y));
		return &eval_obj(&subr_cadr($y)) if (&subr_eq($q, &obj_car($y)) == $g_t_obj);
	}
	return &eval_obj(shift(@args));
}

sub subr_set {
	my($x, $y) = @_;

	&error("set of non-symbol", $x) if (!&obj_is_symbol($x));
	&obj_set_value($x, $y);
	return $y;
}

sub fsubr_setbrk {
	my(@args) = @_;
	my($arg);

	%g_brkchars = ();
	foreach $arg (@args) {
		&error("setbrk on non-symbol", $arg) if (!&obj_is_symbol($arg));
		$g_brkchars{&obj_pname($arg)} = 1;
	}
	return $g_nil_obj;
}

sub subr_setn {
	my($x, $y) = @_;

	&error("setn of non-symbol", $x) if (!&obj_is_symbol($x));
	&error("setn to non-number", $y) if (!&obj_is_num($y));
	if (!&obj_is_num(&obj_value($x))) {
		&obj_set_value($x, &new_num(&obj_value($y)));
	} else {
		&obj_set_value(&obj_value($x), &obj_value($y));
	}
	return &obj_value($x);
}

sub fsubr_setq {
	my($x, $y) = @_;

	&error("setq of non-symbol", $x) if (!&obj_is_symbol($x));
	&obj_set_value($x, &eval_obj($y));
	return &obj_value($x);
}

sub fsubr_setsepr {
	my(@args) = @_;
	my($arg);

	%g_seprchars = ();
	foreach $arg (@args) {
		&error("setsepr on non-symbol", $arg) if (!&obj_is_symbol($arg));
		$g_seprchars{&obj_pname($arg)} = 1;
	}
	return $g_nil_obj;
}

sub subr_terpri {
	print "\n" if ($g_typeout_obj == $g_t_obj);
	print PUNCHFILE "\n" if ($g_punchon_obj == $g_t_obj &&
				 defined($g_punchoutfile));
	return $g_nil_obj;
}

sub fsubr_times {
	my(@args) = @_;
	my($arg, $val);

	$val = 1;
	while (@args) {
		$arg = &eval_obj(shift(@args));
		&error("non-numeric argument of times", $arg) if (!&obj_is_num($arg));
		$val *= &obj_value($arg);
	}
	return &new_num($val);
}

sub subr_typein {
	my($x) = @_;
	my($oldval);

	&error("typein argument is neither t or nil", $x) if (!&obj_is_t($x) &&
							      !&obj_is_nil($x));
	$oldval = $g_typein_obj;
	$g_typein_obj = $x;
	return $oldval;
}

sub subr_typeout {
	my($x) = @_;
	my($oldval);

	&error("typeout argument is neither t or nil", $x) if (!&obj_is_t($x) &&
							       !&obj_is_nil($x));
	$oldval = $g_typeout_obj;
	$g_typeout_obj = $x;
	return $oldval;
}

sub subr_unpack {
	my($x) = @_;
	my($str, $c, $obj);

	&error("unpack of non-symbol", $x) if (!&obj_is_symbol($x));
	$obj = $g_nil_obj;
	$str = &obj_pname($x);
	while ($str =~ /^(.*)(.)$/s) {
		($str, $c) = ($1, $2);
		$obj = &new_pair(&lookup_symbol($c), $obj);
	}
	return $obj;
}

sub new_fsubr { return [ $O_FSUBR, [ @_ ], undef ]; }
sub new_nil { return [ $O_NIL, undef, undef ]; }
sub new_num { return [ $O_NUM, @_, undef ]; }
sub new_pair { return [ $O_PAIR, @_ ]; }
sub new_subr { return [ $O_SUBR, [ @_ ], undef ]; }
sub new_symbol { return [ $O_SYMBOL, $_[1], $g_nil_obj, $_[0], $g_nil_obj ]; }
sub new_t { return [ $O_T, undef, undef ]; }

sub obj_is_fsubr { $_[0]->[0] == $O_FSUBR; }
sub obj_is_nil { $_[0]->[0] == $O_NIL; }
sub obj_is_num { $_[0]->[0] == $O_NUM; }
sub obj_is_pair { $_[0]->[0] == $O_PAIR; }
sub obj_is_subr { $_[0]->[0] == $O_SUBR; }
sub obj_is_symbol { $_[0]->[0] == $O_SYMBOL; }
sub obj_is_t { $_[0]->[0] == $O_T; }

sub obj_is_atom { $_[0]->[0] & $O_ATOM; }

sub obj_car { $_[0]->[1]; }
sub obj_cdr { $_[0]->[2]; }
sub obj_value { $_[0]->[1]; }
sub obj_proplist { $_[0]->[2]; }
sub obj_pname { $_[0]->[3]; }
sub obj_funcdef { $_[0]->[4]; }

sub obj_set_car { $_[0]->[1] = $_[1]; }
sub obj_set_cdr { $_[0]->[2] = $_[1]; }
sub obj_set_value { $_[0]->[1] = $_[1]; }
sub obj_set_proplist { $_[0]->[2] = $_[1]; }
sub obj_set_funcdef { $_[0]->[4] = $_[1]; }

sub lookup_symbol {
	my($name) = @_;
	my($sym);

	$sym = $g_symtab{$name};
	if (!defined($sym)) {
		$sym = &new_symbol($name, $g_nobind_obj);
		$g_symtab{$name} = $sym;
	}
	return $sym;
}

sub print_obj {
	my($fp, $obj) = @_;

	if (&obj_is_num($obj)) {
		printf $fp "%g", &obj_value($obj);
	} elsif (&obj_is_symbol($obj)) {
		print $fp &obj_pname($obj);
	} elsif (&obj_is_pair($obj)) {
		print $fp "(";
		&print_obj($fp, &obj_car($obj));
		while (&obj_is_pair(&obj_cdr($obj))) {
			$obj = &obj_cdr($obj);
			print $fp " ";
			&print_obj($fp, &obj_car($obj));
		}
		if (!&obj_is_nil(&obj_cdr($obj))) {
			print $fp " . ";
			&print_obj($fp, &obj_cdr($obj));
		}
		print $fp ")";
	} elsif (&obj_is_t($obj)) {
		print $fp "t";
	} elsif (&obj_is_nil($obj)) {
		print $fp "nil";
	} elsif (&obj_is_subr($obj)) {
		print $fp $obj->[1]->[0];
	} elsif (&obj_is_fsubr($obj)) {
		print $fp $obj->[1]->[0];
	}
}

sub error {
	my($msg, $obj) = @_;
	my($fobj);

	if ($g_errorset_obj == $g_t_obj) {
		print "$msg: ";
		&print_obj(STDOUT, $obj) if (defined($obj));
		print "\n";
		foreach $fobj (@g_funcstack) {
			print_obj(STDOUT, $fobj);
			print "\n";
		}
	}
	die "error";
	exit 2;
}
