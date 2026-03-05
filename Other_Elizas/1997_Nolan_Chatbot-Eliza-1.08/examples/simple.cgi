#!/usr/bin/perl

# This simple script implements a Chatbot::Eliza
# object in a cgi program.  It uses the CGI.pm module 
# written by Lincoln Stein.
#
# Needless to say, you must have the CGI.pm module
# installed and working properly with CGI scripts on
# your Web server before you can try to run this script.  
# CGI.pm is not included with Eliza.pm.  
# 
# Information about CGI.pm is here:  
# http://www.genome.wi.mit.edu/ftp/pub/software/WWW/cgi_docs.html

use CGI;
use Chatbot::Eliza;

my $cgi 	= new CGI;
my $chatbot 	= new Chatbot::Eliza;

srand( time ^ ($$ + ($$ << 15)) );    # seed the random number generator

print $cgi->header;
print $cgi->start_html;
print $cgi->start_multipart_form;
print $cgi->h2('Eliza session');

# These lines contain the "Eliza" functionality.
# User comments are passed through the module's transform
# method, and the output is used to prompt the user 
# for futher input. 
#
if ( $cgi->param() ) {
	$prompt = $chatbot->transform( $cgi->param('Comment') );
} else {
	$prompt = $chatbot->transform('Hello');
}

$cgi->param('Comment','');

print 	$cgi->h3($prompt),
	$cgi->br,
	$cgi->textarea(	-name => 'Comment',
			-wrap => 'yes',
			-rows => 3,
			-columns => 70 );

print 	$cgi->p,
	$cgi->submit('Action','Send to Eliza'),
	$cgi->reset('Reset');

print $cgi->endform;
print $cgi->end_html;

