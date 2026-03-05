#!/usr/bin/perl

# In this example, we create two bots, and have them
# talk to each other.   This program exposes the 
# weaknesses of the default "psychiatrist" script. 
# This would be more interesting with better scripts.

use Chatbot::Eliza

my ($harry, $sally, $he_says, $she_says);

# Turn autoflush on, so we can watch 
# the output as it is produced. 
$|=1; 

# Seed the random number generator. 
srand( time ^ ($$ + ($$ << 15)) ); 

$sally = new Chatbot::Eliza "Sally";
$harry = new Chatbot::Eliza "Harry";

$he_says  = "I am sad.";

my $loopcount = 5;

for ($i=0; $i < $loopcount; $i++) {

	$she_says = $sally->transform( $he_says );
	print $sally->name, ":  $she_says \n";

	$he_says  = $harry->transform( $she_says );
	print $harry->name, ":  $he_says \n";

}

1;
