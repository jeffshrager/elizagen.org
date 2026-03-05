#!perl

use strict;
use warnings;
use Chatbot::Eliza;
use Test::More 0.88;

my @TESTS =
(
    [ 'I feel happy'       => 'Do you often feel happy?'    ], 
    [ 'I like blueberries' => 'I like blueberries too!'     ], 
    [ 'xyzzy'              => 'Huh?'                        ],
);
my ($input, $output, $expected);

plan tests => int(@TESTS);

my $bot = Chatbot::Eliza->new('TestBot', 't/test-script.txt')
            || BAIL_OUT;

foreach my $test (@TESTS) {
    ($input, $expected) = @$test;
    $output = $bot->transform($input);
    is($output, $expected, "Do we get expected output for '$input'");
}

