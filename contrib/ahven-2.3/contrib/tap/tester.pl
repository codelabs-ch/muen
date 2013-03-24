#!/usr/bin/perl

use TAP::Parser;
use TAP::Parser::Aggregator;

my $parser = TAP::Parser->new( { exec => [ './tap_tester' ]  } );
$parser->run;
# while ( my $result = $parser->next ) {
#     print $result->as_string . "\n";
# }

my $aggregate = TAP::Parser::Aggregator->new;
$aggregate->add( 'testcases', $parser );
printf "\tPassed: %s\n\tFailed: %s\n", scalar $aggregate->passed, scalar $aggregate->failed;

