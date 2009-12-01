#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{
  
  my $a = $ARGV[0];
  my $b = $ARGV[1];

  print 100*( 1 - &conv($b)/&conv($a) ) ."\n";
  
}


sub conv {

  my $val = shift;

  $val =~ /^\s*(\d+):(\d+):(\d+)/;

  return $1*3600 + $2*60 + $3;
  
}
