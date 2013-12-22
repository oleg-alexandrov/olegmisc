#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 number\n";
    exit(0);
  }

  my $sum = 0;
  foreach my $arg (@ARGV){
    $sum += $arg;
  }
  print "Sum is $sum\n";
}
