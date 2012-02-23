#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  my %unique;
  foreach my $line (<STDIN>){
    $unique{$line} = 1;
  }

  print "Unique lines in the input:\n";
  foreach my $line (keys %unique){
    print $line;
  }
  
}
