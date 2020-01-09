#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  my $sum = 0;
  my $num = 0;

  foreach my $line (<STDIN>){
    $line =~ s/\s//g;
    $sum += $line;
    $num += 1;
  }

  my $mean = $sum/$num;
  print "mean is $mean\n";
}
