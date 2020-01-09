#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # print images with indices in given range,
  # with a spacing no less than $spacing
  if (scalar(@ARGV) < 2){
    print "Print lines from file with line number in [min, max). Indices start from 0. Usage: cat file.txt | $0 min max\n";
    exit(0);
  }
  my $min  = int(shift @ARGV);
  my $max  = int(shift @ARGV);
  #my $file = shift @ARGV;

  #open(FILE, "<$file"); my @lines = <FILE>; close(FILE);

  my $count = 0;
  foreach my $line (<STDIN>){
    if ($count >= $min && $count < $max){
      print "$line";
    }
    $count++;
  }
  
}
