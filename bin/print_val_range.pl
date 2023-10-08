#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  if (scalar(@ARGV) < 3) {
    print "Print lines from file with value (second column) in [min, max).\n" .
       " Usage: $0 file.txt min max\n";
    exit(0);
  }
  my $file = shift @ARGV;
  my $min  = shift @ARGV;
  my $max  = shift @ARGV;
  
  open(FILE, "<$file"); my @lines = <FILE>; close(FILE);

  foreach my $line (@lines){
    my @vals = split(/\s/, $line);
    if (scalar(@vals) < 2){
      print "Ignoring line with too few elements: $line\n";
    }

    if ($vals[1] >= $min && $vals[1] < $max) {
      print "$line";
    }
  }
  
}
