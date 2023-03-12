#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  if (scalar(@ARGV) < 2){
    print "Print lines from file with line number in [min, max). Indices start from 0. Usage: cat file.txt | $0 min max. Optionally specify the spacing.\n";
    exit(0);
  }
  my $min  = int(shift @ARGV);
  my $max  = int(shift @ARGV);

  my $spacing = 1;
  if (scalar(@ARGV) > 0){
    $spacing = int(shift @ARGV);
  }

  my $count = 0;
  my $prev_count = -$spacing;
  foreach my $line (<STDIN>){
    if ($count >= $min && $count < $max && $count - $prev_count >= $spacing){
      print "$line";
      $prev_count = $count;
    }
    $count++;
  }
  
}
