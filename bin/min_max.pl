#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Scalar::Util;

MAIN:{
  # Take a column on standard input. Limit it to given bounds.
  
  if (scalar(@ARGV) < 2){
    print "Usage: cat file.txt | $0 minVal maxVal\n";
    exit(1);
  }
  my $min = $ARGV[0];
  my $max = $ARGV[1];
  
  foreach my $line (<STDIN>){
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g; # strip any newline (last line may not have one)
    
    $line =~ s/,//g;
    
    if ($line < $min){
      $line = $min;
    }
    
    if ($line > $max){
      $line = $max;
    }

    print "$line\n";
  }
}
