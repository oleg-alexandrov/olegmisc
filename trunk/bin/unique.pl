#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  # Usage: cat file.txt | unique.pl

  my %unique;
  foreach my $line (<STDIN>){
    next if (exists $unique{$line});
    $unique{$line} = 1;
  }

  print "Unique lines in the input:\n";
  foreach my $line (sort {$a cmp $b} keys %unique){
    print $line;
  }
  
}
