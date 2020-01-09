#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  # Usage: cat file.txt | unique_in_orig_order.pl

  my %unique;
  foreach my $line (<STDIN>){
    next if (exists $unique{$line});
    print $line;
    $unique{$line} = 1;
  }

  
}
