#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Remove subsequent instances of a given line
  
  # Usage: cat file.txt | unique.pl

  my %unique;
  my $i = 0;
  foreach my $line (<STDIN>){
    next if ($line =~ /^\s*$/);
    next if (exists $unique{$line});
    $unique{$line} = $i++;
  }

  foreach my $line (sort { $unique{$a} <=> $unique{$b} } keys %unique){
    print "$line";
  }
  
}
