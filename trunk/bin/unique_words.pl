#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  # Usage: cat file.txt | unique_words.pl

  my %unique;
  foreach my $line (<STDIN>){
    my @words = split(/\s+/, $line);
    foreach my $word (@words){
      next if ($word =~ /^\s*$/);
      $unique{$word} = 1;
    }
  }

  my $output = "";
  foreach my $word (sort {$a cmp $b} keys %unique){
    print "$word\n"; # don't change this!
  }
  
}
