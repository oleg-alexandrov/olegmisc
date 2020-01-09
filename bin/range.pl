#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# Take input from STDIN, and print lines with line numbers in [beg, end), where line number starts from 0
MAIN:{

  if (scalar(@ARGV) < 2){
    print "Usage: cat file.txt | $0 beg end\n";
    exit(1);
  }
  my $beg = $ARGV[0];
  my $end = $ARGV[1];

  my $count = 0;
  foreach my $line (<STDIN>){
    if ($count >= $beg && $count < $end){
      print $line;
    }
    $count++;
  }

}
