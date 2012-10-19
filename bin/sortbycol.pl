#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  if (scalar(@ARGV) < 1){
    print "Usage: cat file.txt $0 colNum\n";
    exit(1);
  }
  my $colNum = $ARGV[0]; 

  my %hash;
  foreach my $line (<STDIN>){
    my @vals = split(/\s+/, $line);
    if (scalar(@vals) < $colNum){
      print "Error: Not enough elements on line: $line\n";
      next;
    }
    $hash{$line} = $vals[$colNum - 1];
  }

  foreach my $line ( sort { $hash{$a} <=> $hash{$b} } keys %hash ){
    print $line;
  }
}
