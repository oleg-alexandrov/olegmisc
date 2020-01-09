#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Scalar::Util qw(looks_like_number);
use List::Util   qw(max min);

MAIN:{

  # Swap two lines in a file
  
  if (scalar(@ARGV) < 4){
    print "Usage: $0 filein line1 line2 fileout\n";
    exit(0);
  }

  my $in  = $ARGV[0];
  my $l1  = $ARGV[1];
  my $l2  = $ARGV[2];
  my $out = $ARGV[3];

  open(FILE, "<$in");
  my @lines = <FILE>;
  close(FILE);

  my $tmp = $lines[$l1-1];
  $lines[$l1-1] = $lines[$l2-1];
  $lines[$l2-1] = $tmp;

  open(FILE, ">$out");
  print FILE join("", @lines);
  close(FILE);
}
