#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  my $text = join(" ", @ARGV);
  my @a = split(" ", $text);
  if (scalar(@a) != 3){
    print "Usage: $0 x y z\n";
    exit(0);
  }
  
  foreach my $val (@a){ $val =~ s/[,\(\)]//g; }
  print sqrt($a[0]*$a[0] + $a[1]*$a[1] + $a[2]*$a[2]) . "\n";
  
}
