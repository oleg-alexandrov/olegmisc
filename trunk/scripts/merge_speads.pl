#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use List::Util qw(max);
undef $/;          # read one whole file in one scalar

MAIN:{
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 spreadSheet1.csv spreadSheet2.csv\n";
    exit(0);
  }

  open(FILE, "<$ARGV[0]"); my @sp0 = split("\n", <FILE>); close(FILE);
  open(FILE, "<$ARGV[1]"); my @sp1 = split("\n", <FILE>); close(FILE);

  my $len0 = scalar(@sp0);
  my $len1 = scalar(@sp1);
  my $len  = max($len0, $len1);
  
  my ($iter, $val0, $val1);
  for ($iter=0 ; $iter < $len ; $iter++){

    if ($iter < $len0) { $val0 = $sp0[$iter];    }
    else               { $val0 = "";             }
    
    if ($iter < $len1) { $val1 = $sp1[$iter];    }
    else               { $val1 = "";             }

    print "$val0, $val1\n";
  }

  
}
