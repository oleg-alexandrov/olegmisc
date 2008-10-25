#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

# Scale a polygon by a given amount
MAIN: {

  if (scalar (@ARGV) <= 2  ){
    print "Usage: input xg, output xg, scale factor\n";
    exit(0);
  }

  my $in_xg  = $ARGV[0];
  my $out_xg = $ARGV[1];
  my $scale  = $ARGV[2];

  open(FILE, "<$in_xg") || die "Can't open file $in_xg";
  my $text = <FILE>; 
  close(FILE);

  $text =~ s/\b(\d+)\b/$scale*$1/ges;

  open(FILE, ">$out_xg");
  print FILE "$text";
  close(FILE);
    
}
