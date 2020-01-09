#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use POSIX;
undef $/; # undefines the separator. Can read one whole file in one scalar.

# Scale a polygon by a given amount
MAIN: {

  if (scalar (@ARGV) < 4  ){
    print "Usage: input xg shiftx shifty output xg\n";
    exit(0);
  }

  my $in_xg   = $ARGV[0];
  my $shiftx  = $ARGV[1];
  my $shifty  = $ARGV[2];
  my $out_xg  = $ARGV[3];

  open(FILE, "<$in_xg") || die "Can't open file $in_xg";
  my $text = <FILE>; 
  close(FILE);

  $text =~ s/(^|\n)([ \t]*)([\-\+\.\d]+)([ \t]+)([\-\+\.\d]+)/$1 . $2 . ($3+$shiftx) . $4 . ($5+$shifty)/ge;

  open(FILE, ">$out_xg");
  print FILE "$text";
  close(FILE);
    
}
