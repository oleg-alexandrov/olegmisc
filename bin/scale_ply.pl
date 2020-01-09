#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use POSIX;
use Scalar::Util;

# Scale a polygon in ply format by a given amount

MAIN: {

  if (scalar (@ARGV) < 3  ){
    print "Usage: input ply, output ply, scale\n";
    exit(0);
  }

  my $in_ply  = $ARGV[0];
  my $out_ply = $ARGV[1];
  my $scale  = $ARGV[2];

  open(FILE, "<$in_ply");
  my @lines = <FILE>;
  close(FILE);

  foreach my $line (@lines){
    my @vals = split(/\s+/, $line);

    # This is a bit fragile
    if (scalar(@vals) != 3){
      next;
    }
    
    my $has_only_numbers = 1;
    foreach my $val (@vals) {
      if (!Scalar::Util::looks_like_number($val)) {
        $has_only_numbers = 0;
        last;
      }
    }
    
    if (!$has_only_numbers){
      next;
    }
    
    if ($has_only_numbers){
      foreach my $val (@vals) {
        $val *= $scale;
      }
      $line = join(" ", @vals) . "\n";
    }
  }

  open(FILE, ">$out_ply");
  print FILE join("", @lines);
  close(FILE);
  
}
