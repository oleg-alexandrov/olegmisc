#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use POSIX;
use Scalar::Util;

# Scale a polygon in ply format by a given amount

MAIN: {

  if (scalar (@ARGV) < 5){
    print "Usage: input.ply, output.ply, shiftx, shifty, shiftz\n";
    exit(0);
  }

  my $in_ply  = $ARGV[0];
  my $out_ply = $ARGV[1];
  my $shiftx  = $ARGV[2];
  my $shifty  = $ARGV[3];
  my $shiftz  = $ARGV[4];

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
      $vals[0] = $vals[0] - $shiftx;
      $vals[1] = $vals[1] - $shifty;
      $vals[2] = $vals[2] - $shiftz;
      $line = join(" ", @vals) . "\n";
    }
    
  }

  open(FILE, ">$out_ply");
  print FILE join("", @lines);
  close(FILE);
  
}
