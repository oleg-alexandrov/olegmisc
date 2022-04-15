#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# Wrapper around ISIS's crop.

MAIN:{

  if (scalar(@ARGV) < 2){
    print "Usage: $0 options in.cub scale\n";
    exit(0);
  }

  my $in = $ARGV[0];
  my $scale = $ARGV[1];

  my $out = $in; $out =~ s/.cub/_sub$scale.cub/g;

  my $cmd = "reduce from = $in to = $out sscale = $scale lscale = $scale";
  print "$cmd\n";
  print qx($cmd) . "\n";
}
