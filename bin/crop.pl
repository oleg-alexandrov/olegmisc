#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# Wrapper around ISIS's crop.

MAIN:{

  if (scalar(@ARGV) < 5){
    print "Usage: $0 options in.cub startx starty widx widy\n";
    exit(0);
  }

  my $in = $ARGV[0];
  my $startx = $ARGV[1];
  my $starty = $ARGV[2];
  my $widx = $ARGV[3];
  my $widy = $ARGV[4];

  $in =~ s/://g; # rm artifacts

  my $out;
  if (scalar(@ARGV) <= 5){
    $out = $in;
    $out =~ s/.cub/_crop.cub/g;
  }else{
    $out = $ARGV[5];
  }

  my $size = qx(gdalinfo -nogcp $in | grep "Size is");
  my ($sizeX, $sizeY) = (0, 0);
  if ($size =~ /Size is\s+(\d+),\s*(\d+)/){
    $sizeX = $1; $sizeY = $2;
  }else{
    print "Cannot match size!\n";
    exit(1);
  }

  if ($startx <= 0){ $startx = 1;}
  if ($starty <= 0){ $starty = 1;}

  if ($startx + $widx > $sizeX){ $widx = $sizeX - $startx; }
  if ($starty + $widy > $sizeY){ $widy = $sizeY - $starty; }

  my $cmd = "crop from = $in to = $out sample = $startx line = $starty nsamples = $widx nlines = $widy";
  print "$cmd\n";
  print qx($cmd) . "\n";
}
