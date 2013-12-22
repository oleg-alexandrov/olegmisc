#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Convert float tif file to int

  if (scalar(@ARGV) == 0){
    print "Usage: $0 fileIn fileOut\n";
    exit(0);
  }

  if (scalar(@ARGV) == 1){
    my $fileOut = $ARGV[0];
    $fileOut =~ s/DEM/hill/g;
    if ($fileOut !~ /hill/){
      $fileOut =~ s/.tif/-hill.tif/g;
    }
    push(@ARGV, $fileOut);
  }

  my $fileIn  = shift;
  my $fileOut = shift;

  print "gdaldem hillshade $fileIn $fileOut\n";
  print qx(gdaldem hillshade $fileIn $fileOut);
}
