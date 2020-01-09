#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use List::Util qw[min max];
MAIN:{

  # Convert float tif file to int

  if (scalar(@ARGV) == 0){
    print "Usage: $0 fileIn fileOut\n";
    exit(0);
  }

  my $fileIn = shift @ARGV;
  my ($fileOut, $sigmaFactor) = ("", 0);
  if (scalar(@ARGV) >= 1){
    if ($ARGV[0] =~ /^[\d\.]*?$/){ # second arg is sigma factor
      $fileOut = "";
      $sigmaFactor = $ARGV[0];
    }else{
      $fileOut = shift @ARGV;
      if (scalar(@ARGV) >= 1){
        $sigmaFactor = $ARGV[0];
      }
    }
  }
  
  if ( $fileOut eq ""){
    $fileOut = $fileIn;
    $fileOut =~ s/\.(\w+)$/_int\.tif/g;
  }

  my $prefix;
  if ( -e $fileIn && $fileIn =~ /^(.*)\.(\w+)$/ ){
    $prefix = $1;
  }else{
    print "Invalid file: $fileIn\n";
    exit(1);
  }

  my $cmd;

  $cmd="rm -fv $fileIn*xml";
  #print "$cmd\n";
  qx($cmd);

  $cmd="gdalinfo -stats $fileIn";
  my $out = qx($cmd);
  #print "$cmd\n";
  #print "$out\n";

  my ($min, $max, $mean, $stddev) = (0, 1, 0.5, 0);
  if ($out =~ /STATISTICS_MINIMUM=(.*?)\s+/){ $min = $1;     }else{ print "Cannot find minimum\n"; $min  = 0; }
  if ($out =~ /STATISTICS_MAXIMUM=(.*?)\s+/){ $max = $1;     }else{ print "Cannot find maximum\n"; $max  = $min + 1; }
  if ($out =~ /STATISTICS_MEAN=(.*?)\s+/)   { $mean = $1;    }else{ print "Cannot find mean\n";   }
  if ($out =~ /STATISTICS_STDDEV=(.*?)\s+/)  { $stddev = $1; }else{ print "Cannot find stddev\n"; }

  my $nmin = $min;
  my $nmax = $max;
  if ($sigmaFactor > 0){
    $nmin = max($nmin, $mean - $sigmaFactor*$stddev);
    $nmax = min($nmax, $mean + $sigmaFactor*$stddev);
  }

  #gdal cannot handle numbers like 1e-5
  if ($nmin  =~ /e-/) {
    if ($nmin < 0){
      while ($nmin =~ /e-/) { $nmin *= 1.2; }
    }else{
      $nmin = 0.0;
    }
  }
  while ($nmax =~ /e-/)  { $nmax *= 1.2; }

  if ($sigmaFactor > 0){
    my $sigmaFactor2 = $sigmaFactor;
    $sigmaFactor2 =~ s/-/m/g;
    $sigmaFactor2 =~ s/\./p/g;
    $fileOut =~ s/\.tif/_sigma$sigmaFactor2.tif/g;
  }

  $cmd = "gdal_translate -scale $nmin $nmax 0 255 -ot byte -of GTiff $fileIn $fileOut";
  print "$cmd\n"; qx($cmd);

}
