#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  # Convert float tif file to int 

  if (scalar(@ARGV) < 1){
    print "Usage: $0 fileName sigmaFactor\n";
    exit(0);
  }

  my $file = shift;
  my $useSigmaFactor =0;
  my $sigmaFactor = 0;
  if (scalar(@ARGV) >= 1){
    $sigmaFactor = shift;
    $useSigmaFactor = 1;
  }

  my $prefix;
  if ( -e $file && $file =~ /^(.*)\.(\w+)$/ ){
    $prefix = $1;
  }else{
    exit(1);
  }

  my $cmd;
  
  $cmd="rm -fv $file*xml";
  #print "$cmd\n";
  qx($cmd);

  $cmd="gdalinfo -stats $file";
  my $out = qx($cmd);
  #print "$cmd\n";
  #print "$out\n";

  my ($min, $max, $mean, $stddev);
  if ($out =~ /STATISTICS_MINIMUM=(.*?)\s+/){ $min = $1;     }else{ print "Cannot find minimum\n"; exit(1); }
  if ($out =~ /STATISTICS_MAXIMUM=(.*?)\s+/){ $max = $1;     }else{ print "Cannot find maximum\n"; exit(1); }
  if ($out =~ /STATISTICS_MEAN=(.*?)\s+/)   { $mean = $1;    }else{ print "Cannot find mean\n"; exit(1); }
  if ($out =~ /STATISTICS_STDDEV=(.*?)\s+/)  { $stddev = $1; }else{ print "Cannot find stddev\n"; exit(1); }

  my $nmin = $min;
  my $nmax = $max;
  if ($useSigmaFactor){
    $nmin = $mean - $sigmaFactor*$stddev;
    $nmax = $mean + $sigmaFactor*$stddev;
  }
  
  my $sigmaFactor2 = $sigmaFactor;
  $sigmaFactor2 =~ s/-/m/g;
  $sigmaFactor2 =~ s/\./p/g;
  my $fout = $prefix . "_sigma" . $sigmaFactor2 . ".tif";
  
  # gdal cannot handle 1e-5 or negative numbers
  if    ($nmin  =~ /-/) { $nmin   = 0;   }
  while ($nmax =~ /e-/) { $nmax *= 1.2; }
  
  $cmd = "gdal_translate -scale $nmin $nmax 0 255 -ot byte -of GTiff $file $fout";
  #$cmd = "scale_and_cast $nmin $nmax $file $fout";
  #$cmd = "colormap --min $nmin --max $nmax $file -o $fout";
  print "$cmd\n"; qx($cmd);

  $cmd = "image2qtree.pl $fout";
  print "$cmd\n"; print qx($cmd) . "\n";

}
