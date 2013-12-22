#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 options in.tif out.tif\n";
    exit(0);
  }

  my $opts = join(" ", @ARGV);
  my $file1 = "";
  if ($opts =~ /\b([^\s]*?.(?:ntf|tif))/i){
    $file1 = $1;
  }else{
    print "Could not match file!\n"; 
    exit(1);
  }
  print "File is is $file1\n";

  my $size = qx(gdalinfo -nogcp $file1 | grep "Size is");
  my ($sizeX, $sizeY) = (0, 0);
  if ($size =~ /Size is\s+(\d+),\s*(\d+)/){
    $sizeX = $1; $sizeY = $2;
  }else{
    print "Cannot match size!\n";
    exit(1);
  }

  my ($bf, $af, $a, $b, $c, $d);
  if ($opts =~ /^(.*?-srcwin\s+)(\d+)\s+(\d+)\s+(\d+)\s+(\d+)(.*?)$/){
    $bf = $1; $a = $2; $b = $3; $c = $4; $d = $5; $af = $6;
  }else{
   print "Cannot match -srcwin\n"; 
 }

  if ($a + $c > $sizeX){ $c = $sizeX - $a; }
  if ($b + $d > $sizeY){ $d = $sizeY - $b; }

  $opts = "gdal_translate -co TILED=yes -co INTERLEAVE=BAND -co BLOCKXSIZE=256 -co BLOCKYSIZE=256 $bf$a $b $c $d$af";
  print "$opts\n";
  qx($opts);
}
