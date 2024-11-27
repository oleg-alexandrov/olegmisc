#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  if (scalar(@ARGV) < 2){
    print "Usage: $0 options in.tif out.tif\n";
    exit(0);
  }

  #$ENV{'PATH'} = $ENV{'HOME'} . '/miniconda3/envs/asp/bin' . ':' . $ENV{'PATH'};

  my $gdal_opts = "-co compress=lzw -co TILED=yes -co INTERLEAVE=BAND -co BLOCKXSIZE=256 -co BLOCKYSIZE=256 -co BIGTIFF=YES";
  
  my $cmd = join(" ", @ARGV);
  $cmd =~ s/://g; # rm stray chars
  my $file = "";
  if ($cmd =~ /([^\s]*\.(?:ntf|nitf|tif|cub|vrt|jpg|png))/i){
    $file = $1;
  }else{
    print "Could not match file!\n";
    exit(1);
  }
  print "File is $file\n";

  # Reorder things intelligently
  if ($cmd =~ /^(.*?)(-srcwin|-projwin)(\s+)($file)(.*?)$/){
    $cmd = $1 . $4 . $3 . $2 . $5;
  }
  
  # Shrink the crop window to fit within the image
  my $size = qx(gdalinfo -nogcp $file | grep "Size is");
  my ($sizeX, $sizeY) = (0, 0);
  if ($size =~ /Size is\s+(\d+),\s*(\d+)/){
    $sizeX = $1; $sizeY = $2;
  }else{
    print "Cannot match size!\n";
    exit(1);
  }

  print "size is $sizeX $sizeY\n";
  my ($bf, $af, $a, $b, $c, $d) = ("", "", "", "", "", "");

  # Capture srcwin bounds
  my $crop = 0;
  if ($cmd =~ /^(.*?)-projwin\s+[^\s]*?\s+[^\s]*?\s+[^\s]*?\s+[^\s]*?\s+(.*?)$/){

    $bf = "$1 -srcwin"; $af = $2;
    $crop = 1;
    $cmd = "gdal_translate $gdal_opts $cmd";
    print "$cmd\n";
    my $ans = qx($cmd);
    print "$ans\n";
    exit(0);
  }elsif ($cmd =~ /^(.*?-srcwin)\s+([\-\d]+)\s+([\-\d]+)\s+([\-\d]+)\s+([\-\d]+)(.*?)$/){
    $crop = 1;
    $bf = $1; $a = $2; $b = $3; $c = $4; $d = $5; $af = $6;
  }

  # Correct the src bounds
  my $correct = 0;
  if ($crop){
    if ($a < 0){ $a = 0; $correct = 1; }
    if ($b < 0){ $b = 0; $correct = 1; }
    if ($a + $c > $sizeX){ $c = $sizeX - $a; $correct = 1; }
    if ($b + $d > $sizeY){ $d = $sizeY - $b; $correct = 1; }
  }else{
   $bf = $cmd;
  }

  if ($correct){
    print "Corrected srcwin bounds.\n";
  }
  
  $cmd = "gdal_translate $gdal_opts $bf $a $b $c $d $af";
  print "$cmd\n";
  my $ans = qx($cmd);
  print "$ans";
}
