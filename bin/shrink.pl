#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Crop by given value along certain sides
  
  if (scalar(@ARGV) < 5){
    print "Usage: $0 in.tif wid wy sides out.tif\n";
    exit(0);
  }

  my $in    = $ARGV[0];
  my $wx    = $ARGV[1];
  my $wy    = $ARGV[2];
  my $sides = $ARGV[3];
  my $out   = $ARGV[4];

  my ($sizeX, $sizeY) = find_size($in);

  my ($lx, $ux, $ly, $uy) = (0, $sizeX, 0, $sizeY);
  print "Size is $sizeX $sizeY\n";

  $lx += $wx if ($sides =~ /l/);
  $ux -= $wx if ($sides =~ /r/);
  $ly += $wy if ($sides =~ /t/);
  $uy -= $wy if ($sides =~ /b/);
    
  my $widx = $ux - $lx;
  my $widy = $uy - $ly;

  my $cmd = "gdal_translate -srcwin $lx $ly $widx $widy $in $out";
  print "$cmd\n";
  print qx($cmd) . "\n";
}

sub tmp {
  
  my @ARGV;
  
  my $cmd = join(" ", @ARGV);
  $cmd =~ s/://g; # rm stray chars
  my $file1 = "";
  if ($cmd =~ /([^\s]*\.(?:ntf|tif|cub|vrt|jpg|png))/i){
    $file1 = $1;
  }else{
    print "Could not match file!\n";
    exit(1);
  }
  print "File is $file1\n";

  # Shrink the crop window to fit within the image
  my $size = qx(gdalinfo -nogcp $file1 | grep "Size is");
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
    $cmd = "gdal_translate $cmd";
    print "$cmd\n";
    my $ans = qx($cmd);
    if ($ans !~ /Computed -srcwin\s+([\-\d]+)\s+([\-\d]+)\s+([\-\d]+)\s+([\-\d]+)\s+from projected window/){
      print "Command failed!\n";
      exit(1);
    }
    $a = $1; $b = $2; $c = $3; $d = $4;
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
  
  $cmd = "gdal_translate -co compress=lzw -co TILED=yes -co INTERLEAVE=BAND -co BLOCKXSIZE=256 -co BLOCKYSIZE=256 $bf $a $b $c $d $af";
  print "$cmd\n";
  my $ans = qx($cmd);
  print "$ans";
}

sub find_size {
  my $file1 = shift;
  
  # Shrink the crop window to fit within the image
  my $size = qx(gdalinfo -nogcp $file1 | grep "Size is");
  my ($sizeX, $sizeY) = (0, 0);
  if ($size =~ /Size is\s+(\d+),\s*(\d+)/){
    $sizeX = $1; $sizeY = $2;
  }else{
    print "Cannot match size!\n";
    exit(1);
  }

  return ($sizeX, $sizeY);
}
