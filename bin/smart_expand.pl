#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Take a small DEM at one resolution, a big DEM at a second resolution.
  # Crop the big DEM at the projwin given by the first DEM.
  # Then shrink it or grow it until getting the desired number of lines.
  
  if (scalar(@ARGV) < 4){
    print "Usage: $0 smallClip bigDEM outLen outClip\n";
    exit(1);
  }

  my $inClip  = $ARGV[0];
  my $bigDem  = $ARGV[1];
  my $outLen  = $ARGV[2];
  my $outClip = $ARGV[3];

  if ($inClip eq $outClip){
    print "Input and output are same.\n";
    exit(1);
  }
  if ( -f $outClip){
    print "File exists $outClip\n";
    exit(1);
  }

  # If bigDEM is just a resolution, form it by interpolating inClip
  if ($bigDem =~ /^tr(\d+)$/){
    my $tr = $1;
    my $in_tr = get_pix_size($inClip);
    print "in and out tr is $in_tr $tr\n";
    if ($in_tr <= $tr){
      print "Input grid must be bigger than output grid.\n";
      exit(1);
    }
    
    $bigDem = "tmp.tif";
    print qx(rm -fv $bigDem) . "\n";
    my $cmd = "gdalwarp -tr $tr $tr -r cubicspline $inClip $bigDem";
    print "$cmd\n";
    print qx($cmd) . "\n";
  }
    
  my $ans = qx(gdalinfo $inClip);
  my ($ulx, $uly, $lrx, $lry);
  if ($ans =~ /Upper\s+Left\s+\(\s*(.*?)\s*,\s*(.*?)\s*\)/){
    $ulx = $1; $uly = $2;
  }else{
    print "Cannot match the upper-left corner.\n";
    exit(1);
  }
  if ($ans =~ /Lower\s+Right\s+\(\s*(.*?)\s*,\s*(.*?)\s*\)/){
    $lrx = $1; $lry = $2;
  }else{
    print "Cannot match the lower-right corner.\n";
    exit(1);
  }
  #print "window is $ulx $uly $lrx $lry\n";

  #print "projwin wid and ht is " . ($lrx-$ulx) . " " . ($uly - $lry) . "\n";
  #my $cmd = "gdal_translate -projwin $ulx $uly $lrx $lry $bigDem $outClip";
  #print "$cmd\n";
  #qx($cmd);

  my $pixSize_in = get_pix_size($inClip);
  my $pixSize_big = get_pix_size($bigDem);
  my $ctx = ($lrx + $ulx)/2;
  my $cty = ($lry + $uly)/2;

  # output projwin
  $ulx = $ctx - $outLen*$pixSize_big/2.0;
  $lrx = $ctx + $outLen*$pixSize_big/2.0;
  $lry = $cty - $outLen*$pixSize_big/2.0;
  $uly = $cty + $outLen*$pixSize_big/2.0;

  $ans = qx(gdalinfo $bigDem | grep -i "size is");
  print "ans is $ans\n";
  my ($wd, $ht);
  if ($ans =~ /size\s+is\s+(\d+)[,\s]+(\d+)/i){
    $wd = $1; $ht = $2;
    print "wd=$wd, ht=$ht\n";
  }else{
    print "could not match wid and height. \n";
    exit(1);
  }
  
  #print "projwin out $ulx $uly $lrx $lry\n";
  my $cmd = "gdal_translate -projwin $ulx $uly $lrx $lry $bigDem $outClip";
  print "$cmd\n";
  $ans =  qx($cmd 2>&1);
  print "$ans\n";
  my ($begx, $begy, $widx, $widy) = (0, 0, 0, 0);
  if ($ans =~ /Computed -srcwin\s+(.*?)\s+(.*?)\s+(.*?)\s+(.*?)\s+/i){
    $begx = $1; $begy = $2; $widx = $3; $widy = $4;
    if ($begx < 0){ $begx = 0; }
    if ($begy < 0){ $begy = 0; }
    if ($begx + $widx > $wd){ $widx = $wd - $begx; }    
    if ($begy + $widy > $wd){ $widy = $ht - $begy; }    
    $cmd = "gdal_translate -srcwin $begx $begy $widx $widy $bigDem $outClip";
    print "$cmd\n";
    print qx($cmd) . "\n";
  }else{
   print "No match!\n"; 
  }

  exit(1);
  

  my $dw = int(($wd - $outLen)/2.0); if ($dw <= 0){ $dw = 0; }
  my $dh = int(($ht - $outLen)/2.0); if ($dh <= 0){ $dh = 0; }
  #print "dw and dh is $dw $dh\n";

  my $tmp = $outClip . ".tmp.tif";
  my $nw = $outLen; if ($dw + $nw > $wd) { $nw = $wd - $dw;}
  my $nh = $outLen; if ($dh + $nh > $ht) { $nh = $ht - $dh;}

  print "nw and nh is $nw $nh\n";
  $cmd  = "gdal_translate -srcwin $dw $dh $nw $nh $outClip $tmp";
  print "$cmd\n";
  print qx($cmd);

  print qx(mv -fv $tmp $outClip) . "\n";
}

sub get_pix_size{
  my $file = shift @_;
  my $ans = qx(gdalinfo $file);
  if ($ans =~ /Pixel Size = \((.*?),/){
    return $1;
  }
  
  print "Could not find the pixel size from $file\n";
  exit(1);
}

  
