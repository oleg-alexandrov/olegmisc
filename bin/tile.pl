#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Cwd;
use File::Spec;
MAIN:{

  # Break a tif into tiles.

  if (scalar(@ARGV) < 4){
    print "Usage: input.tif tile_sizex tile_sizey outDir pad\n";
    exit(1);
  }
  
  my $img       = $ARGV[0];
  my $tileSizeX = $ARGV[1];
  my $tileSizeY = $ARGV[2];
  my $outDir    = $ARGV[3];
  my $pad       = $ARGV[4];
  my $cmd;
  
  
  print "img=$img\n";

  my $size = qx(gdalinfo -nogcp $img | grep "Size is");
  my ($sizeX, $sizeY) = (0, 0);
  if ($size =~ /Size is\s+(\d+),\s*(\d+)/){
    $sizeX = $1; $sizeY = $2;
  }

  print "size is $sizeX $sizeY\n";
  
  my $nTilesX = int($sizeX/$tileSizeX + 0.5);
  my $nTilesY = int($sizeY/$tileSizeY + 0.5);
  print "num tiles is $nTilesX by $nTilesY\n";
  
  # Make tiles about the same size
  $tileSizeX = int( $sizeX / $nTilesX ); $nTilesX = int($sizeX/$tileSizeX) || 1;
  $tileSizeY = int( $sizeY / $nTilesY ); $nTilesY = int($sizeY/$tileSizeY) || 1;

  my $imgPref;
  if ($outDir =~ /^(.*?)\/(.*?)$/){
    $outDir = $1;
    $imgPref = "$1/$2-tile-";
  }else{
    $imgPref = $outDir . "/tile-";
  }
  mkdir $outDir;
  
  my $numPix = $nTilesX*$nTilesY;
  my $count = 1;
  while ($count < $numPix){
    $count *= 10;
  }
  
  my $list = "";
  for (my $x = 0; $x < $nTilesX; $x++){
    for (my $y = 0; $y < $nTilesY; $y++){
      $count++;

      $list = "$list $count";
         
      my $begX = ($x-0)*$tileSizeX - $pad; $begX = 0      if ($begX < 0);
      my $endX = ($x+1)*$tileSizeX + $pad; $endX = $sizeX if ($endX > $sizeX);
      my $widX = $endX - $begX;
      next if ($widX <= 0);
      
      my $begY = ($y-0)*$tileSizeY - $pad; $begY = 0      if ($begY < 0);
      my $endY = ($y+1)*$tileSizeY + $pad; $endY = $sizeY if ($endY > $sizeY);
      my $widY = $endY - $begY;
      next if ($widY <= 0);

      my $tag = "x$count"; $tag =~ s/x1//g;
      
      my $imgTile = "$imgPref$tag.tif";
      
      $cmd = "gdal_translate -co TILED=yes -co INTERLEAVE=BAND -co BLOCKXSIZE=256 -co BLOCKYSIZE=256 -co compress=lzw -co bigtiff=no -srcwin $begX $begY $widX $widY $img $imgTile";
      print "$cmd\n";
      print qx($cmd) . "\n";
      #rename_nicely($imgTile, $outDir);
    }
  }
  
}

sub rename_nicely {

  # Rename to tile_148E_6S.tif
  my $tile = shift;
  my $outDir = shift;
  
  my $cr = qx(gdalinfo $tile | grep "Upper Left");
  my ($lon, $lat);
  if ($cr =~ /\(\s*(.*?)\s*,\s*(.*?)\s*\)/){
    $lon = $1; $lat = $2;
  }else{
    print "Could not match $lat and $lon\n";
    exit(1);
  }
  $lon = int($lon + 0.5);
  $lat = int($lat + 0.5);

  if ($lon < 0){
    $lon = (-$lon) . "W";
  }else{
    $lon = $lon . "E";
  }
  
  if ($lat < 0){
    $lat = (-$lat) . "S";
  }else{
    $lat = $lat . "N";
  }

  my $suffix = $outDir;
  $suffix =~ s/^.*\///g;
  my $ntile = $outDir . "/" . $suffix . "_" . $lon . "_" . $lat . ".tif";
  print "$ntile\n";

  # This check is necessary to not overwrite perhaps a different tile
  # with same name.
  if ( -f $ntile){
    print "Tile $ntile exists.\n";
    exit(1);
  }

  print qx(mv -fv $tile $ntile) . "\n";
  sleep 2; # NFS break
  
  # Wipe empty tiles.
  my $out = qx(gdalinfo -stats $ntile);
  print "Out is $out\n";
  if ( ($out =~ /Failed/is && $out =~ /compute/is) ||
       $out !~ /STATISTICS_MEAN/is
     ){
    #print "---EMPTY: $ntile\n";
    print "Wiping empty tile: $ntile\n";
    print qx(rm -fv $ntile) . "\n";
  }else{
    #print "---NOT EMPTY: $ntile\n";
  }
  print qx(rm -fv $ntile*aux.xml) . "\n";
  
}
