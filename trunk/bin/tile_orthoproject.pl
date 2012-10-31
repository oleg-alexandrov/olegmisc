#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Cwd;
use File::Spec;
MAIN:{

  # Break a DEM into tiles, orthoproject on each tile, then combine the results

  my $tileSize = 500;
  my $demPad   = 10;
  my $cmd;
  
  # Separate the options
  my (@opt, @other);
  my $len = scalar(@ARGV);
  for (my $i = 0; $i < $len; $i++){
    if ($i < $len - 1 && $ARGV[$i] =~ /^-/){
      push(@opt, $ARGV[$i]);
      push(@opt, $ARGV[$i+1]);
      $i++;
    }else{
     push (@other, $ARGV[$i]); 
    }
  }

  if (scalar(@other) < 3){
    print "ERROR: Use this program with the same options as orthoproject.\n";
    exit(1);
  }
  
  my $dem = $other[0];
  my $cub = $other[1];
  my $drg = $other[2];
  my $opts = join(" ", @opt);
  
  print "dem=$dem\n";
  print "cub=$cub\n";
  print "drg=$drg\n";
  print "opts=$opts\n";

  my $size = qx(gdalinfo -stats $dem | grep "Size is");
  my ($sizeX, $sizeY) = (0, 0);
  if ($size =~ /Size is\s+(\d+),\s*(\d+)/){
    $sizeX = $1; $sizeY = $2;
  }

  my $nTilesX = int($sizeX/$tileSize) + 1;
  my $nTilesY = int($sizeY/$tileSize) + 1;

  my $dir = "tmp_" . generate_random_string(20);
  mkdir $dir;
  my $demPref = "$dir/dem-";
  my $drgPref = "$dir/drg-";
  my $vrt     = "$dir/vrt.tif";
  
  my $count = 0;
  my $list = "";
  for (my $x = 0; $x < $nTilesX; $x++){
    for (my $y = 0; $y < $nTilesY; $y++){
      $count++;

      $list = "$list $count";
         
      my $startX = $x*$tileSize - $demPad;
      $startX = 0 if ($startX < 0);
      my $widX = $tileSize + 2*$demPad;
      if ($startX + $widX > $sizeX){ $widX = $sizeX - $startX; }
      next if ($widX <= 0);
      
      my $startY = $y*$tileSize - $demPad;
      $startY = 0 if ($startY < 0);
      my $widY = $tileSize + 2*$demPad;
      if ($startY + $widY > $sizeY){ $widY = $sizeY - $startY; }
      next if ($widY <= 0);

      my $demTile = "$demPref$count.tif";
      my $drgTile = "$drgPref$count.tif";
      $cmd = "gdal_translate -co compress=lzw -srcwin $startX $startY $widX $widY $dem $demTile";
      print "$cmd\n";
      qx($cmd);
    }
  }

  $cmd = ". isis_setup.sh; echo $list | xargs -d ' ' -P 16 -I \{\} orthoproject $opts $demPref\{\}.tif $cub $drgPref\{\}.tif";
  print "$cmd\n";
  qx($cmd);
  
  $cmd = "gdalbuildvrt -resolution highest $vrt $drgPref*tif";
  print "$cmd\n";
  qx($cmd);

  $cmd = "gdal_translate -co compress=lzw $vrt $drg";
  print "$cmd\n";
  qx($cmd);

  $cmd = "rm -rfv ./$dir";
  print "$cmd\n";
  qx($cmd);
  
}

sub generate_random_string{
  
  my $len=shift;# the length of 
  # the random string to generate
  
  my @chars=('a'..'z','A'..'Z','0'..'9','_');
  my $random_string;
  foreach (1..$len){
    # rand @chars will generate a random 
    # number between 0 and scalar @chars
    $random_string.=$chars[rand @chars];
  }
  return $random_string;
}

