#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use List::Util qw[min max];
MAIN:{

  # Create colormaps for multiple files, using as min and max the
  # values mean-sigmaFactor*stddev and mean+sigmaFactor*stddev for
  # each file. If an extra numerical value is specified, use that one
  # as sigmaFactor, otherwise use 1. If two numerical values are
  # specified, use those as min and max.

  if (scalar(@ARGV) == 0){
    print "Usage: $0 file1 ... file_n [ sigmaFactor | min max ] [ options ]\n";
    exit(0);
  }

  # Iterate over ARGV and extract the options.
  # What is left are files and numerical values.
  my @other = ();
  my @options = ();
  # iterate over the arguments
  foreach my $arg (@ARGV){
    if ($arg =~ /^\-/ && $arg !~ /^\-[\d\.]+/) {
      push(@options, $arg);
    }else{
      push(@other, $arg);
    }
  }
  @ARGV = @other;
  
  # Extract the files. Those must exist.
  my @files = ();
  @other = ();
  foreach my $file (@ARGV){
    if (-e $file){
      push(@files, $file);
    }else{
      push(@other, $file);
    }
  }
  @ARGV = @other;
  
  # Extract the sigma factor.
  my $sigmaFactor = 1.0;
  if (scalar(@ARGV) > 0) {
    $sigmaFactor = shift @ARGV;
  }

  # Actually there are two numerical values, the min and max to use
  # for all input files.
  my $useInputMinMax = 0;
  my ($inputMin, $inputMax);
  if (scalar (@ARGV) > 0){
    $useInputMinMax = 1;
    $inputMin = $sigmaFactor;
    $inputMax = shift @ARGV;
  }

  my $useSameMinMaxForAll = 0;
  # Use abs(sigmaFactor), and use same min and max for all iamges
  if ($sigmaFactor < 0 ){
    $useSameMinMaxForAll = 1;
    $sigmaFactor = -$sigmaFactor;
  }

  my @outFiles;
  my $cmd;
  foreach my $fileIn (@files){

    my ($nmin, $nmax);
    if (!$useInputMinMax){

      # Find the stats and infer the min and max
      $cmd="gdalinfo -stats $fileIn";
      my $out = qx($cmd);

      my ($min, $max, $mean, $stddev) = (0, 1, 0.5, 0);
      if ($out =~ /STATISTICS_MINIMUM=(.*?)\s+/) {
        $min = $1;
      } else {
        print "Cannot find minimum\n"; $min  = 0;
      }
      if ($out =~ /STATISTICS_MAXIMUM=(.*?)\s+/) {
        $max = $1;
      } else {
        print "Cannot find maximum\n"; $max  = $min + 1;
      }
      if ($out =~ /STATISTICS_MEAN=(.*?)\s+/) {
        $mean = $1;
      } else {
        print "Cannot find mean\n";
      }
      if ($out =~ /STATISTICS_STDDEV=(.*?)\s+/) {
        $stddev = $1;
      } else {
        print "Cannot find stddev\n";
      }

      $nmin = $mean - $sigmaFactor*$stddev;
      $nmax = $mean + $sigmaFactor*$stddev;

      # Correction for images with non-negative values
      if ($min >=0 && $nmin <= 0) {
        $nmin = 0;
      }
    
      #gdal cannot handle numbers like 1e-5
      if ($nmin  =~ /e-/) {
        if ($nmin < 0) {
          while ($nmin =~ /e-/) {
            $nmin *= 1.2;
          }
        } else {
          $nmin = 0.0;
        }
      }
      while ($nmax =~ /e-/) {
        $nmax *= 1.2;
      }

      if ($useSameMinMaxForAll == 1) {
        # All images wll use the nmin and nmax calculated for first image
        $useSameMinMaxForAll = $useSameMinMaxForAll + 1;
        $useInputMinMax = 1;
        $inputMin = $nmin;
        $inputMax = $nmax;
      }
      
    } else {
      # Use user-provided min and max
      $nmin = $inputMin;
      $nmax = $inputMax;
    }

    my $fileOut = $fileIn;
    $fileOut =~ s/\.\w+$/_CMAP/g;
    print qx(rm -fv $fileOut*) . "\n"; # Wipe cmap image and sub-images
    $fileOut .= ".tif";
    $cmd = "colormap --min $nmin --max $nmax $fileIn -o $fileOut";
    # Append the options
    $cmd .= " " . join(" ", @options);
    
    print "$cmd\n";
    system($cmd);
    print "Done\n";
    push(@outFiles, $fileOut);
    #print qx($cmd) . "\n";
    #print qx(~/bin/image2qtree.pl $fileOut) . "\n";
  }

  print "Wrote: " . join(" ", @outFiles) . "\n";
}
