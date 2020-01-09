#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use List::Util qw[min max];

sub find_bounds {
  my $file = shift @_;
  my $cmd="gdalinfo -stats $file";
  my $out = qx($cmd);
  my ($min, $max) = (0, 1);
  if ($out =~ /STATISTICS_MINIMUM=(.*?)\s+/){ $min = $1;     }else{ print "Cannot find minimum\n"; $min  = 0; }
  if ($out =~ /STATISTICS_MAXIMUM=(.*?)\s+/){ $max = $1;     }else{ print "Cannot find maximum\n"; $max  = $min + 1; }
  #if ($out =~ /STATISTICS_MEAN=(.*?)\s+/)   { $mean = $1;    }else{ print "Cannot find mean\n";   }
  #if ($out =~ /STATISTICS_STDDEV=(.*?)\s+/)  { $stddev = $1; }else{ print "Cannot find stddev\n"; }

  return ($min, $max);
}

MAIN:{

  # Given n tif files, find the maximum of the their minimia, the
  # minimum of their maxima, and scale all images to this range.

  if (scalar(@ARGV) < 2){
    print "Usage: $0 file1 file2 ... \n";
    exit(0);
  }

  my ($min, $max);
  my $count = 0; 
  foreach my $file (@ARGV){
    $count = $count + 1; 
    my ($curr_min, $curr_max) = find_bounds($file);
    if ($count == 1){
      ($min, $max) = ($curr_min, $curr_max);      
    }else{
      $min = max($min, $curr_min);
      $max = min($max, $curr_max);
    }
  }
  
  foreach my $file (@ARGV){
    my $file_out = $file; $file_out =~ s/\.(\w+)$/_int\.tif/g;
    my $cmd = "gdal_translate -scale $min $max 0 255 -ot byte -of GTiff $file $file_out";
    print "$cmd\n";
    qx($cmd);
  }
    
}
