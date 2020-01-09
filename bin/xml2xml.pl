#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # convert to opencv calibration
  if (scalar(@ARGV) < 2){
    print "Usage: $0 in.xml out.xml\n";
    exit(0);
  }

  my $file1 = $ARGV[0];
  my $file2 = $ARGV[1];

  print "files $file1 $file2\n";

  open(FILE, "<$file1");
  my $text = join("", <FILE>);
  my $val;
  if ($text =~ /<params>\s*\[\s*(.*?)\]/){
    $val = $1;
  }else{
    print "no match!\n";
  }
  $val =~ s/\s//g;
  print "$val\n";
  my @vals = split(";", $val);

  #546.2549; 546.7267; 637.1075; 473.6218; 0.9534997
  print "Writing: $file2\n";
  open(FILE, ">$file2");
  print FILE "<?xml version=\"1.0\"?>
<opencv_storage>
<intrinsic_matrix type_id=\"opencv-matrix\">
  <rows>3</rows>
  <cols>3</cols>
  <dt>d</dt>
  <data>
    $vals[0] 0. $vals[2]
    0. $vals[1] $vals[3]
    0. 0. 1.</data></intrinsic_matrix>
<distortion type_id=\"opencv-matrix\">
  <rows>1</rows>
  <cols>1</cols>
  <dt>d</dt>
  <data>
    $vals[4]</data></distortion>
<width>1280</width>
<height>960</height>
<undistorted_width>3000</undistorted_width>
<undistorted_height>1800</undistorted_height>
</opencv_storage>
\n";
  close(FILE);
}
