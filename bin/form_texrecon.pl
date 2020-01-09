#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

# Within a scene folder a .cam file has to be given for each image.
# A .cam file is structured as follows:
#     tx ty tz R00 R01 R02 R10 R11 R12 R20 R21 R22
#     f d0 d1 paspect ppx ppy
# First line: Extrinsics - translation vector and rotation matrix
# Second line: Intrinsics - focal length, distortion coefficients, pixel aspect ratio and principal point
# The focal length is the distance between camera center and image plane normalized by dividing with the larger image dimension.
# For non zero distortion coefficients the image will be undistorted prior to the texturing process. If only d0 is non zero the Noah Snavely's distortion model is assumed otherwise the distortion model of VSFM is assumed.
# The pixel aspect ratio is usually 1 or close to 1. If your SfM system doesn't output it, but outputs a different focal length in x and y direction, you have to encode this here.
#    The principal point has to be given in unit dimensions (e.g. 0.5 0.5).

  if (scalar(@ARGV) < 3){
    print "Usage: $0 intrinsics.txt cam2world.txt out_dir\n";
    exit(0);
  }

  my $intr_file = $ARGV[0];
  my $cam_file  = $ARGV[1];
  my $out_dir   = $ARGV[2];
  my @vals;

  if (! -f $intr_file || ! -f $cam_file){
    print "Missing either $intr_file or $cam_file\n";
    exit(1);
  }
  
  open(FILE, "<$intr_file");
  my $line;
  foreach my $curr_line (<FILE>){
    if ($curr_line =~ /^\#/){
      next;
    }
    $line = $curr_line;
    last;
  }
  @vals = split(/\s+/, $line);
  my $widx = $vals[0];
  my $widy = $vals[1];
  my $f = $vals[2];
  my $cx = $vals[3];
  my $cy = $vals[4];

  print "f cx cy widx widy $f $cx $cy $widx $widy\n";
  close(FILE);

  open(FILE, "<$cam_file");
  my $all = join(" ", <FILE>);
  $all =~ s/^\s*//g;
  $all =~ s/\s*$//g;
  @vals = split(/\s+/, $all);
  close(FILE);

  my @C = ($vals[3], $vals[7], $vals[11]);
  my @R = ($vals[0], $vals[1], $vals[2],
           $vals[4], $vals[5], $vals[6],
           $vals[8], $vals[9], $vals[10]);
  
  my $max_wid = $widx;
  if ($widy > $max_wid){
    $max_wid = $widy;
  }
  print "max wid is $max_wid\n";

  my $nf = $f / $max_wid;
  my $ncx = $cx / $widx;
  my $ncy = $cy / $widy;
  my $d0 = 0;
  my $d1 = 0;
  my $paspect = 1;
  
  $cam_file =~ s/\.txt/\.cam/g;
  $cam_file =~ s/^.*\///g;
  $cam_file = $out_dir . "/" . $cam_file;
  
  print "Writing: $cam_file\n";
  open(FILE, ">$cam_file");
  printf(FILE "%0.17g %0.17g %0.17g ",
         $C[0], $C[1], $C[2]);
  printf(FILE "%0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g\n",
         $R[0], $R[1], $R[2],
         $R[3], $R[4], $R[5],
         $R[6], $R[7], $R[8]
        );

  printf(FILE "%0.17g %0.17g %0.17g %0.17g %0.17g %0.17g\n",
         $nf, $d0, $d1, $paspect, $ncx, $ncy);

  close(FILE);
}
