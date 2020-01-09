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

  my $intr_file       = $ARGV[0];
  my $input_dir       = $ARGV[1];
  my $out_file        = $ARGV[2]; 

  my @vals;

  if (scalar(@ARGV) < 3){
    print "Must provide 3 inputs.\n";
    exit(1);
  }
  
  if (! -f $intr_file ){
    print "Missing $intr_file\n";
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

  my $all;
  my @images = glob($input_dir . '/*jpg' );
  my $num = scalar(@images);

  print "Writing: $out_file\n";
  open(OUT_FILE, ">$out_file");

  printf(OUT_FILE "%d\n", $num);

  foreach my $image (@images){

    my $cam2world_file = $image; $cam2world_file =~ s/.jpg/_cam2world.txt/g;
    my $world2cam_file = $image; $world2cam_file =~ s/.jpg/_world2cam.txt/g;
  
    open(FILE, "<$cam2world_file");
    $all = join(" ", <FILE>);
    $all =~ s/^\s*//g;
    $all =~ s/\s*$//g;
    @vals = split(/\s+/, $all);
    close(FILE);
    
    my @Cc2w = ($vals[3], $vals[7], $vals[11]);
    my @Rc2w = ($vals[0], $vals[1], $vals[2],
              $vals[4], $vals[5], $vals[6],
              $vals[8], $vals[9], $vals[10]);
    
    open(FILE, "<$world2cam_file");
    $all = join(" ", <FILE>);
    $all =~ s/^\s*//g;
    $all =~ s/\s*$//g;
    @vals = split(/\s+/, $all);
    close(FILE);
    
    my @Cw2c = ($vals[3], $vals[7], $vals[11]);
    my @Rw2c = ($vals[0], $vals[1], $vals[2],
              $vals[4], $vals[5], $vals[6],
              $vals[8], $vals[9], $vals[10]);
    
  
    printf(OUT_FILE "%0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g ",
           $f, 0, $cx,
           0, $f, $cy,
           0, 0, 1
          );
    
    printf(OUT_FILE "%0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g ",
           $Rw2c[0], $Rw2c[1], $Rw2c[2],
           $Rw2c[3], $Rw2c[4], $Rw2c[5],
           $Rw2c[6], $Rw2c[7], $Rw2c[8]
          );
    printf(OUT_FILE "%0.17g %0.17g %0.17g ",
           $Cc2w[0], $Cc2w[1], $Cc2w[2]);
    
    printf(OUT_FILE "%0.17g %0.17g ",
           $widx, $widy);
    
    printf(OUT_FILE "\n");
  }
  
  close(OUT_FILE);
}
