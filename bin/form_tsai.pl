#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 intrinsics.txt cam2world.txt\n";
    exit(0);
  }

  my $intr_file = $ARGV[0];
  my $cam_file  = $ARGV[1];
  my @vals;

  if (! -f $intr_file || ! -f $cam_file){
    print "Missing either $intr_file or $cam_file\n";
    exit(1);
  }
  
  open(FILE, "<$intr_file");
  @vals = split(/\s+/, join(" ", <FILE>));
  my $f = $vals[0];
  my $cx = $vals[1];
  my $cy = $vals[2];
  close(FILE);

  open(FILE, "<$cam_file");
  my $all = join(" ", <FILE>);
  $all =~ s/^\s*//g;
  $all =~ s/\s*$//g;
  @vals = split(/\s+/, $all);
  close(FILE);

  my @C = ($vals[3], $vals[7], 1737400 + $vals[11]);
  my @R = ($vals[0], $vals[1], $vals[2],
           $vals[4], $vals[5], $vals[6],
           $vals[8], $vals[9], $vals[10]);
  
  $cam_file =~ s/\.txt/\.tsai/g;
  print "Writing: $cam_file\n";
  open(FILE, ">$cam_file");
  printf(FILE "VERSION_4\n");
  printf(FILE "PINHOLE\n");
  printf(FILE "fu = %0.17g\n", $f);
  printf(FILE "fv = %0.17g\n", $f);
  printf(FILE "cu = %0.17g\n", $cx);
  printf(FILE "cv = %0.17g\n", $cy);
  printf(FILE "u_direction = 1  0  0\n");
  printf(FILE "v_direction = 0  1  0\n");
  printf(FILE "w_direction = 0  0  1\n");
  printf(FILE "C = %0.17g %0.17g %0.17g\n",
         $C[0], $C[1], $C[2]);
  printf(FILE "R = %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g %0.17g\n",
         $R[0], $R[1], $R[2],
         $R[3], $R[4], $R[5],
         $R[6], $R[7], $R[8]
        );
  printf(FILE "pitch = 1.0\n");
  printf(FILE "NULL\n");
  close(FILE);
}
