#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# Convert a .ply to .csv. Then create a DEM with:
# point2dem --csv-format 1:x,2:y,3:z --datum MOON --stereographic --proj-lon 0 --proj-lat 90 --tr 0.01 file.csv --t_projwin 0.138 -0.154 2.038 -2.654

my $height = 1737400.0;
my $shift = 1; # to avoid pole artifacts

MAIN:{
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 file.ply\n";
    exit(0);
  }

  my $ply_file = $ARGV[0];
  my $csv_file = $ply_file;
  $csv_file =~ s/\.\w+$/.csv/g;

  if ($ply_file eq $csv_file){
    print "Input is already csv.\n";
    exit(1);
  }
  open(FILE, "<$ply_file");
  open(FILE2, ">$csv_file");
  print "Writing: $csv_file\n";
  foreach my $line (<FILE>){
    if ($line =~ /^[a-z]/i || $line =~ /^\s*$/){
      next;
    }

    # Skip info on faces
    if ($line =~ /^3\s+\d+\s+\d+\s+\d+\s*$/){
      next;
    }
    
    $line =~ s/^\s*//g;
    my @vals = split(/\s+/, $line);
    if (scalar(@vals) == 3){
      printf(FILE2 "%0.17g %0.17g %0.17g\n",
             $vals[0] + $shift, $vals[1] + $shift, $vals[2] + $height);
    }else {
      if (scalar(@vals) >= 4){
        # print pixel value as well
        printf(FILE2 "%0.17g %0.17g %0.17g %0.17g\n",
               $vals[0] + $shift, $vals[1] + $shift,
               $vals[2] + $height, $vals[3] + $height
              );
      }
    }
    
  }
  close(FILE);
  close(FILE2);
}
