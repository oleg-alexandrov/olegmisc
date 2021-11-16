#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use POSIX;
use Scalar::Util;

# Scale a polygon in ply format by a given amount
MAIN: {

  if (scalar (@ARGV) < 5){
    print "Usage: input.ply, output.ply, shiftx, shifty, shiftz\n";
    exit(0);
  }

  my $in_ply  = $ARGV[0];
  my $out_ply = $ARGV[1];
  my $shiftx  = $ARGV[2];
  my $shifty  = $ARGV[3];
  my $shiftz  = $ARGV[4];

  print "Reading $in_ply\n";
  open(FILE, "<$in_ply");
  my @lines = <FILE>;
  close(FILE);

  my $line_number = 0; # Line count will start from 1.
  my $num_verts = 0;
  my $first_vert_line = 0;
  foreach my $line (@lines){

    $line_number++;
    
    if ($line =~ /^element\s+vertex\s+(\d+)/i){
      $num_verts = $1;
    }

    if ($line =~ /^end_header/i){
      $first_vert_line = $line_number + 1;
    }

    # This is a bit fragile
    my $good_line = ($first_vert_line > 0 && $num_verts > 0 && $line_number >= $first_vert_line &&
                    $line_number < $first_vert_line + $num_verts);
      
    if (!$good_line) {
      next;
    }

    my @vals = split(/\s+/, $line);

    my $has_only_numbers = 1;
    foreach my $val (@vals) {
      if (!Scalar::Util::looks_like_number($val)) {
        $has_only_numbers = 0;
        print "Found invalid line: $line\n";
        exit(1);
      }
    }
    
    $vals[0] = $vals[0] + $shiftx;
    $vals[1] = $vals[1] + $shifty;
    $vals[2] = $vals[2] + $shiftz;
    $line = join(" ", @vals) . "\n";
  }

  print "Writing: $out_ply\n";
  open(FILE, ">$out_ply");
  print FILE join("", @lines);
  close(FILE);
  
}
