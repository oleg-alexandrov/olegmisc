#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use List::Util qw(max);
require "$ENV{HOME}/bin/xg_utils.pl";

undef $/;          # read one whole file in one scalar

MAIN:{
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 file.xg\n";
    exit(0);
  }

  my $file = $ARGV[0];
  my @polys = read_xg($file);
  print_xg(\@polys);
  
  foreach my $poly (@polys){
    
    my ($len, $wid) = (0, 0);
    my ($prevVert, $ctrx, $ctry, $numVerts) = (0, 0, 0, 0);
    
    foreach my $vert (@$poly){

      my $x = $vert->[0];
      my $y = $vert->[1];
      $ctrx += $x;
      $ctry += $y;
      $numVerts++;
      
      if ($prevVert != 0){
        my $px = $prevVert->[0];
        my $py = $prevVert->[1];
        $len = max($len, abs($x - $px));
        $wid = max($wid, abs($y - $py));
      }
      
      print $vert->[0] . " " . $vert->[1] . "\n";
      $prevVert = $vert;
    }

    # Make the polygon closed
    if ( scalar(@$poly) >= 1 ){
      my $vert = $poly->[0];
      print $vert->[0] . " " . $vert->[1] . "\n";
    }
    
    $ctrx /= $numVerts;
    $ctry /= $numVerts;
    print "anno $ctrx $ctry $len $wid\n";
    print "NEXT\n";
    
  }
  
}
