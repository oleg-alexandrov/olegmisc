#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use List::Util qw(min max);

MAIN:{

  if (scalar(@ARGV) < 2){
    print "Usage: file1.tif file2.tif \n";
    exit(0);
  }

  my $file1 = shift @ARGV;
  my $file2 = shift @ARGV;

  my $res1 = qx(source $ENV{HOME}/.bashenv; gdalinfo $file1);
  my ($lx1, $ly1, $ux1, $uy1) = parse($res1);

  my $res2 = qx(source $ENV{HOME}/.bashenv; gdalinfo $file2);
  my ($lx2, $ly2, $ux2, $uy2) = parse($res2);

#   print "$lx1 $ly1 $ux1 $uy1\n";
#   print "$lx2 $ly2 $ux2 $uy2\n";

  my $lx = max($lx1, $lx2); my $ly = max($ly1, $ly2);
  my $ux = min($ux1, $ux2); my $uy = min($uy1, $uy2);

  print "$lx $uy $ux $ly\n"; # $uy and $ly inverted on purpose

}

sub parse{

  my ($lx, $ly, $ux, $uy);
  my $text = shift;

  if ( $text =~ /Upper Left\s+\((.*?)\s*,\s*(.*?)\)/ ){
    $lx = $1; $uy = $2;
  }else{
    print "No match\n";
    exit(1);
  }

  if ( $text =~ /Lower Right\s+\((.*?)\s*,\s*(.*?)\)/ ){
    $ux = $1; $ly = $2;
  }else{
    print "No match\n";
    exit(1);
  }

  return ($lx, $ly, $ux, $uy);

}
