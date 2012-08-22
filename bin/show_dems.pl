#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 name dem_files\n";
    exit(0);
  }

  if ( scalar(@ARGV) == 1 ){
    # Manufacture an output name out of the input
    my $name = $ARGV[0];
    $name =~ s/^\.\/*//g;
    $name =~ s/\//_/g;
    $name =~ s/\..*?$//g;
    @ARGV = ($name, @ARGV);
  }

  my $name = shift @ARGV;

  my %files;
  foreach my $file (@ARGV){
    $files{$file} = 1;
  }
  
  qx(rm -rf tmp; mkdir tmp;);
  my $count = 0;
  foreach my $file (keys %files){
    my $cmd = "~/visionworkbench/src/vw/tools/hillshade -o "
       . "tmp/hill$count.tif -a 315 -s 0 --nodata-value -32767 "
          .  $file;
    print "$cmd\n";
    qx($cmd);
    $count++;
  }

  print qx(pwd) . "\n";
  my $cmd = "image2qtree.pl $name tmp/*tif";
  print "$cmd\n";
  print qx($cmd) . "\n";
}
