#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use File::Basename;
use File::Spec;
use Cwd;
use lib dirname(File::Spec->rel2abs($0));
require 'utils.pl';

MAIN:{
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 name dem_files\n";
    exit(0);
  }

  maybe_call_itself_on_remote_host(@ARGV);

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

  my $tmpDir = "tmp_" . $name;
  my $cmd = "rm -rf ./$tmpDir; mkdir $tmpDir";
  print "$cmd\n";
  qx($cmd);
  my $count = 0;
  foreach my $file (keys %files){
    $cmd = "~/projects/visionworkbench/src/vw/tools/hillshade -o "
       . "$tmpDir/hill$count.tif -a 315 -s 0 --nodata-value -32767 "
          .  $file;
    print "$cmd\n";
    qx($cmd);
    $count++;
  }

  print qx(pwd) . "\n";
  $cmd = "image2qtree.pl $name $tmpDir/*tif";
  print "$cmd\n";
  print qx($cmd) . "\n";
  print "rm -rf ./$tmpDir\n";
  print qx(rm -rf ./$tmpDir) . "\n";

}
