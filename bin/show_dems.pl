#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use File::Basename;
use File::Spec;
use Cwd;
use lib dirname(File::Spec->rel2abs($0));
require 'utils.pl';

MAIN:{

  set_path();

  if (scalar(@ARGV) < 1){
    print "Usage: $0 name dem_files\n";
    exit(0);
  }

  maybe_call_itself_on_remote_host(@ARGV);

  if ( scalar(@ARGV) == 1 ){
    # Manufacture an output name out of the input
    @ARGV = ($ARGV[0], @ARGV);
  }

  my $name = shift @ARGV;
  $name =~ s/^\.\/*//g;
  $name =~ s/\//_/g;
  $name =~ s/\.(tif|ntf)\s*$//ig; # rm filename extension
  $name =~ s#\.#_#g;

  my %files;
  foreach my $file (@ARGV){
    $files{$file} = 1;
  }

  my $tmpDir = "tmp_" . $name;
  my $cmd = "rm -rf ./$tmpDir; mkdir $tmpDir";
  print "$cmd\n";
  system($cmd);
  my $count = 0;
  foreach my $file (keys %files){
    $cmd = "hillshade --azimuth 300 --elevation 20 -o "
       . "$tmpDir/hill$count.tif -s 0 "
          .  $file;
    print "$cmd\n";
    system($cmd);
    $count++;
  }

  print qx(pwd) . "\n";
  $cmd = "image2qtree.pl $name $tmpDir/*tif";
  print "$cmd\n";
  system($cmd);
  $cmd = "rm -rf ./$tmpDir";
  print "$cmd\n";
  print qx($cmd) . "\n";

}
