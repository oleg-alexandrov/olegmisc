#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Return true if a file is newer than 10 seconds.
  if (scalar(@ARGV) < 1){
    print "Usage: $0 file\n";
    exit(0);
  }

  my $file = shift;
  if ( ! -e $file ){
    print "0\n";
  }

  my @array=stat($file);
  my $mod_time = $array[9];
  my $cur_time = time;
  
  my $diff_time = $cur_time - $mod_time;
  if ($diff_time > 10){
    print "1\n";
  }else{
   print "0\n"; 
  }

}
