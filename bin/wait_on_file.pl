#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Wait until the file was not touched for a while, then do something on it.
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 file wait cmd\n";
    exit(0);
  }

  my $file = shift @ARGV;
  my $wait = shift @ARGV;
  my $cmd = join(" ", @ARGV);

  print "File is $file\n";
  print "cmd is $cmd\n";

  my $machine = qx(uname -n);
  print "Running on machine: $machine\n";

  my $numDays = 5;
  my $sleep = 10;
  my $total = $numDays*24*3600/$sleep;
  for (my $val = 0; $val < $total; $val++){
    
    sleep $sleep;

    if (! -f $file){
      print "File $file does not exist, will wait.\n";
      sleep $sleep;
      next;
    }

    
    my @array=stat($file);
    my $mod_time = $array[9];
    my $cur_time = time;
      
    my $diff_time = $cur_time - $mod_time;
    print "File: $file modified $diff_time seconds ago.\n";
    
    if ($diff_time  > $wait){
      print "$cmd\n";
      print qx($cmd) . "\n";
      last;
    }else{
      print "File: $file too new, will wait.\n";
      sleep $sleep;
      next;
    }
    
    
  }

}
