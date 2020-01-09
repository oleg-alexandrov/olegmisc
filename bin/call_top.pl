#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 progName\n";
    exit(0);
  }

  my $num_fails = 100;
  my $fail_iter = 0;
  my $progName = $ARGV[0];
  my $numHours = 10;
  my $spacing = 2; # seconds
  my $whoami = qx(whoami);
  $whoami =~ s/\s*$//g;
  my $ans;
  while(1){
    
    $ans = qx( top -u $whoami -b -n 1 | grep -v top | grep -v grep | grep $progName 2>&1 );
    print $ans;
    sleep $spacing;

    if ($ans =~ /^\s*$/){
      $fail_iter++;
    }

    if ($fail_iter > $num_fails){
      print "No program $progName, will exit!\n";
      exit(0);
    }
    
  }
  
}
