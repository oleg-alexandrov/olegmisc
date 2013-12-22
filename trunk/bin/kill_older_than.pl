#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Proc::ProcessTable;

MAIN:{

  # Kill program running for longer than certain number of minutes.
  # This is greedy! Kills too many things!
  # To do: Below, replace kill_older_than with prog name.

  if (scalar(@ARGV) < 2){
    print "Usage: $0 minutes progname\n";
    exit(0);
  }
  my $mins = $ARGV[0];
  shift @ARGV;
  my @progs = @ARGV;

  while (1){
    my $t = new Proc::ProcessTable;
    foreach my $p ( @{$t->table} ){

      my $pid = $p->pid;
      my $elapsed_mins = (time - $p->start)/60;
      #print $p->pid . " " . $p->cmndline . " " . $elapsed_mins . "\n";

      foreach my $prog (@progs){

        my $curr_prog = $p->cmndline;
        next unless ($curr_prog =~ /$prog/ && $curr_prog !~ /kill_older_than/
                     && $elapsed_mins >= $mins);
        print "Will kill pid $pid running for $elapsed_mins > $mins. "
           . "Prog: $curr_prog\n";
        qx(kill -9 $pid);
      }
    }

    #exit(1);
    print "Sleeping for 10 seconds in $0\n";
    sleep 10;
  }

}
