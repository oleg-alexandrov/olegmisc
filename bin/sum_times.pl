#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Date::Parse;
use POSIX qw{strftime};

MAIN:{
  
#   if (scalar(@ARGV) < 2){
#     print "Usage: $0 \n";
#     exit(0);
#   }

  my $sum = 0;
  my @lines = <STDIN>;
  foreach my $line (@lines){
    next unless($line =~ /(\d+:\d+:\d+)/);
    $line = $1;
    my $date = str2time($line) - str2time("0:0:0");
    print "$line $date\n";
    $sum += $date;
  }

  print "Time is $sum\n";
  my $hours = int($sum/3600);
  $sum = $sum - 3600*$hours;
  my $mins = int($sum/60);
  $sum = $sum - 60*$mins;
  print "Time is $hours:$mins:$sum\n";
#   my $date1 = "01:00:06";
#   my $date0 = "0:0:0";
#   print "date is $date\n";
  
}
