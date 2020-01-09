#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # print images with indices in given range,
  # with a spacing no less than $spacing
  if (scalar(@ARGV) < 4){
    print "Usage: $0 min max sample vals\n";
    exit(0);
  }
  my $min     = int(shift @ARGV);
  my $max     = int(shift @ARGV);
  my $spacing = shift @ARGV;
  my @vals = @ARGV;
  # print "vals are $min $max\n";

  my $count = 0;
  my $prev_num = -1;
  foreach my $val (@vals){
    $count++;
    next unless ($val =~ /^.*?\/.*?(\d+)\./);
    my $num = $count; # int($1);
    #print "num is $val $min $num $max\n";
    if ($num >= $min && $num < $max){
      if ($prev_num < 0 || $num - $prev_num >= $spacing){
        print "$val\n";
        $prev_num = $num;
      }
    }
  }
}
