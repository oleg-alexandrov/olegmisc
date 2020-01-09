#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  if (scalar(@ARGV) < 3){
    print "Usage: $0 im dir1 dir2";
    exit(0);
  }

  my @im;
  my $head;
  open(FILE, "<$ARGV[0]");
  foreach my $line (<FILE>){
    $line =~ s/\n//g;
    if ($line =~ /^\%/){
      $head = $line;
      next;
    }
    push(@im, $line);
  }

  my %im2num;
  my $count=0;
  foreach my $file (<$ARGV[1]/*jpg>){
    $file =~ s/^.*?\///g;
    $im2num{$file} = $count;
    $count++;
  }

  if (scalar(keys %im2num) != scalar(@im)){
    exit(1);
  }

  print "$head\n";
  foreach my $file (<$ARGV[2]/*jpg>){
    $file =~ s/^.*?\///g;
    print $im[$im2num{$file}] . "\n";
  }

}
