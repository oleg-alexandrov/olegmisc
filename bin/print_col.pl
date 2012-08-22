#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Print the specified columns in a file. This works
  # like awk '{print $1}' but is less to type.
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 colNumbers\n";
    exit(0);
  }

  my @cols;
  foreach my $num (@ARGV){
    $num -= 1; # start from 0 instead of 1
    push(@cols, $num);
  }
  
  foreach my $line (<STDIN>){
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g;
    my @vals = split(/\s+/, $line);
    foreach my $num (@cols){
      $num += scalar(@vals) if ($num < 0); # treat -1 as last element
      next unless ($num =~ /^\d+$/ && $num < scalar(@vals) );
      print $vals[$num] . "\t";
    }
    print "\n";
  }
  
}
