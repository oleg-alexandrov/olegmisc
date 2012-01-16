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

  foreach my $line (<STDIN>){
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g;
    my @vals = split(/\s+/, $line);
    foreach my $num (@ARGV){
      next unless ($num =~ /^\d+$/ && scalar(@vals) >= $num);
      print $vals[$num - 1] . "\t";
    }
    print "\n";
  }
  
}
