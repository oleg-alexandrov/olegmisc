#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use POSIX qw/ceil/;

MAIN:{

  # Prepend a line to a file
  if (scalar(@ARGV) < 2){
    print "Usage: file.txt line\n";
    exit(0);
  }

  my $in = $ARGV[0];
  
  my $line = $ARGV[1];
  $line = $line . "\n";
  
  open(FILE, "<$in");
  my @lines = ($line, <FILE>);
  close(FILE);
  open(FILE, ">$in");
  foreach $line (@lines){
      print FILE "$line";
    }
  close(FILE);
}
