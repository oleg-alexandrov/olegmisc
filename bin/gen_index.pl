#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 files\n";
    exit(0);
  }

  my $index = "index.html";
  my @files = @ARGV;

  open(FILE, ">$index");
  foreach my $file (@files){
    print FILE "<p><a href=\"$file\">$file</a>\n";
  }
  
}
