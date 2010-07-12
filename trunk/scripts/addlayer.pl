#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 file.xg layer\n";
    exit(0);
  }

  my $file  = $ARGV[0];
  my $layer = $ARGV[1];
  
  open(FILE, "<$file"); my $text = <FILE>; close(FILE);

  my @lines = split("\n", $text);
  foreach my $line (@lines){
    $line =~ s/^\s*([\+\-\d\.e]+\s+[\+\-\d\.e]+).*?$/$1 ; $layer/g; 
  }
  $text = join("\n", @lines);

  open(FILE, ">$file"); print FILE $text; close(FILE);
  
}
