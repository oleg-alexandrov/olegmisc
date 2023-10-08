#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 file\n";
    exit(0);
  }

  open(FILE, "<$ARGV[0]");
  my @lines = split("\n", join("\n", <FILE>));
  close(FILE);

  foreach my $line (@lines){
    #$line = utf8::decode($line);
    
    $line =~ s/\x{2019}/\'/g;
    print "$line\n";
  }
}
