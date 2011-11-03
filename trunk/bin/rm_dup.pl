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

  my %hash;
  foreach my $line (@lines){
    $line =~ s/^\s*(.*?)\s*$/$1/g;
    next if ($line =~ /^\s*$/);
    next if (exists $hash{$line});
    print "$line\n";
    $hash{$line} = 1;
  }
}
