#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# For each line, see how often it shows up

MAIN:{

  if (scalar(@ARGV) < 1){
    print "Usage: $0 fileName\n";
    exit(1);
  }

  my $file = $ARGV[0];
  open(FILE, "<$file");
  my @lines = <FILE>;
  close(FILE);

  my %hash;
  foreach my $line (@lines){
    $line =~ s/\s*$//g;
    $hash{$line}++;
  }

  foreach my $key ( sort { $hash{$a} <=> $hash{$b} } keys %hash ){
    print "$key $hash{$key}\n";
  }

}
