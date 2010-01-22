#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{

  # Replace the word passed as input with the line in the alias file
  # which defines that word as an alias.
  # For example, if "pp" was the input to this script, this
  # script will print the line
  # alias pp=<the name of the alias>
  # provided that this alias exists.
  
  my $in = $ARGV[0];
  
  open(FILE, "<$ENV{HOME}/.bash_aliases");
  my $text = <FILE>;
  close(FILE);

  my @lines = split("\n", $text);

  my $match = 0;
  foreach my $line (@lines){
    next unless ($line =~ /(alias $in=.*?)$/);
    $match = 1;
    print "$line\n";
    last;
  }

  if (!match){
    print "$in\n";
  }
}
