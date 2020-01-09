#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

# use main to avoid the curse of global variables
MAIN: {

  my $text = <>;
  my @lines = split("\n", $text);

  my ($line, %repeats, $path);

  $path = "";
  foreach $line (@lines){

    next if (exists $repeats{$line});
    $repeats{$line} = 1;

    $path = $path . ":" . $line;
  }

  $path =~ s/^://;
  
  print "$path\n";
}
