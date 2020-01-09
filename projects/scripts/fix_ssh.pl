#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{

  my $dir = "$ENV{HOME}/bin";
  chdir $dir;
  print `svn update` . "\n";
  
  open(FILE, "<index.html");
  my $ip = <FILE>;
  close(FILE);
  $ip =~ s/[^\d\.]//g;

  my $aliases = "$ENV{HOME}/.bash_aliases";
  open(FILE, "<$aliases");
  my $text = <FILE>;
  close(FILE);

  $text =~ s/(sho=\'.*?ssh\s+)([\d\.]+)/$1$ip/g;

  open(FILE, ">$aliases");
  print FILE $text;
  close(FILE);
  
}
