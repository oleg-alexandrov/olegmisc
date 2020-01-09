#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Set an alias by ensuring quotes are put where necessary
  my $str = join(" ", <STDIN>);
  $str =~ s/^\s*//g; 
  $str =~ s/\s*$//g;
  $str =~ s/(=)(.*?)(\s*$)/$1\'$2\'$3/g;  # quotes
  my $ba = $ENV{'HOME'} . '/.bash_aliases';
  my $machine = qx(uname -n);
  $machine =~ s/\..*?$//g; $machine =~ s/\s*$//g;
  #print "appending on $machine $str to $ba\n";
  open(FILE, ">>$ba");
  print FILE "$str\n";
  close(FILE);
}
