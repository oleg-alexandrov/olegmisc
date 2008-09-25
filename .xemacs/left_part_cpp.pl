#!/usr/bin/perl
use strict;                            # 'strict' insists that all variables be declared
use diagnostics;                       # 'diagnostics' expands the cryptic warnings

# make 'double myn::f(int a, int b){' into 'double f(int a, int b);'

undef $/;

MAIN:{
  
  $_=<>;
  
  s/\w+\:\://g; # strip namespace
  s/\)\s*\{/\);/g; 

  print "$_";
}
# $_ =~ s/\n//g;

# $_ =~ /(^.*?)\((.*?)\)/; 
# my $a=$1; my $b=$2;
# my @junk;


# $b =~ s/\s*,\s*/,/g;
# my @args = split(',', $b);

# for (my $i=0; $i<=$#args; $i++) {
#   @junk=split(' ', $args[$i]);
#   pop(@junk);
#   $args[$i]=join(' ', @junk);
# }

# $b=join(', ', @args);

# print "$a\($b\);";
