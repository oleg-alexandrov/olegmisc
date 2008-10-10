#!/usr/bin/perl
use strict;                            # 'strict' insists that all variables be declared
use diagnostics;                       # 'diagnostics' expands the cryptic warnings
undef $/;

# make double f(int a, int b) into f(a, b)

MAIN:{
  my $junk;
  my @anarray;

  $_ = <>;

  s/\/\/.*?\n/\n/g; # strip comments
  s/\n/ /g;          # strip newlines
  s/\*//g;

  if (! /(^.*?)\((.*?)\)/ ){
    print "Failed to match function prototype\n";
    exit(0);
  }
  
  my $a=$1; my $b=$2; # $b is the list of args
  
  ($junk, $a)=split(' ', $a) if (/ /);
  
  $b =~ s/\s*,\s*/,/g;
  my @args = split(',', $b);
  
  for (my $i=0; $i<=$#args; $i++) {
    @anarray=split(' ', $args[$i]);
    $args[$i]=$anarray[$#anarray];
    $args[$i] =~ s/\&//g;
  }
  
  $b=join(', ', @args);
  
  print "$a\($b\);";
}
