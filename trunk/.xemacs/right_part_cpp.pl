#!/usr/bin/perl
use strict;                            # 'strict' insists that all variables be declared
use diagnostics;                       # 'diagnostics' expands the cryptic warnings
my $junk;
my @anarray;
# make double f(int a, int b) into f(a, b)
undef $/;
$_=<>;
$_ =~ s/\n//g;
s/\*//g;

$_ =~ /(^.*?)\((.*?)\)/; #}
my $a=$1; my $b=$2;#b is the list of args
($junk, $a)=split(' ', $a) if (/ /);
#$a =~/(^.*?)(\w+)\s+(\w+)$/;
#$a=join('', $1, $3);

#print "$a 444 $b 555";

$b =~ s/\s*,\s*/,/g;
my @args = split(',', $b);

for (my $i=0; $i<=$#args; $i++) {
  @anarray=split(' ', $args[$i]);
  $args[$i]=$anarray[$#anarray];
#   ($junk, $args[$i])=split(' ', $args[$i]);
  $args[$i] =~ s/\&//g;
}

$b=join(', ', @args);

print "$a\($b\);";
