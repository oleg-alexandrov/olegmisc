#!/usr/bin/perl
use strict;                # 'strict' insists that all variables be declared
use diagnostics;           # 'diagnostics' expands the cryptic warnings                  


undef $/;

my $contents = <>;
$contents =~ s/\s+/ /g;

$contents =~ /(^.*?\()/;
my $spaces = " " x length($1);
my $maxlength=80-length($1);

$contents =~ /(^.*?),(.*?$)/;
my $body=$1;
my $args=$2;

$args =~ s/^\s*//g;
$args =~ s/\s*,\s*/,/g;

my @args = split(",", $args);




$contents=$body;
my $line="";
my $pad;

foreach (@args) {
  if (length($line) + length($_) >= $maxlength) {
    $contents = "$contents" . "$line";
    $pad = $maxlength - length($line); # calculate the padding length
    $contents = "$contents, \n$spaces";
    $line = $_;
  }else {
    $line = "$line, $_";
  }
}
		     

$contents = "$contents" . "$line";
print "$contents";
