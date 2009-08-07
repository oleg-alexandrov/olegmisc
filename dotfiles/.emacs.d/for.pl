#!/usr/bin/perl
use strict;                            # 'strict' insists that all variables be declared
use diagnostics;                       # 'diagnostics' expands the cryptic warnings

undef $/;
$_=<>;
/for \(\s*(.*?)\s*;/;
my $m = $1;
my $n;

if ($m =~ /(\w+)\s+(\w+)$/) {
  $m=$1." "; $n=$2;
}else {
  $n=$m; $m="";
}
print "for \($m$n= ; $n < ; $n\+\+\)\{";
